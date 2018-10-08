%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ranch_proxy_header).

-export([parse/1]).

-type proxy_info() :: #{
	%% Mandatory part.
	version := 1 | 2,
	command := local | proxy,
	transport_family => undefined | ipv4 | ipv6 | unix,
	transport_protocol => undefined | stream | dgram,
	%% Addresses.
	src_address => inet:ip_address() | binary(),
	src_port => inet:port_number(),
	dest_address => inet:ip_address() | binary(),
	dest_port => inet:port_number(),
	%% Extra TLV-encoded data.
	alpn => binary(), %% US-ASCII.
	authority => binary(), %% UTF-8.
	ssl => #{
		client := [ssl | cert_conn | cert_sess],
		verified := boolean(),
		version => binary(), %% US-ASCII.
		cipher => binary(), %% US-ASCII.
		sig_alg => binary(), %% US-ASCII.
		key_alg => binary(), %% US-ASCII.
		cn => binary() %% UTF-8.
	},
	netns => binary(), %% US-ASCII.
	%% Unknown TLVs can't be parsed so the raw data is given.
	raw_tlvs => [{0..255, binary()}]
}.
-export_type([proxy_info/0]).

-spec parse(Data) -> {ok, proxy_info(), Data} | {error, atom()} when Data::binary().
parse(<<"\r\n\r\n\0\r\nQUIT\n", Rest/bits>>) ->
	parse_v2(Rest);
parse(<<"PROXY ", Rest/bits>>) ->
	parse_v1(Rest);
parse(_) ->
	{error, 'The PROXY protocol header signature was not recognized. (PP 2.1, PP 2.2)'}.

-ifdef(TEST).
parse_unrecognized_header_test() ->
	{error, _} = parse(<<"GET / HTTP/1.1\r\n">>),
	ok.
-endif.

%% Human-readable header format (Version 1).
parse_v1(<<"TCP4 ", Rest/bits>>) ->
	parse_v1(Rest, ipv4);
parse_v1(<<"TCP6 ", Rest/bits>>) ->
	parse_v1(Rest, ipv6);
parse_v1(<<"UNKNOWN\r\n", Rest/bits>>) ->
	{ok, #{
		version => 1,
		command => proxy,
		transport_family => undefined,
		transport_protocol => undefined
	}, Rest};
parse_v1(<<"UNKNOWN ", Rest0/bits>>) ->
	case binary:split(Rest0, <<"\r\n">>) of
		[_, Rest] ->
			{ok, #{
				version => 1,
				command => proxy,
				transport_family => undefined,
				transport_protocol => undefined
			}, Rest};
		[_] ->
			{error, 'Malformed or incomplete PROXY protocol header line. (PP 2.1)'}
	end;
parse_v1(_) ->
	{error, 'The INET protocol and family string was not recognized. (PP 2.1)'}.

parse_v1(Rest0, Family) ->
	try
		{ok, SrcAddr, Rest1} = parse_ip(Rest0, Family),
		{ok, DestAddr, Rest2} = parse_ip(Rest1, Family),
		{ok, SrcPort, Rest3} = parse_port(Rest2, $\s),
		{ok, DestPort, Rest4} = parse_port(Rest3, $\r),
		<<"\n", Rest/bits>> = Rest4,
		{ok, #{
			version => 1,
			command => proxy,
			transport_family => Family,
			transport_protocol => stream,
			src_address => SrcAddr,
			src_port => SrcPort,
			dest_address => DestAddr,
			dest_port => DestPort
		}, Rest}
	catch
		throw:parse_ipv4_error ->
			{error, 'Failed to parse an IPv4 address in the PROXY protocol header line. (PP 2.1)'};
		throw:parse_ipv6_error ->
			{error, 'Failed to parse an IPv6 address in the PROXY protocol header line. (PP 2.1)'};
		throw:parse_port_error ->
			{error, 'Failed to parse a port number in the PROXY protocol header line. (PP 2.1)'};
		_:_ ->
			{error, 'Malformed or incomplete PROXY protocol header line. (PP 2.1)'}
	end.

parse_ip(<<Addr:7/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:8/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:9/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:10/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:11/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:12/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:13/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:14/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:15/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:39/binary, $\s, Rest/binary>>, ipv6) -> parse_ipv6(Addr, Rest).

parse_ipv4(Addr0, Rest) ->
	case inet:parse_ipv4strict_address(binary_to_list(Addr0)) of
		{ok, Addr} -> {ok, Addr, Rest};
		{error, einval} -> throw(parse_ipv4_error)
	end.

parse_ipv6(Addr0, Rest) ->
	case inet:parse_ipv6strict_address(binary_to_list(Addr0)) of
		{ok, Addr} -> {ok, Addr, Rest};
		{error, einval} -> throw(parse_ipv6_error)
	end.

parse_port(<<Port:1/binary, C, Rest/bits>>, C) -> parse_port(Port, Rest);
parse_port(<<Port:2/binary, C, Rest/bits>>, C) -> parse_port(Port, Rest);
parse_port(<<Port:3/binary, C, Rest/bits>>, C) -> parse_port(Port, Rest);
parse_port(<<Port:4/binary, C, Rest/bits>>, C) -> parse_port(Port, Rest);
parse_port(<<Port:5/binary, C, Rest/bits>>, C) -> parse_port(Port, Rest);

parse_port(Port0, Rest) ->
	try binary_to_integer(Port0) of
		Port when Port >= 0, Port =< 65535 ->
			{ok, Port, Rest};
		_ ->
			throw(parse_port_error)
	catch _:_ ->
		throw(parse_port_error)
	end.

-ifdef(TEST).
parse_v1_test() ->
	%% Examples taken from the PROXY protocol header specification.
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := ipv4,
		transport_protocol := stream,
		src_address := {255, 255, 255, 255},
		src_port := 65535,
		dest_address := {255, 255, 255, 255},
		dest_port := 65535
	}, <<>>} = parse(<<"PROXY TCP4 255.255.255.255 255.255.255.255 65535 65535\r\n">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := ipv6,
		transport_protocol := stream,
		src_address := {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535},
		src_port := 65535,
		dest_address := {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535},
		dest_port := 65535
	}, <<>>} = parse(<<"PROXY TCP6 "
		"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff "
		"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 65535 65535\r\n">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := undefined,
		transport_protocol := undefined
	}, <<>>} = parse(<<"PROXY UNKNOWN\r\n">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := undefined,
		transport_protocol := undefined
	}, <<>>} = parse(<<"PROXY UNKNOWN "
		"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff "
		"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 65535 65535\r\n">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := ipv4,
		transport_protocol := stream,
		src_address := {192, 168, 0, 1},
		src_port := 56324,
		dest_address := {192, 168, 0, 11},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r\nHost: 192.168.0.11\r\n\r\n">>} = parse(<<
		"PROXY TCP4 192.168.0.1 192.168.0.11 56324 443\r\n"
		"GET / HTTP/1.1\r\n"
		"Host: 192.168.0.11\r\n"
		"\r\n">>),
	%% Test cases taken from tomciopp/proxy_protocol.
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := ipv4,
		transport_protocol := stream,
		src_address := {192, 168, 0, 1},
		src_port := 56324,
		dest_address := {192, 168, 0, 11},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r">>} = parse(<<
		"PROXY TCP4 192.168.0.1 192.168.0.11 56324 443\r\nGET / HTTP/1.1\r">>),
	{error, _} = parse(<<"PROXY TCP4 192.1638.0.1 192.168.0.11 56324 443\r\nGET / HTTP/1.1\r">>),
	{error, _} = parse(<<"PROXY TCP4 192.168.0.1 192.168.0.11 1111111 443\r\nGET / HTTP/1.1\r">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := ipv6,
		transport_protocol := stream,
		src_address := {8193, 3512, 0, 66, 0, 35374, 880, 29492},
		src_port := 4124,
		dest_address := {8193, 3512, 0, 66, 0, 35374, 880, 29493},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r">>} = parse(<<"PROXY TCP6 "
		"2001:0db8:0000:0042:0000:8a2e:0370:7334 "
		"2001:0db8:0000:0042:0000:8a2e:0370:7335 4124 443\r\nGET / HTTP/1.1\r">>),
	{error, _} = parse(<<"PROXY TCP6 "
		"2001:0db8:0000:0042:0000:8a2e:0370:7334 "
		"2001:0db8:00;0:0042:0000:8a2e:0370:7335 4124 443\r\nGET / HTTP/1.1\r">>),
	{error, _} = parse(<<"PROXY TCP6 "
		"2001:0db8:0000:0042:0000:8a2e:0370:7334 "
		"2001:0db8:0000:0042:0000:8a2e:0370:7335 4124 foo\r\nGET / HTTP/1.1\r">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := undefined,
		transport_protocol := undefined
	}, <<"GET / HTTP/1.1\r">>} = parse(<<"PROXY UNKNOWN 4124 443\r\nGET / HTTP/1.1\r">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := undefined,
		transport_protocol := undefined
	}, <<"GET / HTTP/1.1\r">>} = parse(<<"PROXY UNKNOWN\r\nGET / HTTP/1.1\r">>),
	ok.
-endif.

%% Binary header format (version 2).

%% LOCAL.
parse_v2(<<2:4, 0:4, _:8, Len:16, Rest0/bits>>) ->
	case Rest0 of
		<<_:Len/binary, Rest/bits>> ->
			{ok, #{
				version => 2,
				command => local
			}, Rest};
		_ ->
			{error, 'Missing data in the PROXY protocol binary header. (PP 2.2)'}
	end;
%% PROXY.
parse_v2(<<2:4, 1:4, Family:4, Protocol:4, Len:16, Rest/bits>>)
		when Family =< 3, Protocol =< 2 ->
	case Rest of
		<<Header:Len/binary, _/bits>> ->
			parse_v2(Rest, Len, family(Family), protocol(Protocol),
				<<Family:4, Protocol:4, Len:16, Header:Len/binary>>);
		_ ->
			{error, 'Missing data in the PROXY protocol binary header. (PP 2.2)'}
	end;
%% Errors.
parse_v2(<<Version:4, _/bits>>) when Version =/= 2 ->
	{error, 'Invalid version in the PROXY protocol binary header. (PP 2.2)'};
parse_v2(<<_:4, Command:4, _/bits>>) when Command > 1 ->
	{error, 'Invalid command in the PROXY protocol binary header. (PP 2.2)'};
parse_v2(<<_:8, Family:4, _/bits>>) when Family > 3 ->
	{error, 'Invalid address family in the PROXY protocol binary header. (PP 2.2)'};
parse_v2(<<_:12, Protocol:4, _/bits>>) when Protocol > 2 ->
	{error, 'Invalid transport protocol in the PROXY protocol binary header. (PP 2.2)'}.

family(0) -> undefined;
family(1) -> ipv4;
family(2) -> ipv6;
family(3) -> unix.

protocol(0) -> undefined;
protocol(1) -> stream;
protocol(2) -> dgram.

parse_v2(Data, Len, Family, Protocol, _)
		when Family =:= undefined; Protocol =:= undefined ->
	<<_:Len/binary, Rest/bits>> = Data,
	{ok, #{
		version => 2,
		command => proxy,
		%% In case only one value was undefined, we set both explicitly.
		%% It doesn't make sense to have only one known value.
		transport_family => undefined,
		transport_protocol => undefined
	}, Rest};
parse_v2(<<
		S1, S2, S3, S4,
		D1, D2, D3, D4,
		SrcPort:16, DestPort:16, Rest/bits>>, Len, Family=ipv4, Protocol, Header)
		when Len >= 12 ->
	parse_tlv(Rest, Len - 12, #{
		version => 2,
		command => proxy,
		transport_family => Family,
		transport_protocol => Protocol,
		src_address => {S1, S2, S3, S4},
		src_port => SrcPort,
		dest_address => {D1, D2, D3, D4},
		dest_port => DestPort
	}, Header);
parse_v2(<<
		S1:16, S2:16, S3:16, S4:16, S5:16, S6:16, S7:16, S8:16,
		D1:16, D2:16, D3:16, D4:16, D5:16, D6:16, D7:16, D8:16,
		SrcPort:16, DestPort:16, Rest/bits>>, Len, Family=ipv6, Protocol, Header)
		when Len >= 36 ->
	parse_tlv(Rest, Len - 36, #{
		version => 2,
		command => proxy,
		transport_family => Family,
		transport_protocol => Protocol,
		src_address => {S1, S2, S3, S4, S5, S6, S7, S8},
		src_port => SrcPort,
		dest_address => {D1, D2, D3, D4, D5, D6, D7, D8},
		dest_port => DestPort
	}, Header);
parse_v2(<<SrcAddr0:108/binary, DestAddr0:108/binary, Rest/bits>>,
		Len, Family=unix, Protocol, Header)
		when Len >= 216 ->
	try
		[SrcAddr, _] = binary:split(SrcAddr0, <<0>>),
		true = byte_size(SrcAddr) > 0,
		[DestAddr, _] = binary:split(DestAddr0, <<0>>),
		true = byte_size(DestAddr) > 0,
		parse_tlv(Rest, Len - 216, #{
			version => 2,
			command => proxy,
			transport_family => Family,
			transport_protocol => Protocol,
			src_address => SrcAddr,
			dest_address => DestAddr
		}, Header)
	catch _:_ ->
		{error, 'Invalid UNIX address in PROXY protocol binary header. (PP 2.2)'}
	end;
parse_v2(_, _, _, _, _) ->
	{error, 'Invalid length in the PROXY protocol binary header. (PP 2.2)'}.

-ifdef(TEST).
parse_v2_test() ->
	%% Test cases taken from tomciopp/proxy_protocol.
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := ipv4,
		transport_protocol := stream,
		src_address := {127, 0, 0, 1},
		src_port := 444,
		dest_address := {192, 168, 0, 1},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10, %% Signature.
		33, %% Version and command.
		17, %% Family and protocol.
		0, 12, %% Length.
		127, 0, 0, 1, %% Source address.
		192, 168, 0, 1, %% Destination address.
		1, 188, %% Source port.
		1, 187, %% Destination port.
		"GET / HTTP/1.1\r\n">>),
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := ipv4,
		transport_protocol := dgram,
		src_address := {127, 0, 0, 1},
		src_port := 444,
		dest_address := {192, 168, 0, 1},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10, %% Signature.
		33, %% Version and command.
		18, %% Family and protocol.
		0, 12, %% Length.
		127, 0, 0, 1, %% Source address.
		192, 168, 0, 1, %% Destination address.
		1, 188, %% Source port.
		1, 187, %% Destination port.
		"GET / HTTP/1.1\r\n">>),
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := ipv6,
		transport_protocol := stream,
		src_address := {5532, 4240, 1, 0, 0, 0, 0, 0},
		src_port := 444,
		dest_address := {8193, 3512, 1, 0, 0, 0, 0, 0},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10, %% Signature.
		33, %% Version and command.
		33, %% Family and protocol.
		0, 36, %% Length.
		21, 156, 16, 144, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, %% Source address.
		32, 1, 13, 184, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, %% Destination address.
		1, 188, %% Source port.
		1, 187, %% Destination port.
		"GET / HTTP/1.1\r\n">>),
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := ipv6,
		transport_protocol := dgram,
		src_address := {5532, 4240, 1, 0, 0, 0, 0, 0},
		src_port := 444,
		dest_address := {8193, 3512, 1, 0, 0, 0, 0, 0},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10, %% Signature.
		33, %% Version and command.
		34, %% Family and protocol.
		0, 36, %% Length.
		21, 156, 16, 144, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, %% Source address.
		32, 1, 13, 184, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, %% Destination address.
		1, 188, %% Source port.
		1, 187, %% Destination port.
		"GET / HTTP/1.1\r\n">>),
	Path = <<"/var/pgsql_sock">>,
	Len = byte_size(Path),
	Padding = 8 * (108 - Len),
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := unix,
		transport_protocol := stream,
		src_address := Path,
		dest_address := Path
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10,
		33,
		49,
		0, 216,
		Path/binary, 0:Padding,
		Path/binary, 0:Padding,
		"GET / HTTP/1.1\r\n">>),
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := unix,
		transport_protocol := dgram,
		src_address := Path,
		dest_address := Path
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10,
		33,
		50,
		0, 216,
		Path/binary, 0:Padding,
		Path/binary, 0:Padding,
		"GET / HTTP/1.1\r\n">>),
	ok.
-endif.

parse_tlv(Rest, 0, Info, _) ->
	{ok, Info, Rest};
%% PP2_TYPE_ALPN.
parse_tlv(<<16#1, TLVLen:16, ALPN:TLVLen/binary, Rest/bits>>, Len, Info, Header) ->
	parse_tlv(Rest, Len - TLVLen - 3, Info#{alpn => ALPN}, Header);
%% PP2_TYPE_AUTHORITY.
parse_tlv(<<16#2, TLVLen:16, Authority:TLVLen/binary, Rest/bits>>, Len, Info, Header) ->
	parse_tlv(Rest, Len - TLVLen - 3, Info#{authority => Authority}, Header);
%% PP2_TYPE_CRC32C.
parse_tlv(<<16#3, TLVLen:16, CRC32C:32, Rest/bits>>, Len0, Info, Header) when TLVLen =:= 4 ->
	Len = Len0 - TLVLen - 3,
	BeforeLen = byte_size(Header) - Len - 7, %% 3 Family/Protocol/Len, 4 CRC32C
	<<Before:BeforeLen/binary, _:32, After:Len/binary>> = Header,
	%% The initial CRC is erlang:crc32(<<"\r\n\r\n\0\r\nQUIT\n", 2:4, 1:4>>).
	case erlang:crc32(1302506282, [Before, <<0:32>>, After]) of
		CRC32C ->
			parse_tlv(Rest, Len - TLVLen - 3, Info, Header);
		_ ->
			{error, 'Failed CRC32C verification in PROXY protocol binary header. (PP 2.2)'}
	end;
%% PP2_TYPE_NOOP.
parse_tlv(<<16#4, TLVLen:16, _:TLVLen/binary, Rest/bits>>, Len, Info, Header) ->
	parse_tlv(Rest, Len - TLVLen - 3, Info, Header);
%% PP2_TYPE_SSL.
parse_tlv(<<16#20, TLVLen:16, Client, Verify:32, Rest0/bits>>, Len, Info, Header) ->
	SubsLen = TLVLen - 5,
	case Rest0 of
		<<Subs:SubsLen/binary, Rest/bits>> ->
			SSL0 = #{
				client => client(<<Client>>),
				verified => Verify =:= 0
			},
			case parse_ssl_tlv(Subs, SubsLen, SSL0) of
				{ok, SSL, <<>>} ->
					parse_tlv(Rest, Len - TLVLen - 3, Info#{ssl => SSL}, Header);
				Error={error, _} ->
					Error
			end;
		_ ->
			{error, 'Invalid TLV length in the PROXY protocol binary header. (PP 2.2)'}
	end;
%% PP2_TYPE_NETNS.
parse_tlv(<<16#30, TLVLen:16, NetNS:TLVLen/binary, Rest/bits>>, Len, Info, Header) ->
	parse_tlv(Rest, Len - TLVLen - 3, Info#{netns => NetNS}, Header);
%% Unknown TLV.
parse_tlv(<<TLVType, TLVLen:16, TLVValue:TLVLen/binary, Rest/bits>>, Len, Info, Header) ->
	RawTLVs = maps:get(raw_tlvs, Info, []),
	parse_tlv(Rest, Len - TLVLen - 3, Info#{raw_tlvs => [{TLVType, TLVValue}|RawTLVs]}, Header);
%% Invalid TLV length.
parse_tlv(_, _, _, _) ->
	{error, 'Invalid TLV length in the PROXY protocol binary header. (PP 2.2)'}.

client(<<_:5, ClientCertSess:1, ClientCertConn:1, ClientSSL:1>>) ->
	Client0 = case ClientCertSess of
		0 -> [];
		1 -> [cert_sess]
	end,
	Client1 = case ClientCertConn of
		0 -> Client0;
		1 -> [cert_conn|Client0]
	end,
	case ClientSSL of
		0 -> Client1;
		1 -> [ssl|Client1]
	end.

parse_ssl_tlv(Rest, 0, Info) ->
	{ok, Info, Rest};
%% Valid TLVs.
parse_ssl_tlv(<<TLVType, TLVLen:16, TLVValue:TLVLen/binary, Rest/bits>>, Len, Info) ->
	case ssl_subtype(TLVType) of
		undefined ->
			{error, 'Invalid TLV subtype for PP2_TYPE_SSL in PROXY protocol binary header. (PP 2.2)'};
		Type ->
			parse_ssl_tlv(Rest, Len - TLVLen - 3, Info#{Type => TLVValue})
	end;
%% Invalid TLV length.
parse_ssl_tlv(_, _, _) ->
	{error, 'Invalid TLV length in the PROXY protocol binary header. (PP 2.2)'}.

ssl_subtype(16#21) -> version;
ssl_subtype(16#22) -> cn;
ssl_subtype(16#23) -> cipher;
ssl_subtype(16#24) -> sig_alg;
ssl_subtype(16#25) -> key_alg;
ssl_subtype(_) -> undefined.
