%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
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

-module(proxy_header_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).

%% ct.

all() ->
	ct_helper:all(?MODULE).

%% Tests.

recv_v1_proxy_header_tcp(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 1,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v1_proxy_header_tcp_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 1,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

recv_v2_proxy_header_tcp(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v2_proxy_header_tcp_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

recv_v2_local_header_tcp(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 2,
		command => local
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v2_local_header_tcp_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 2,
		command => local
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

do_proxy_header_tcp(Name, ProxyInfo, Data1, Data2) ->
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		proxy_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [ranch_proxy_header:header(ProxyInfo), Data1]),
	receive
		{proxy_protocol, ProxyInfo} ->
			ok
	after 2000 ->
		error(timeout)
	end,
	ok = gen_tcp:send(Socket, Data2),
	Len1 = byte_size(Data1),
	Len2 = byte_size(Data2),
	{ok, <<Data1:Len1/binary, Data2/binary>>} = gen_tcp:recv(Socket, Len1 + Len2, 1000),
	ok = ranch:stop_listener(Name),
	ok.

recv_v1_proxy_header_ssl(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 1,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v1_proxy_header_ssl_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 1,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

recv_v2_proxy_header_ssl(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v2_proxy_header_ssl_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

recv_v2_local_header_ssl(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 2,
		command => local
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v2_local_header_ssl_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = ?FUNCTION_NAME,
	ProxyInfo = #{
		version => 2,
		command => local
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

do_proxy_header_ssl(Name, ProxyInfo, Data1, Data2) ->
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts ++ [{verify, verify_none}],
		proxy_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket0} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket0, [ranch_proxy_header:header(ProxyInfo)]),
	{ok, Socket} = ssl:connect(Socket0, [{verify, verify_none}, {versions, ['tlsv1.2']}], 1000),
	ok = ssl:send(Socket, Data1),
	receive
		{proxy_protocol, ProxyInfo} ->
			ok
	after 2000 ->
		error(timeout)
	end,
	ok = ssl:send(Socket, Data2),
	Len1 = byte_size(Data1),
	Len2 = byte_size(Data2),
	{ok, <<Data1:Len1/binary, Data2/binary>>} = ssl:recv(Socket, Len1 + Len2, 1000),
	ok = ranch:stop_listener(Name),
	ok.

recv_v2_split(_) ->
	doc("A version 2 header larger than the minimum TCP payload "
		"is reassembled and the following bytes stay buffered."),
	%% 536 bytes is the smallest TCP payload: a 576-byte IPv4 MTU
	%% minus 40 bytes of IP and TCP headers.
	Cert = binary:copy(<<$a>>, 600),
	ProxyInfo = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443,
		ssl => #{
			client => [ssl, cert_conn],
			verified => true,
			client_cert => Cert
		}
	},
	Header = iolist_to_binary(ranch_proxy_header:header(ProxyInfo)),
	<<Part1:536/binary, Part2/binary>> = Header,
	{ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
	{ok, Port} = inet:port(Listen),
	{ok, Client} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
	{ok, Server} = gen_tcp:accept(Listen, 1000),
	ok = gen_tcp:close(Listen),
	ok = gen_tcp:send(Client, Part1),
	_ = spawn(fun () ->
		timer:sleep(200),
		ok = gen_tcp:send(Client, [Part2, <<"NEXT">>])
	end),
	{ok, ProxyInfo} = ranch_tcp:recv_proxy_header(Server, 1000),
	{ok, <<"NEXT">>} = gen_tcp:recv(Server, 4, 1000),
	ok = gen_tcp:close(Client),
	ok = gen_tcp:close(Server).

trusted_proxy_exact(_) ->
	doc("An exact host prefix matches the immediate peer, not the header source."),
	Name = ?FUNCTION_NAME,
	Header = ranch_proxy_header:header(claimed_proxy_info()),
	{Peer, Result} = do_proxy_result(Name,
		#{trusted_proxies => [{{127, 0, 0, 1}, 32}]},
		fun do_connect_ipv4/1, Header),
	{{127, 0, 0, 1}, _} = Peer,
	{ok, Info} = Result,
	Info = claimed_proxy_info(),
	ok.

trusted_proxy_cidr(_) ->
	doc("A prefix shorter than the host matches the immediate peer."),
	Name = ?FUNCTION_NAME,
	Header = ranch_proxy_header:header(claimed_proxy_info()),
	{_, {ok, Info}} = do_proxy_result(Name,
		#{trusted_proxies => [{{127, 0, 0, 0}, 8}]},
		fun do_connect_ipv4/1, Header),
	Info = claimed_proxy_info(),
	{_, {ok, Info}} = do_proxy_result(Name,
		#{trusted_proxies => [{{0, 0, 0, 0}, 1}]},
		fun do_connect_ipv4/1, Header),
	{_, {error, protocol_error, _}} = do_proxy_result(Name,
			#{trusted_proxies => [{{128, 0, 0, 0}, 1}]},
			fun do_connect_ipv4/1, Header),
	ok.

trusted_proxy_miss(_) ->
	doc("A peer outside the list is rejected even when the header source is inside it."),
	Name = ?FUNCTION_NAME,
	Info = (claimed_proxy_info())#{src_address => {10, 1, 2, 3}},
	{Peer, Result} = do_proxy_result(Name,
		#{trusted_proxies => [{{10, 0, 0, 0}, 8}]},
		fun do_connect_ipv4/1,
		ranch_proxy_header:header(Info)),
	{{127, 0, 0, 1}, _} = Peer,
	{error, protocol_error, _} = Result,
	ok.

trusted_proxy_empty(_) ->
	doc("An empty list rejects every header."),
	Name = ?FUNCTION_NAME,
	Header = ranch_proxy_header:header(claimed_proxy_info()),
	{_, Result} = do_proxy_result(Name,
		#{trusted_proxies => []},
		fun do_connect_ipv4/1, Header),
	{error, protocol_error, _} = Result,
	ok.

trusted_proxy_unknown(_) ->
	doc("The allowlist is checked when the header family is unknown."),
	Name = ?FUNCTION_NAME,
	Header = <<"PROXY UNKNOWN\r\n">>,
	Unknown = #{
		version => 1,
		command => proxy,
		transport_family => undefined,
		transport_protocol => undefined
	},
	{_, {ok, Unknown}} = do_proxy_result(Name,
		#{trusted_proxies => [{{127, 0, 0, 1}, 32}]},
		fun do_connect_ipv4/1, Header),
	{_, {error, protocol_error, _}} = do_proxy_result(Name,
			#{trusted_proxies => [{{10, 0, 0, 0}, 8}]},
			fun do_connect_ipv4/1, Header),
	ok.

trusted_proxy_ipv6(_) ->
	doc("A native IPv6 peer matches IPv6 entries only."),
	case gen_tcp:listen(0, [inet6, {ip, {0, 0, 0, 0, 0, 0, 0, 1}}]) of
		{ok, LSock} ->
			ok = gen_tcp:close(LSock),
			do_trusted_proxy_ipv6();
		{error, _} ->
			{skip, "IPv6 listen is not available."}
	end.

do_trusted_proxy_ipv6() ->
	Name = ?FUNCTION_NAME,
	Header = ranch_proxy_header:header(claimed_proxy_info()),
	SockOpts = [inet6, {ip, {0, 0, 0, 0, 0, 0, 0, 1}}],
	{Peer, {ok, Info}} = do_proxy_result(Name,
		#{socket_opts => SockOpts,
			trusted_proxies => [{{0, 0, 0, 0, 0, 0, 0, 1}, 128}]},
		fun do_connect_ipv6/1, Header),
	{{0, 0, 0, 0, 0, 0, 0, 1}, _} = Peer,
	Info = claimed_proxy_info(),
	{_, {ok, Info}} = do_proxy_result(Name,
		#{socket_opts => SockOpts,
			trusted_proxies => [{{0, 0, 0, 0, 0, 0, 0, 0}, 0}]},
		fun do_connect_ipv6/1, Header),
	{{{0, 0, 0, 0, 0, 0, 0, 1}, _}, {error, protocol_error, _}}
		= do_proxy_result(Name,
			#{socket_opts => SockOpts,
				trusted_proxies => [{{0, 0, 0, 0}, 0}]},
			fun do_connect_ipv6/1, Header),
	ok.

trusted_proxy_mapped(_) ->
	doc("An IPv4-mapped peer matches IPv4 entries and not an IPv6 prefix."),
	case gen_tcp:listen(0, [inet6, {ip, {0, 0, 0, 0, 0, 0, 0, 0}},
			{ipv6_v6only, false}]) of
		{ok, LSock} ->
			ok = gen_tcp:close(LSock),
			do_trusted_proxy_mapped();
		{error, _} ->
			{skip, "Dual-stack listen is not available."}
	end.

do_trusted_proxy_mapped() ->
	Name = ?FUNCTION_NAME,
	Header = ranch_proxy_header:header(claimed_proxy_info()),
	SockOpts = [inet6, {ip, {0, 0, 0, 0, 0, 0, 0, 0}}, {ipv6_v6only, false}],
	{Peer, {ok, Info}} = do_proxy_result(Name,
		#{socket_opts => SockOpts,
			trusted_proxies => [{{127, 0, 0, 1}, 32}]},
		fun do_connect_ipv4/1, Header),
	{{0, 0, 0, 0, 0, 16#ffff, 16#7f00, 1}, _} = Peer,
	Info = claimed_proxy_info(),
	{{{0, 0, 0, 0, 0, 16#ffff, 16#7f00, 1}, _}, {error, protocol_error, _}}
		= do_proxy_result(Name,
			#{socket_opts => SockOpts,
				trusted_proxies => [{{0, 0, 0, 0, 0, 0, 0, 0}, 0}]},
			fun do_connect_ipv4/1, Header),
	{{{0, 0, 0, 0, 0, 16#ffff, 16#7f00, 1}, _}, {error, protocol_error, _}}
		= do_proxy_result(Name,
			#{socket_opts => SockOpts,
				trusted_proxies => [{{0, 0, 0, 0, 0, 0, 0, 1}, 128}]},
			fun do_connect_ipv4/1, Header),
	ok.

trusted_proxy_local(_) ->
	doc("A local socket peer matches no CIDR entry."),
	case os:type() of
		{unix, _} ->
			do_trusted_proxy_local();
		_ ->
			{skip, "No local socket support."}
	end.

do_trusted_proxy_local() ->
	Name = ?FUNCTION_NAME,
	SockFile = list_to_binary(lists:droplast(os:cmd("mktemp -u"))),
	try
		{ok, _} = ranch:start_listener(Name,
			ranch_tcp, #{
				socket_opts => [{ip, {local, SockFile}}],
				trusted_proxies => [{{127, 0, 0, 1}, 32}]
			},
			proxy_header_result, #{pid => self()}),
		{ok, Socket} = gen_tcp:connect({local, SockFile}, 0,
			[binary, {active, false}, {packet, raw}]),
		ok = gen_tcp:send(Socket, ranch_proxy_header:header(claimed_proxy_info())),
		receive
			{result, {local, _}, {error, protocol_error, _}} ->
				ok
		after 2000 ->
			error(timeout)
		end
	after
		ranch:stop_listener(Name),
		file:delete(SockFile)
	end.

trusted_proxy_ssl(_) ->
	doc("The allowlist uses the TCP peer of an accepted TLS socket."),
	Name = ?FUNCTION_NAME,
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, #{
			socket_opts => Opts ++ [{verify, verify_none}],
			trusted_proxies => [{{127, 0, 0, 1}, 32}]
		},
		proxy_header_result, #{pid => self()}),
	try
		Port = ranch:get_port(Name),
		{ok, Socket0} = gen_tcp:connect({127, 0, 0, 1}, Port,
			[binary, {active, false}, {packet, raw}]),
		ok = gen_tcp:send(Socket0, ranch_proxy_header:header(claimed_proxy_info())),
		receive
			{result, {{127, 0, 0, 1}, _}, {ok, Info}} ->
				Info = claimed_proxy_info()
		after 2000 ->
			error(timeout)
		end,
		{ok, Socket} = ssl:connect(Socket0, [{verify, verify_none},
			{versions, ['tlsv1.2']}], 5000),
		ok = ssl:close(Socket)
	after
		ranch:stop_listener(Name)
	end.

trusted_proxy_bad_option(_) ->
	doc("Reject trusted_proxies values that are not CIDR tuples."),
	Name = ?FUNCTION_NAME,
	Bad = [
		not_a_list,
		[{127, 0, 0, 1}],
		["10.0.0.0/8"],
		[<<"10.0.0.0/8">>],
		[{{127, 0, 0, 1}, 33}],
		[{{127, 0, 0, 1}, -1}],
		[{{127, 0, 0, 1}, 32.0}],
		[{{127, 0, 0, 1.0}, 32}],
		[{{0, 0, 0, 0, 0, 0, 0, 1}, 129}],
		[{{0, 0, 0, 0, 0, 0, 0, 16#10000}, 128}],
		[{{0, 0, 0, 0, 0, 16#ffff, 16#7f00, 1}, 0}],
		[{{0, 0, 0, 0, 0, 16#ffff, 16#7f00, 1}, 128}],
		[{{127, 0, 0, 1}, 32} | bad],
		[{{127, 0, 0, 1}, 32, extra}]
	],
	_ = [{error, {bad_option, trusted_proxies}} = ranch:start_listener(Name,
		ranch_tcp, #{trusted_proxies => Value},
		proxy_header_result, #{pid => self()})
		|| Value <- Bad],
	ok.

claimed_proxy_info() ->
	#{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {1, 2, 3, 4},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	}.

do_proxy_result(Name, TransOpts, Connect, Header) ->
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, TransOpts,
		proxy_header_result, #{pid => self()}),
	try
		Port = ranch:get_port(Name),
		{ok, Socket} = Connect(Port),
		ok = gen_tcp:send(Socket, Header),
		receive
			{result, Peer, Result} ->
				{Peer, Result}
		after 2000 ->
			error(timeout)
		end
	after
		ranch:stop_listener(Name)
	end.

do_connect_ipv4(Port) ->
	gen_tcp:connect({127, 0, 0, 1}, Port,
		[inet, binary, {active, false}, {packet, raw}]).

do_connect_ipv6(Port) ->
	gen_tcp:connect({0, 0, 0, 0, 0, 0, 0, 1}, Port,
		[inet6, binary, {active, false}, {packet, raw}]).
