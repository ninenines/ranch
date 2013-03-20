%% Copyright (c) 2012, Loïc Hoguin <essen@ninenines.eu>
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

%% @private
-module(ranch_transport).

%% Behaviour API
-export([behaviour_info/1]).
-export_type([socket/0, opts/0]).

%% @doc Behaviour information callback.
behaviour_info(callbacks) ->
    [{name,0},                 %% Name of the transport
     {messages,0},             %% Atoms used to identify messages in {active, once | true} mode.
     {listen,1},               %% Listen for connections on the given port number.
     {accept,2},               %% Accept connections with the given listening socket.
     {recv, 3},                %% Receive data from a socket in passive mode.
     {send, 2},                %% Send data on a socket.
     {setopts, 2},             %% Set options on the given socket.
     {controlling_process, 2}, %% Give control of the socket to a new process.
     {peername, 1},            %% Return the remote address and port of the connection.
     {sockname, 1},            %% Return the local address and port of the connection.
     {close, 1}];              %% Close the given socket
behaviour_info(_) ->
    undefined.

-type socket() :: any().
-type opts() :: any().


%% Name of the transport.
%%-callback name() -> atom().


%% Atoms used to identify messages in {active, once | true} mode.
%%-callback messages() -> {OK::atom(), Closed::atom(), Error::atom()}.

%% Listen for connections on the given port number.
%%
%% Calling this function returns a listening socket that can then
%% be passed to accept/2 to accept connections.
%%
%% Available options may vary between transports.
%%
%% You can listen to a random port by setting the port option to 0.
%% It is then possible to retrieve this port number by calling
%% sockname/1 on the listening socket. If you are using Ranch's
%% listener API, then this port number can obtained through
%% ranch:get_port/1 instead.
%%-callback listen(opts()) -> {ok, socket()} | {error, atom()}.

%% Accept connections with the given listening socket.
%%-callback accept(socket(), timeout())
%%	-> {ok, socket()} | {error, closed | timeout | atom() | tuple()}.

%% Experimental. Open a connection to the given host and port number.
%%-callback connect(string(), inet:port_number(), opts())
%%	-> {ok, socket()} | {error, atom()}.

%% Receive data from a socket in passive mode.
%%-callback recv(socket(), non_neg_integer(), timeout())
%%	-> {ok, any()} | {error, closed | timeout | atom()}.

%% Send data on a socket.
%%-callback send(socket(), iodata()) -> ok | {error, atom()}.

%% Set options on the given socket.
%%-callback setopts(socket(), opts()) -> ok | {error, atom()}.

%% Give control of the socket to a new process.
%%
%% Must be called from the process currently controlling the socket,
%% otherwise an {error, not_owner} tuple will be returned.
%%-callback controlling_process(socket(), pid())
%%	-> ok | {error, closed | not_owner | atom()}.

%% Return the remote address and port of the connection.
%%-callback peername(socket())
%%	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.

%% Return the local address and port of the connection.
%%-callback sockname(socket())
%%	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.

%% Close the given socket.
%%-callback close(socket()) -> ok.
