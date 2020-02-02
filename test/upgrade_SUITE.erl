%% Copyright (c) 2020, Loïc Hoguin <essen@ninenines.eu>
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

-module(upgrade_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).

%% ct.

all() ->
	ct_helper:all(?MODULE).

init_per_suite(Config) ->
	%% Remove environment variables inherited from Erlang.mk.
	os:unsetenv("ERLANG_MK_TMP"),
	os:unsetenv("APPS_DIR"),
	os:unsetenv("DEPS_DIR"),
	os:unsetenv("ERL_LIBS"),
	os:unsetenv("CI_ERLANG_MK"),
	%% Ensure we are using the C locale for all os:cmd calls.
	os:putenv("LC_ALL", "C"),
	Config.

end_per_suite(_Config) ->
	ok.

%% Find GNU Make.

do_find_make_cmd() ->
	case os:getenv("MAKE") of
		false ->
			case os:find_executable("gmake") of
				false -> "make";
				Cmd   -> Cmd
			end;
		Cmd ->
			Cmd
	end.

%% Manipulate the release.

do_get_paths(Example0) ->
	Example = atom_to_list(Example0),
	{ok, CWD} = file:get_cwd(),
	Dir = CWD ++ "/../../examples/" ++ Example,
	Rel = Dir ++ "/_rel/" ++ Example ++ "_example/bin/" ++ Example ++ "_example",
	Log = Dir ++ "/_rel/" ++ Example ++ "_example/log/erlang.log.1",
	{Dir, Rel, Log}.

do_compile_and_start(Example, Config) ->
	Make = do_find_make_cmd(),
	{Dir, Rel, _} = do_get_paths(Example),
	ct:log("~s~n", [os:cmd(Make ++ " -C " ++ Dir ++ " distclean")]),
	%% TERM=dumb disables relx coloring.
	ct:log("~s~n", [os:cmd(Make ++ " -C " ++ Dir ++ " TERM=dumb")]),
	%% @todo Should move the "1" tarball in the correct directory to avoid a warning on downgrade?
	ct:log("~s~n", [os:cmd(Rel ++ " stop")]),
	ct:log("~s~n", [os:cmd(Rel ++ " start")]),
	timer:sleep(2000),
	ct:log("~s~n", [os:cmd(Rel ++ " eval 'application:info()'")]).

do_stop(Example) ->
	{Dir, Rel, Log} = do_get_paths(Example),
	ct:log("~s~n", [os:cmd("sed -i.bak s/\"2\"/\"1\"/ " ++ Dir ++ "/relx.config")]),
	ct:log("~s~n", [os:cmd(Rel ++ " stop")]),
	ct:log("~s~n", [element(2, file:read_file(Log))]).

%% When we are on a tag (git describe --exact-match succeeds),
%% we use the tag before that as a starting point. Otherwise
%% we use the most recent tag.
do_use_ranch_previous(Example) ->
	TagsOutput = os:cmd("git tag | tr - \\~ | sort -V | tr \\~ -"),
	ct:log("~s~n", [TagsOutput]),
	Tags = string:lexemes(TagsOutput, "\n"),
	DescribeOutput = os:cmd("git describe --exact-match"),
	ct:log("~s~n", [DescribeOutput]),
	{CommitOrTag, Prev} = case DescribeOutput of
		"fatal: no tag exactly matches " ++ _ -> {commit, hd(lists:reverse(Tags))};
		_ -> {tag, hd(tl(lists:reverse(Tags)))}
	end,
	do_use_ranch_commit(Example, Prev),
	CommitOrTag.

%% Replace the current Ranch commit with the one given as argument.
do_use_ranch_commit(Example, Commit) ->
	{Dir, _, _} = do_get_paths(Example),
	ct:log("~s~n", [os:cmd(
		"sed -i.bak s/\"dep_ranch_commit = .*\"/\"dep_ranch_commit = "
		++ Commit ++ "\"/ " ++ Dir ++ "/Makefile"
	)]).

%% Remove Ranch and rebuild, this time generating a relup.
do_build_relup(Example, CommitOrTag) ->
	Make = do_find_make_cmd(),
	{Dir, _, _} = do_get_paths(Example),
	ct:log("~s~n", [os:cmd("rm -rf " ++ Dir ++ "/deps/ranch")]),
	ct:log("~s~n", [os:cmd("sed -i.bak s/\"1\"/\"2\"/ " ++ Dir ++ "/relx.config")]),
	%% We need Ranch to be fetched first in order to copy the current appup
	%% and optionally update its version when we are not on a tag.
	ct:log("~s~n", [os:cmd(Make ++ " -C " ++ Dir ++ " deps")]),
	ct:log("~s~n", [os:cmd("cp " ++ Dir ++ "/../../ebin/ranch.appup "
		++ Dir ++ "/deps/ranch/ebin/")]),
	case CommitOrTag of
		tag -> ok;
		commit ->
			ProjectVersion = os:cmd("grep \"PROJECT_VERSION = \" " ++ Dir ++ "/deps/ranch/Makefile"),
			ct:log(ProjectVersion),
			["PROJECT_VERSION = " ++ Vsn0|_] = string:lexemes(ProjectVersion, "\n"),
			[A, B|Tail] = string:lexemes(Vsn0, "."),
			Vsn = binary_to_list(iolist_to_binary([A, $., B, ".9", lists:join($., Tail)])),
			ct:log("Changing Ranch version from ~s to ~s~n", [Vsn0, Vsn]),
			ct:log("~s~n", [os:cmd(
				"sed -i.bak s/\"PROJECT_VERSION = .*\"/\"PROJECT_VERSION = " ++ Vsn ++ "\"/ "
					++ Dir ++ "/deps/ranch/Makefile"
			)]),
			%% The version in the appup must be the same as PROJECT_VERSION.
			ct:log("~s~n", [os:cmd(
				"sed -i.bak s/\"" ++ Vsn0 ++ "\"/\"" ++ Vsn ++ "\"/ "
					++ Dir ++ "/deps/ranch/ebin/ranch.appup"
			)])
	end,
	ct:log("~s~n", [os:cmd(Make ++ " -C " ++ Dir ++ " relup")]).

%% Copy the tarball in the correct location and upgrade.
do_upgrade(Example) ->
	ExampleStr = atom_to_list(Example),
	{Dir, Rel, _} = do_get_paths(Example),
	ct:log("~s~n", [os:cmd("cp "
		++ Dir ++ "/_rel/" ++ ExampleStr
			++ "_example/" ++ ExampleStr ++ "_example-2.tar.gz "
		++ Dir ++ "/_rel/" ++ ExampleStr
			++ "_example/releases/2/" ++ ExampleStr ++ "_example.tar.gz")]),
	ct:log("~s~n", [os:cmd(Rel ++ " upgrade \"2\"")]),
	ct:log("~s~n", [os:cmd(Rel ++ " eval 'application:info()'")]).

do_downgrade(Example) ->
	{_, Rel, _} = do_get_paths(Example),
	ct:log("~s~n", [os:cmd(Rel ++ " downgrade \"1\"")]),
	ct:log("~s~n", [os:cmd(Rel ++ " eval 'application:info()'")]).

%% Tests.

upgrade_ranch_one_conn(Config) ->
	Example = tcp_echo,
	Port = 5555,
	try
		%% Build and start the example release using the previous Ranch version.
		CommitOrTag = do_use_ranch_previous(Example),
		do_compile_and_start(Example, Config),
		%% Establish a connection and check that it works.
		{ok, S} = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
		ok = gen_tcp:send(S, "Hello!"),
		{ok, <<"Hello!">>} = gen_tcp:recv(S, 0, 1000),
		%% Update Ranch to master then build a release upgrade.
		do_use_ranch_commit(Example, "master"),
		do_build_relup(Example, CommitOrTag),
		%% Perform the upgrade, then check that our connection is still up.
		do_upgrade(Example),
		ok = gen_tcp:send(S, "Hello!"),
		{ok, <<"Hello!">>} = gen_tcp:recv(S, 0, 1000),
		%% Check that new connections are still accepted.
		{ok, _} = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
		%% Perform the downgrade, then check that our connection is still up.
		do_downgrade(Example),
		ok = gen_tcp:send(S, "Hello!"),
		{ok, <<"Hello!">>} = gen_tcp:recv(S, 0, 1000),
		%% Check that new connections are still accepted.
		{ok, _} = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
		ok
	after
		do_stop(tcp_echo)
	end.

%% @todo upgrade_ranch_max_conn
