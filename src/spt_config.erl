%% 用于读取配置文件动态生成erlang模块
%% 假设配置文件的结构如下：
%% {database, "galaxy_empire"}.
%% {hostname, "localhost"}.
%% {port, 3306}.
%% {username, "root"}.
%% {password, ""}.
%%
-module(spt_config).
-export([gen/2]).

gen(ModuleName, Path) ->
  M1 = spt_smerl:new(ModuleName),
  M2 = parse(Path, fun({K, V} , M) ->
                       Fun = term_to_string(K) ++ "() ->" ++ term_to_string(V) ++ ".",
                       {ok, MM} = spt_smerl:add_func(M, Fun),
                       MM
                   end, M1),
 	spt_smerl:compile(M2),
	ok.

parse(Path, Fun, State) ->
  case file:open(Path, read) of
    {ok, File} ->
      NewState = do_parse_line(File, Fun, State),
      file:close(File),
      NewState;
    {error, Reason} ->
      io:format("server_config Reason ~p ~n", [Reason]),
      State
  end.

do_parse_line(File, Fun, State) ->
  case io:read(File, '') of
    eof -> ok, State;
    {error, Reason} -> io:format("server_config Reason ~p ~n", [Reason]);
    {ok, Term} ->
      NewState = Fun(Term, State),
      do_parse_line(File, Fun, NewState)
  end.

term_to_string(Term) ->
  R = io_lib:format("~p", [Term]),
  lists:flatten(R).

