%%% ==================================================================
%%% @author:ianliao
%%% @doc 管理一个进程列表，并向其中每个进程发送消息.
%%% @end
%%% ==================================================================

-module(spt_cast_sup).
-behaviour(supervisor).

%% API
-export([init/1, start_caster/1]).

-define(MAX_RESTART, 5000000).
-define(MAX_TIME, 60).

%% 开启一个连接服务进程.
start_caster(Cast) ->
  supervisor:start_child(?MODULE, [Cast]).

init([]) ->
  {ok,
   {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
    [{undefined, {spt_cast, start_link, []}, transient, 2000, worker, []}]
   }
  }.
