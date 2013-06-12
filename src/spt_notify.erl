%% 记录所有注册的事件及分发.
%% 使用进程目的是为了托管表.
-module(spt_notify).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%% 初始化表格
init([]) ->
  ets:new(spt_notify_m, [named_table, public, bag]),
  {ok, spt_notify_m}.

sub(Event, Fun) ->
  ets:insert(spt_notify_m, {Event, Fun}).

ubsub(Event, Fun) ->
  ets:delete_object(spt_notify_m, {Event, Fun}).

post(Event, Param) ->
  L = ets:lookup(spt_notify_m, Event),
  [Fun(Param)  || {_, Fun} <- L].

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(_Reason, _State) ->
  ok.

all_test() ->
  start_link(),
  Fun1 = fun(X) ->  io:format("x1 ~p~n", [X]) end,
  Fun2 = fun(X) ->  io:format("x2 ~p~n", [X]) end,
  sub(e1,  Fun1),
  sub(e1, Fun2),
  post(e1, 23),
  ubsub(e1, Fun1),
  post(e1, 23),
  stop(),
  ok.
