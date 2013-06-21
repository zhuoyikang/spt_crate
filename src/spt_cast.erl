%%% ==================================================================
%%% @doc 本模块完成向指定进程组广播信息的功能
%%% @end
%%% ==================================================================

-module(spt_cast).
-behaviour(gen_server).

-record(state, {key, tableid}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1, join/2, quit/2, send/2, stop/1, test/1, cast/2]).

%%% -------------------------------------------------------------------
%%% 启动函数.
%%% -------------------------------------------------------------------

start_link(Caster) ->
  Atom = list_to_atom(atom_to_list(Caster) ++ "_cast"),
  gen_server:start_link({local, Atom}, ?MODULE, [Atom], []).

%%% --------------------------------------------------------------------
%%% 外部调用函数
%%% --------------------------------------------------------------------

%% 停止函数
stop(Caster) ->
  RegName = list_to_atom(atom_to_list(Caster) ++ "_cast"),
  gen_server:cast(RegName, stop).

%% 加入指定广播进程组
join(Caster, PID) ->
  RegName = list_to_atom(atom_to_list(Caster) ++ "_cast"),
  gen_server:call(RegName, {join, PID}).

%% 推出指定广播进程组
quit(Caster, PID) ->
  RegName = list_to_atom(atom_to_list(Caster) ++ "_cast"),
  gen_server:call(RegName, {quit, PID}).

%% 向指定广播进程组信息
send(Caster, Msg) ->
  RegName = list_to_atom(atom_to_list(Caster) ++ "_cast"),
  gen_server:call(RegName, {send, Msg}).

%% 向指定广播进程组信息
cast(Caster, PID) ->
  RegName = list_to_atom(atom_to_list(Caster) ++ "_cast"),
  gen_server:cast(RegName, {send, PID}).

%%% ---------------------------------------------------------------------
%%% 回调函数
%%% ---------------------------------------------------------------------

init(Atom) ->
  process_flag(trap_exit, true),
  TableId = ets:new(cast,[set, protected, {keypos, 1}]),
  {ok, #state{key = Atom, tableid = TableId}}.

%% stop函数调用
handle_cast(stop, State) ->
	{stop, normal, State};

%% 向已经加入的进程广播消息
handle_cast({send, Msg}, State = #state{tableid = TableId}) ->
  cast_all(TableId, Msg),
  {noreply, State};

%% handle_cast其他情况
handle_cast(_Info, State) ->
  {noreply, State}.

%% 接收推出信号
handle_info({'EXIT', _Pid, _Reason}, State) ->
  {noreply, State}.

%% 进程加入广播列表
handle_call({join, PID}, _From, State = #state{tableid=TableId}) ->
  ets:insert(TableId, {PID}),
  {reply, ok, State};

%% 删除已加入进程
handle_call({quit, PID}, _From, State = #state{tableid=TableId}) ->
  ets:delete(TableId, PID),
  {reply, ok, State};

%% 向已经加入的进程广播消息
handle_call({send, Msg}, _From, State = #state{tableid=TableId}) ->
  cast_all(TableId, Msg),
  {reply, ok, State};

%% 同步请求，直接返回
handle_call(_Request, _From, State) ->
  {noreply, State}.

%%
terminate(_Reason, _State = #state{tableid = _TableId}) ->
  %% ets:delete(_TableId),
  ok.

%%
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% ---------------------------------------------------------------------
%%% 回调函数
%%% ---------------------------------------------------------------------

cast_all(TableId, Msg) ->
  case ets:first(TableId) of
    '$end_of_table' -> [];
    Next -> cast_all(TableId, Next, Msg)
	end.
%% 向ets中的每个元素PID，发送Msg
cast_all(_TableID, '$end_of_table', _Msg) ->
  ok;
cast_all(TableID, Key, Msg) ->
  Key ! Msg,
  cast_all(TableID, ets:next(TableID, Key), Msg).

%% 接收测试信息
loop() -> receive Msg -> io:format("msg:~p~n", [Msg]), loop() end.

%% 测试函数
test(Cast) ->
	{ok, _Pid} = spt_cast_sup:start_caster(Cast),
  Pid1 = spawn(fun loop/0),
  Pid2 = spawn(fun loop/0),
  Pid3 = spawn(fun loop/0),
  io:format("three msg below ~n"),
  spt_cast:join(Cast, Pid1),
  spt_cast:join(Cast, Pid2),
  spt_cast:join(Cast, Pid3),
  spt_cast:send(Cast, {msg1}),
  io:format("three msg below ~n"),
  spt_cast:send(Cast, {msg2}),
  io:format("two msg below ~n"),
  spt_cast:quit(Cast, Pid2),
  spt_cast:send(Cast, {msg3}),
  spt_cast:quit(Cast, Pid1),
  io:format("one msg below ~n"),
  spt_cast:send(Cast, {msg4}),
  ok.
