%%% ==================================================================
%%% @doc 本模块完成向指定进程组广播信息的功能
%%% @end
%%% ==================================================================

-module(spt_cast).
-behaviour(gen_server).

-record(state, {registername, tableid}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-export([start_link/1, join/2, quit/2, send/2, sendSync/2, 
   stop/1, test/1]).

%%% -------------------------------------------------------------------
%%% 启动函数.
%%% -------------------------------------------------------------------

start_link(Cast) ->
  Atom = list_to_atom(atom_to_list(Cast) ++ "_cast"),	
  gen_server:start_link({local, Atom}, ?MODULE, [Atom], []).

%%% --------------------------------------------------------------------
%%% 外部调用函数
%%% --------------------------------------------------------------------

%% 停止函数
stop(PID) ->
  RegName = list_to_atom(atom_to_list(PID) ++ "_cast"),
  gen_server:cast(RegName, stop).

%% 加入指定广播进程组
join(PID, PIDMSG) ->
  RegName = list_to_atom(atom_to_list(PID) ++ "_cast"),
  gen_server:cast(RegName, {insert, PIDMSG}).

%% 推出指定广播进程组
quit(PID, PIDMSG) ->
  RegName = list_to_atom(atom_to_list(PID) ++ "_cast"),	
  gen_server:cast(RegName, {delete, PIDMSG}).

%% 向指定广播进程组信息
send(PID, PIDMSG) ->
  RegName = list_to_atom(atom_to_list(PID) ++ "_cast"),
  gen_server:cast(RegName, {send, PIDMSG}).

%% 空函数定义
sendSync(PID, PIDMSG) ->
  RegName = list_to_atom(atom_to_list(PID) ++ "_cast"),  
  gen_server:call(RegName, {send, PIDMSG}).

%%% ---------------------------------------------------------------------
%%% 回调函数
%%% ---------------------------------------------------------------------

init(Atom) ->
  io:format("~p fork", [self()]),
  process_flag(trap_exit, true),
  TableId = ets:new(cast,[set, protected, {keypos, 1}]),
  {ok, #state{registername = Atom, tableid = TableId}}.

%% stop函数调用 
handle_cast(stop, State) ->
	{stop, normal, State};

%% 进程加入广播列表
handle_cast({insert, PIDMSG}, State = #state{tableid = TableId}) ->
  ets:insert(TableId, {PIDMSG}), 
  {noreply, State};

%% 删除已加入进程
handle_cast({delete, PIDMSG}, State = #state{tableid = TableId}) ->
  ets:delete(TableId, {PIDMSG}),
  {noreply, State};

%% 向已经加入的进程广播消息
handle_cast({send, PIDMSG}, State = #state{tableid = TableId}) ->
  case ets:first(TableId) of
    '$end_of_table' -> [];
    Next -> castMsgToAll(TableId, Next, PIDMSG)
	end,
  {noreply, State};

%% handle_cast其他情况 
handle_cast(Info, State) ->
  io:format("handle_cast ~p ~p", [Info, State#state.registername]),
  {noreply, State}.

%% 接收推出信号
handle_info({'EXIT', Pid, Reason}, State) ->
  io:format("~p ~p ~p", [Pid, Reason, State#state.registername]),
  {noreply, State}.

%% 同步请求，直接返回
handle_call(_Request, From, State) ->
  {noreply, From, State}.	

%% 
terminate(Reason, _State = #state{tableid = _TableId}) ->
  io:format("spt_cast_id:~p terminate reason ~p", [self(), Reason]),
  %% ets:delete(_TableId),
  ok.

%% 
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
	
%%% ---------------------------------------------------------------------
%%% 回调函数
%%% ---------------------------------------------------------------------

%% 向ets中的每个元素PID，发送Msg
castMsgToAll(_TableID, '$end_of_table', _Msg) ->
  [];
castMsgToAll(TableID, Key, Msg) ->
  Key ! Msg,
  castMsgToAll(TableID, ets:next(TableID, Key), Msg).

%% 接收测试信息
loop() ->
  receive
		stop ->
		  {stop};
	  {test,Msg} ->
		  io:format("the message you receive is ~p ~n", [Msg]),
		  loop();
		_Other -> 
		  io:format("no result! ~n"),
			loop()
	end.
		
%% 测试函数
test(Cast) ->
	{ok, _Pid} = spt_cast_sup:start_caster(Cast),
	 Pid1=spawn(spt_test, loop, []),
	 Pid2=spawn(spt_test, loop, []),
	 Pid3=spawn(spt_test, loop, []),
	 spt_cast:join(Cast, Pid1),
	 spt_cast:join(Cast, Pid2),
	 spt_cast:join(Cast, Pid3),
	 ok.	
