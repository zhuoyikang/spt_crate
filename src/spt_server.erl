%%%-------------------------------------------------------------------
%%% @author zhuoyikang <>
%%% @copyright (C) 2016, zhuoyikang
%%% @doc
%%% 上一次收到消息的时间，如果超过Quit时间没有消息则退出.
%%% @end
%%% Created : 14 Jan 2016 by zhuoyikang <>
%%%-------------------------------------------------------------------

-module(spt_server).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {model,raw_state,status,last,quit}).


handle_ret({inited,SaveTime,QuitTime,RawState},State) ->
    timer:send_interval(SaveTime*1000, save),
    State1 = State#state{raw_state=RawState,status=1, quit=QuitTime},
    {noreply,State1};

handle_ret({inited,QuitTime,RawState},State) ->
    State1 = State#state{raw_state=RawState,status=1, quit=QuitTime},
    {noreply,State1};

handle_ret({ok,RawState},State) ->
    State1 = State#state{raw_state=RawState},
    {ok,State1};
handle_ret({ok,RawState,S},State) ->
    State1 = State#state{raw_state=RawState},
    {ok,State1,S};
handle_ret({reply,Ret,RawState},State) ->
    State1= State#state{raw_state=RawState},
    {reply,Ret,State1};
handle_ret({reply,Ret,RawState,S},State) ->
    State1= State#state{raw_state=RawState},
    {reply,Ret,State1,S};
handle_ret({noreply,RawState},State) ->
    State1 = State#state{raw_state=RawState},
    {noreply,State1};
handle_ret({noreply,RawState,S},State) ->
    State1 = State#state{raw_state=RawState},
    {noreply,State1,S};
handle_ret({stop,Reason,RawState},State) ->
    State1= State#state{raw_state=RawState},
    {stop,Reason,State1};
handle_ret({stop,Reason,Reply,RawState},State) ->
    State1= State#state{raw_state=RawState},
    {stop,Reason,Reply,State1};
%% 其余消息
handle_ret(X,State) ->
    #state{model=Mod} = State,
    io:format("unkown ret ~p ~p", [Mod,X]),
    {noreply,State}.

init([Mod,Args]) ->
    Ret=apply(Mod,init,[Args]),
    self() ! init,
    handle_ret(Ret,#state{model=Mod,last=spt_time:utc_timestamp()}).

handle_call(Msg,From,State) ->
    #state{model=Mod,raw_state=Rs,status=Status} = State,
    case Status of
        undefined ->
            io:format("handle call uninited ~p ~p", [self(), Msg]),
            {reply,not_inited,State};
        _ ->
            Ret=evaluate(Mod,handle_call,[Msg,From,Rs]),
            handle_ret(Ret,State#state{last=spt_time:utc_timestamp()})
    end.

handle_cast(Msg, State) ->
    #state{model=Mod,raw_state=Rs,status=Status} = State,
    case Status of
        undefined ->
            io:format("handle cast uninited ~p ~p", [self(), Msg]),
            {noreply,State};
        _ ->
            Ret=evaluate(Mod,handle_cast,[Msg,Rs]),
            handle_ret(Ret,State#state{last=spt_time:utc_timestamp()})
    end.

handle_info(Info, State) ->
    #state{model=Mod,raw_state=Rs,status=Status,quit=Quit} = State,
    case {Info,Status} of
        {init,undefined} ->
            Ret = case catch evaluate(Mod,handle_info,[Info,Rs]) of X -> X end,
            case erlang:element(1,Ret) of
                inited -> ignore;
                E ->
                    io:format("Mode init failed mod ~p reason ~p", [Mod,E]),
                    erlang:send_after(5000, self(), init)
            end,
            handle_ret(Ret,State);
        {save,_} ->
            Last = State#state.last,
            Now = spt_time:utc_timestamp(),
            if
                Quit == infinity ->
                    Ret = case catch evaluate(Mod,handle_info,[Info,Rs]) of X -> X end,
                    handle_ret(Ret,State);
                (Now -Last) > Quit ->
                    {stop,normal,State};
                true ->
                    Ret = case catch evaluate(Mod,handle_info,[Info,Rs]) of X -> X end,
                    handle_ret(Ret,State)
            end;
        {_,undefined} ->
            io:format("handle info uninited ~p ~p ~p", [self(), Info, Mod]),
            {noreply,State};
        _ ->
            Ret=evaluate(Mod,handle_info,[Info,Rs]),
            handle_ret(Ret,State#state{last=spt_time:utc_timestamp()})
    end.

terminate(Reason,State) ->
    #state{model=Mod,raw_state=Rs} = State,
    Ret = Mod:terminate(Reason,Rs),
    handle_ret(Ret,State).

code_change(OldVsn, State, Extra) ->
    #state{model=Mod,raw_state=Rs} = State,
    Ret=Mod:code_change(OldVsn, Rs, Extra),
    handle_ret(Ret,State).

time_mfa(M,F,A) ->
    {T, V} = timer:tc(M, F, A),
    %% [A1|_]=A,
    if
        T > 10000 ->
            %% io:format("time:~p M:~p F:~p A:~p", [T,M,F,A1]);
            ignore;
        true ->
            ignore
    end,
    V.


%% 直接捕获异常.
evaluate(M,F,A) ->
    catch time_mfa(M,F,A).
