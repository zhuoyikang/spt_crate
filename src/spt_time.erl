%%%-------------------------------------------------------------------
%%% @author zhuoyikang <>
%%% @copyright (C) 2016, zhuoyikang
%%% @doc
%%% 封装一些时间相关函数.
%%% @end
%%% Created : 14 Jan 2016 by zhuoyikang <>
%%%-------------------------------------------------------------------


-module(spt_time).

-export([utc_timestamp/0]).


%% --------------------------------------
%% @doc 服务器Unix时间戳
%% --------------------------------------

utc_timestamp() ->
    {MegaSecs, Secs, _} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
