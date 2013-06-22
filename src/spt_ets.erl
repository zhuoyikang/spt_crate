-module(spt_ets).
-export([safe_create/2]).

%% 安全新建ets表
safe_create(AtomName, Options) ->
  case ets:info(AtomName) of
    undefined -> ets:new(AtomName, Options);
    _Other -> AtomName
  end.
