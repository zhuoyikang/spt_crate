
-module(slg_support_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Notify = {
    spt_notify,
    {spt_notify, start_link, []},
    permanent,
    infinity,
    worker,
    [spt_notify]
   },
  Reloader = {
    spt_reloader,
    {spt_reloader, start_link, []},
    permanent,
    infinity,
    worker,
    [spt_reloader]
   },
  {ok, { {one_for_one, 5, 10}, [Notify, Reloader]} }.
