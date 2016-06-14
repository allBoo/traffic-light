%%%-------------------------------------------------------------------
%% @doc traffic_light top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(traffic_light_sup).
-include_lib("traffic_light.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(config),
        ?CHILD(db_cluster),
        ?CHILD_SUP(recognizer_sup),
        ?CHILD(recognizer_ctrl),
        ?CHILD(api)
    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
