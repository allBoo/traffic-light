%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(recognizer_sup).
-author("alboo").

-behaviour(supervisor).
-include_lib("traffic_light.hrl").

%% API
-export([
  start_link/0,
  start_recognizer/1,
  stop_recognizer/1,
  stop_all/0,
  list_all/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_recognizer(Uuid) when is_list(Uuid) ->
  {ok, Pid} = supervisor:start_child(?SERVER, [Uuid]),
  Pid;

start_recognizer(Sequence) when is_record(Sequence, sequence) ->
  {ok, Pid} = supervisor:start_child(?SERVER, [Sequence]),
  Pid.


stop_recognizer(Pid) when is_pid(Pid) ->
  supervisor:terminate_child(?SERVER, Pid);

stop_recognizer(Uuid) when is_list(Uuid) ->
  case recognizer:registered(Uuid) of
    Pid when is_pid(Pid) ->
      supervisor:terminate_child(?SERVER, Pid);

    _ ->
      {error, not_found}
  end.


stop_all() ->
  supervisor:terminate_child(traffic_light_sup, ?SERVER),
  supervisor:restart_child(traffic_light_sup, ?SERVER).


list_all() ->
  [recognizer:get_id(Pid) || {_, Pid , _, _} <- supervisor:which_children(?SERVER)].

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [
    {recognizer, {recognizer, start_link, []}, temporary, 5, worker, [recognizer]}
  ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
