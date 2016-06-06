%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("alboo").

%%% ====================================================================
%%% Main include file
%%% ====================================================================
-include_lib("log.hrl").
-include_lib("error.hrl").


%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(CHILD(I, A), {I, {I, start_link, A}, permanent, 5000, worker, [I]}).
-define(CHILD(N, I, A), {N, {I, start_link, A}, permanent, 5000, worker, [I]}).
-define(CHILD_SUP(I), {I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).
-define(CHILD_SUP_T(I), {I, {I, start_link, []}, transient, infinity, supervisor, [I]}).


%%% ====================================================================
%%% Data types
%%% ====================================================================

-record(sections, {
  first = 0  :: integer(),
  second = 0 :: integer()
}).

-record(start_item, {
  start = 0 :: integer(),
  last  = 0 :: integer(),
  missing = #sections{} :: #sections{}
}).

-record(sequence, {
  id           :: string(),
  start   = [] :: [#start_item{}],
  last    = [] :: [integer()],          %% last numbers
  missing = #sections{} :: #sections{}, %% sections which are identified as non working
  working = #sections{} :: #sections{}, %% sections which are identified as working
  finished = false :: boolean()         %% recognition is finished
}).
