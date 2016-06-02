%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(storage).
-author("alboo").
-include_lib("traffic_light.hrl").

%% API
-export([
  find/1,
  save/1,
  reset/0
]).


find(_Uuid) ->
  undefined.


save(Sequence) when is_record(Sequence, sequence) ->
  ok.

reset() ->
  ok.
