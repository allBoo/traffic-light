%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(migration_1).
-author("alboo").
-include_lib("traffic_light.hrl").

%% API
-export([
  up/0
]).


up() ->
  mnesia:create_table(sequence, [{disc_copies, [node()]}, {attributes, record_info(fields, sequence)}]).
