%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(alphabet_test).
-author("alboo").

-include_lib("traffic_light.hrl").
-include_lib("eunit/include/eunit.hrl").


matches_test() ->
  ?assert(alphabet:get_matches(2#1110111) =:= [{0, 2#0000000}, {8, 2#0001000}]).

encode_test() ->
  ?assert(alphabet:encode(4) =:= 2#0111010).
