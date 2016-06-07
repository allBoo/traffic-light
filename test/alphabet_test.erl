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


matches_test_() ->
  [
    ?_assertEqual([{0, 2#0000000}, {8, 2#0001000}], alphabet:get_matches(2#1110111)),
    ?_assertEqual([{0, 0}], alphabet:get_matches(-1)),
    ?_assertException(throw, {error, format_error}, alphabet:get_matches(-2)),
    ?_assertException(throw, {error, format_error}, alphabet:get_matches(128))
  ].

encode_test() ->
  ?assertEqual(2#0111010, alphabet:encode(4)).
