%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(storage_test).
-author("alboo").

-include_lib("traffic_light.hrl").
-include_lib("eunit/include/eunit.hrl").


storage_test_() ->
  [
    ?_assertEqual(ok, storage:reset()),
    ?_assertEqual(undefined, storage:find("1234")),
    ?_test(begin
       ExpectedSequence = #sequence{id = "1234", last = [1, 2, 3]},
       ?assertEqual(undefined, storage:find("1234")),
       ?assertEqual(ok, storage:save(ExpectedSequence)),
       ?assertEqual(ExpectedSequence, storage:find("1234")),
       ?assertEqual(ok, storage:reset()),
       ?assertEqual(undefined, storage:find("1234"))
     end)
  ].
