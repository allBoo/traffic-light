%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Июнь 2016 23:38
%%%-------------------------------------------------------------------
-module(recognizer_random_test).
-author("alex").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).


random_test_() ->
  application:ensure_all_started(traffic_light),
  lager:set_loglevel(lager_console_backend, error),
  random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
  start(100, 10).

start(Sequences, Tests) ->
  MissedFirst = random:uniform(128) - 1,
  MissedSecond = random:uniform(128) - 1,

  {generator,
   fun () ->
     if
       Sequences > 0 ->
         run_tests(MissedFirst, MissedSecond, Tests) ++ start(Sequences - 1, Tests);
       true ->
         application:stop(traffic_light),
         []
      end
   end
  }.

run_tests(_, _, 0) -> [];
run_tests(MissedFirst, MissedSecond, Tests) ->
  StartValue = random:uniform(99),
  [?_test(run_test(MissedFirst, MissedSecond, StartValue)) | run_tests(MissedFirst, MissedSecond, Tests - 1)].

run_test(MissedFirst, MissedSecond, StartValue) ->
  Pid = prepare(),
  run_observation(Pid, MissedFirst, MissedSecond, StartValue, StartValue),
  cleanup().


run_observation(Pid, MissedFirst, MissedSecond, StartValue, 0) ->
  Response = recognizer_ctrl:observation(Pid, {red}),
  ?_assertMatch({ok, [StartValue], [_, _]}, Response),
  {ok, [_], [DetectedMissingFirst, DetectedMissingSecond]} = Response,
  ?_assert(DetectedMissingFirst bor MissedFirst == MissedFirst),
  ?_assert(DetectedMissingSecond bor MissedSecond == MissedSecond);


run_observation(Pid, MissedFirst, MissedSecond, StartValue, CurrentValue) ->
  EncodedFirst = alphabet:encode(CurrentValue div 10) band (bnot MissedFirst),
  EncodedSecond = alphabet:encode(CurrentValue rem 10) band (bnot MissedSecond),

  Response = recognizer_ctrl:observation(Pid, {green, EncodedFirst, EncodedSecond}),
  ?_assertMatch({ok, _, [_, _]}, Response),
  {ok, DetectedStartValues, [DetectedMissingFirst, DetectedMissingSecond]} = Response,
  ?_assertNotEqual(DetectedStartValues, []),

  case DetectedStartValues of
    [DetectedStartValue] ->
      ?_assertEqual(DetectedStartValue, StartValue),
      ?_assert(DetectedMissingFirst bor MissedFirst == MissedFirst),
      ?_assert(DetectedMissingSecond bor MissedSecond == MissedSecond);

    _ ->
      run_observation(Pid, MissedFirst, MissedSecond, StartValue, CurrentValue - 1)
  end.



prepare()->
  {ok, Pid} = recognizer_ctrl:create(),
  Pid.

cleanup() ->
  recognizer_ctrl:reset().
