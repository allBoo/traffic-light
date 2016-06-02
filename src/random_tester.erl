%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(random_tester).
-author("alboo").
-include_lib("traffic_light.hrl").

%% API
-export([start/2]).

start(0, _) -> done;
start(Sequences, Tests) ->
  ?LOG("Start sequence ~p with ~p tests", [Sequences, Tests]),
  random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),

  MissedFirst = random:uniform(128) - 1,
  MissedSecond = random:uniform(128) - 1,

  ?LOG("Start test sequence with missed ~p", [[format:print(MissedFirst), format:print(MissedSecond)]]),
  run_tests(MissedFirst, MissedSecond, Tests),
  start(Sequences - 1, Tests).

run_tests(_, _, 0) -> ok;
run_tests(MissedFirst, MissedSecond, Tests) ->
  StartValue = random:uniform(99),
  ?LOG("Run test with missed ~p and start value ~p", [[format:print(MissedFirst), format:print(MissedSecond)], StartValue]),
  run_test(MissedFirst, MissedSecond, StartValue),
  run_tests(MissedFirst, MissedSecond, Tests - 1).


run_test(MissedFirst, MissedSecond, StartValue) ->
  recognizer:reset(),
  run_observation(MissedFirst, MissedSecond, StartValue, StartValue).



run_observation(MissedFirst, MissedSecond, StartValue, 0) ->
  Iterations = StartValue,
  case recognizer:done() of
    {ok, [], _} ->
      ?ERR("Test failed with ~p iterations! Empty response", [Iterations]);

    {ok, [DetectedStartValue], [DetectedMissingFirst, DetectedMissingSecond]} when
      DetectedStartValue == StartValue,
      DetectedMissingFirst bor MissedFirst == MissedFirst,
      DetectedMissingSecond bor MissedSecond == MissedSecond ->
      ?LOG("Test passed with ~p iterations", [Iterations]);

    {ok, [DetectedStartValue], [DetectedMissingFirst, DetectedMissingSecond]} when DetectedStartValue == StartValue ->
      ?WARN("Test incompete with ~p iterations!\nExpected value ~p, got ~p\nExpected missed first ~p, got ~p\nExpected missed second ~p, got ~p",
        [Iterations, StartValue, DetectedStartValue, format:print(MissedFirst), format:print(DetectedMissingFirst), format:print(MissedSecond), format:print(DetectedMissingSecond)]);

    {ok, [DetectedStartValue], [DetectedMissingFirst, DetectedMissingSecond]} ->
      ?WARN("Test failed with ~p iterations!\nExpected value ~p, got ~p\nExpected missed first ~p, got ~p\nExpected missed second ~p, got ~p",
        [Iterations, StartValue, DetectedStartValue, format:print(MissedFirst), format:print(DetectedMissingFirst), format:print(MissedSecond), format:print(DetectedMissingSecond)]);

    {ok, DetectedStartValues, [DetectedMissingFirst, DetectedMissingSecond]} ->
      ?WARN("Test incompete with ~p iterations!\nExpected value ~p, got ~p\nExpected missed first ~p, got ~p\nExpected missed second ~p, got ~p",
        [Iterations, StartValue, DetectedStartValues, format:print(MissedFirst), format:print(DetectedMissingFirst), format:print(MissedSecond), format:print(DetectedMissingSecond)]);

    Err ->
      ?ERR("Test failed with ~p iterations! Error ~p", [Iterations, Err])
  end;

run_observation(MissedFirst, MissedSecond, StartValue, CurrentValue) ->
  Iterations = StartValue - CurrentValue,
  EncodedFirst = alphabet:encode(CurrentValue div 10) band (bnot MissedFirst),
  EncodedSecond = alphabet:encode(CurrentValue rem 10) band (bnot MissedSecond),

  case recognizer:add({EncodedFirst, EncodedSecond}) of
    {ok, [], _} ->
      ?ERR("Test failed with ~p iterations! Empty response", [Iterations]);

    {ok, [DetectedStartValue], [DetectedMissingFirst, DetectedMissingSecond]} when
      DetectedStartValue == StartValue,
      DetectedMissingFirst bor MissedFirst == MissedFirst,
      DetectedMissingSecond bor MissedSecond == MissedSecond ->
      ?LOG("Test passed with ~p iterations", [Iterations]);

    {ok, [DetectedStartValue], [DetectedMissingFirst, DetectedMissingSecond]} when DetectedStartValue == StartValue ->
      ?WARN("Test incompete with ~p iterations!\nExpected value ~p, got ~p\nExpected missed first ~p, got ~p\nExpected missed second ~p, got ~p",
        [Iterations, StartValue, DetectedStartValue, format:print(MissedFirst), format:print(DetectedMissingFirst), format:print(MissedSecond), format:print(DetectedMissingSecond)]);

    {ok, [DetectedStartValue], [DetectedMissingFirst, DetectedMissingSecond]} ->
      ?WARN("Test failed with ~p iterations!\nExpected value ~p, got ~p\nExpected missed first ~p, got ~p\nExpected missed second ~p, got ~p",
        [Iterations, StartValue, DetectedStartValue, format:print(MissedFirst), format:print(DetectedMissingFirst), format:print(MissedSecond), format:print(DetectedMissingSecond)]);

    {ok, _, _} ->
      run_observation(MissedFirst, MissedSecond, StartValue, CurrentValue - 1);

    Err ->
      ?ERR("Test failed with ~p iterations! Error ~p", [Iterations, Err])
  end.

