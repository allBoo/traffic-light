%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(recognizer_test).
-author("alboo").

-include_lib("traffic_light.hrl").
-include_lib("eunit/include/eunit.hrl").


register_test_() ->
  [
    ?_test(application:ensure_all_started(traffic_light)),
    ?_test(begin
      ExpectedUuid = "xxx",

      {ok, Pid} = gen_server:start(recognizer, [ExpectedUuid], []),
      ?assertEqual(recognizer:registered(ExpectedUuid), Pid),
      gen_server:stop(Pid),
      ?assertEqual(recognizer:registered(ExpectedUuid), undefined),

      {ok, Pid1} = gen_server:start(recognizer, [ExpectedUuid], []),
      ?assertEqual(recognizer:registered(ExpectedUuid), Pid1),
      gen_server:stop(Pid1),
      ?assertEqual(recognizer:registered(ExpectedUuid), undefined)
    end),
    ?_test(begin
       ExpectedUuid = "xxx",
       Sequence = #sequence{id = "xxx"},
       {ok, Pid} = gen_server:start(recognizer, [Sequence], []),
       ?assert(recognizer:registered(ExpectedUuid) =:= Pid),
       gen_server:stop(Pid),
       ?assert(recognizer:registered(ExpectedUuid) =:= undefined)
     end)
  ].


add_test_() ->
  [
    ?_test(begin
       InitialState = #sequence{},
       {reply, {ok, StartNumbers, Missing}, NewState, _} = recognizer:handle_call({add, {2#1110111, 2#0011101}}, undefined, InitialState),
       ?assert(StartNumbers =:= [2, 8, 82, 88]),
       ?assert(Missing =:= [2#0000000, 2#1000000]),
       ?assert(is_record(NewState, sequence)),
       ?assert(NewState#sequence.last =:= [2, 8, 82, 88]),
       ?assert(NewState#sequence.missing =:= #sections{first = 2#0000000, second = 2#1000000}),
       ?assert(NewState#sequence.working =:= #sections{first = 2#1110111, second = 2#0011101})
     end),

    ?_test(begin
       InitialState = #sequence{missing = #sections{first = 2#1111111, second = 2#1111111}},
       ?assertException(throw, {stop, normal, {error, unresolved}, _}, recognizer:handle_call({add, {2#1111111, 2#1111111}}, undefined, InitialState))
     end),

    ?_test(begin
       InitialState = #sequence{last = [1]},
       ?assertException(throw, {stop, normal, {error, unresolved}, _}, recognizer:handle_call({add, {2#1111111, 2#1111111}}, undefined, InitialState))
     end),

    ?_test(begin
       InitialState = #sequence{finished = true},
       ?assertMatch({stop, normal, {error, already_finished}, InitialState}, recognizer:handle_call({add, {2#1110111, 2#0011101}}, undefined, InitialState))
     end)
  ].

done_test_() ->
  [
    ?_test(begin
      InitialState = #sequence{
        start   = [#start_item{
          start = 1,
          last  = 1
        }],
        last    = [1],
        working = #sections{second = 2#0010010}
      },
      ?assertMatch({stop, normal, {ok, [1], [0, 0]}, _NewState}, recognizer:handle_call(done, undefined, InitialState))
    end),

    ?_test(begin
      InitialState = #sequence{last = [99]},
      ?assertException(throw, {stop, normal, {error, unresolved}, _}, recognizer:handle_call(done, undefined, InitialState))
    end),

    ?_test(begin
       InitialState = #sequence{finished = true},
       ?assertMatch({stop, normal, {error, already_finished}, InitialState}, recognizer:handle_call(done, undefined, InitialState))
     end),

    ?_test(begin
       InitialState = #sequence{},
       ?assertMatch({stop, normal, {error, no_data}, _}, recognizer:handle_call(done, undefined, InitialState))
    end)
  ].


reset_test() ->
  InitialState = #sequence{id = "xxx", last = [1, 2, 3], finished = true},
  ExpectedState = #sequence{id = "xxx"},
  ?assertMatch({reply, ok, ExpectedState, _}, recognizer:handle_call(reset, undefined, InitialState)).


get_id_test() ->
  InitialState = #sequence{id = "xxx"},
  ?assertMatch({reply, "xxx", InitialState, _}, recognizer:handle_call(get_id, undefined, InitialState)).


any_request_test() ->
  InitialState = #sequence{id = "xxx"},
  ?assertMatch({reply, {error, unknown}, InitialState, _}, recognizer:handle_call(any_request, undefined, InitialState)).


filter_neighbors_test_() ->
  [
    ?_assert(recognizer:filter_neighbors(any, [])),
    ?_assert(recognizer:filter_neighbors(23, [1, 24, 56])),
    ?_assertNot(recognizer:filter_neighbors(23, [1, 23, 56]))
  ].


get_missed_test() ->
  Variants = [
    #sections{first = 2#1000000, second = 2#1010101},
    #sections{first = 2#1000001, second = 2#0101010}
  ],
  Current = #sections{first = 2#0001000, second = 2#1111000},
  ?assertEqual(#sections{first = 2#1001000, second = 2#1111000}, recognizer:get_missed(Variants, Current)).


associate_variants_test_() ->
  [
    ?_test(begin
       AllVariants = [
         {1, #sections{first = 2#1111111, second = 2#0000000}},
         {10, #sections{first = 2#1010101, second = 2#0101010}}
       ],
       Expected = [
         #start_item{start = 1, last = 1, missing = #sections{first = 2#1111111, second = 2#0000000}},
         #start_item{start = 10, last = 10, missing = #sections{first = 2#1010101, second = 2#0101010}}
       ],
       ?assertEqual(Expected, recognizer:associate_variants(AllVariants, any, []))
     end),

    ?_test(begin
      AllVariants = [
        {1, #sections{first = 2#1101111, second = 2#0000000}},
        {10, #sections{first = 2#1010101, second = 2#0101010}}, %% will be filtered by missed sections
        {70, #sections{first = 2#1010101, second = 2#0101010}}  %% will be filtered by neighbors filter
      ],
      StartData = [
        #start_item{start = 2, last = 2, missing = #sections{first = 2#1101111, second = 2#0000000}},
        #start_item{start = 11, last = 11, missing = #sections{first = 2#1111111, second = 2#0000000}}
      ],
      Current = #sections{first = 2#0010000, second = 2#1110111},
      Expected = [
        #start_item{start = 2, last = 1, missing = #sections{first = 2#1101111, second = 2#0000000}}
      ],
      ?assertEqual(Expected, recognizer:associate_variants(AllVariants, Current, StartData))
    end)
  ].


stl_test() ->
  ?assertEqual([2#1110001, 2#1011101],  recognizer:stl(#sections{first = 2#1110001, second = 2#1011101})).

