%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%% gproc aliases
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(format_test).
-author("alboo").

-include_lib("traffic_light.hrl").
-include_lib("eunit/include/eunit.hrl").


print_test_() ->
  [
    ?_assert(format:print(2#1111111) =:= <<"1111111">>),
    ?_assert(format:print(0) =:= <<"0000000">>),
    ?_assert(format:print(2#0101010) =:= <<"0101010">>),
    ?_assert(format:print(#sections{first = 2#0101010, second = 2#1010101}) =:= [<<"0101010">>, <<"1010101">>]),
    ?_assert(format:print(
      [
        #sections{first = 2#0101010, second = 2#1010101},
        #sections{first = 2#0000000, second = 2#1111111},
        ignore, "test"
      ]
    ) =:= [[<<"0101010">>, <<"1010101">>], [<<"0000000">>, <<"1111111">>]])
  ].


decode_test_() ->
  [
    ?_assert(format:decode(122) =:= 122),
    ?_assertException(throw, format_error, format:decode(-1)),
    ?_assertException(throw, format_error, format:decode(128)),
    ?_assert(format:decode("0101010") =:= 2#0101010),
    ?_assert(format:decode("0000000") =:= 0),
    ?_assertException(throw, format_error, format:decode("111")),
    ?_assertException(throw, format_error, format:decode("1234567")),
    ?_assert(format:decode(<<"0101010">>) =:= 2#0101010),
    ?_assert(format:decode(<<"0000000">>) =:= 0),
    ?_assertException(throw, format_error, format:decode(<<"111">>)),
    ?_assertException(throw, format_error, format:decode(<<"1234567">>))
  ].
