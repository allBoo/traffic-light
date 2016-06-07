%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(alphabet).
-author("alboo").

-include_lib("traffic_light.hrl").

-define(ALPHABET, [
  %%  0   1   2   3   4   5   6   7   8   9
  %%  -       -   -       -   -   -   -   -
  %% | |   |   |   | | | |   |     | | | | |
  %%          -   -   -   -   -       -   -
  %% | |   | |     |   |   | | |   | | |   |
  %%  -       -   -       -   -       -   -
  %%
  %%    -0
  %% 1 | | 2
  %%    -3
  %% 4 | | 5
  %%    -6
  {0, 2#1110111},
  {1, 2#0010010},
  {2, 2#1011101},
  {3, 2#1011011},
  {4, 2#0111010},
  {5, 2#1101011},
  {6, 2#1101111},
  {7, 2#1010010},
  {8, 2#1111111},
  {9, 2#1111011}
]).

%% API
-export([get_matches/1, encode/1]).


get_matches(-1) -> [{0, 0}];
get_matches(Input) when is_integer(Input), Input >= 0, Input < 128 ->
  get_matches(?ALPHABET, Input, []);
get_matches(_) ->
  ?THROW_ERROR({error, format_error}).

get_matches([], _, Acc) -> Acc;
get_matches([{Num, Code} | Tail], Input, Acc) when (Input band Code =:= Input) ->
    get_matches(Tail, Input, Acc ++ [{Num, Code band bnot Input}]);   %% collect number and missed sections
                                                                      %% @todo test for XOR
get_matches([_ | Tail], Input, Acc) ->
  get_matches(Tail, Input, Acc).

encode(Num) ->
  element(2, lists:nth(Num + 1, ?ALPHABET)).
