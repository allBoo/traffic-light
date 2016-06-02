%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(format).
-author("alboo").

-include_lib("traffic_light.hrl").


%% API
-export([
  print/1,
  decode/1
]).

print(Sections) when is_list(Sections) ->
  [print(Section) || Section <- Sections, is_record(Section, sections)];

print(Section) when is_record(Section, sections) ->
  [print(Section#sections.first), print(Section#sections.second)];

print(Digit) when is_integer(Digit) ->
  [Bin2] = io_lib:fwrite("~.2B", [Digit]),
  Len = length(Bin2),
  if
    Len < 7 ->
      [Res] = io_lib:format("~s", [string:chars($0, 7 - Len, Bin2)]),
      list_to_binary(Res);
    true -> list_to_binary(Bin2)
  end.

%% "1110111" > 2#1110111 (119)
decode(Num) when is_integer(Num), Num >= 0, Num < 128 -> Num;
decode(Num) when is_list(Num), length(Num) == 7 -> decode(Num, 0);
decode(<<Num:56/bitstring>>) -> decode(Num, 0);
decode(_) -> ?THROW_ERROR(format_error).

decode([], Acc) -> Acc;
decode(<<"">>, Acc) -> Acc;
decode([$1 | Tail], Acc) ->
  decode(Tail, (Acc bsl 1) bor 1);
decode([$0 | Tail], Acc) ->
  decode(Tail, Acc bsl 1);
decode(<<"1",Tail/bitstring>>, Acc) ->
  decode(Tail, (Acc bsl 1) bor 1);
decode(<<"0",Tail/bitstring>>, Acc) ->
  decode(Tail, Acc bsl 1);
decode(_, _) ->
  ?THROW_ERROR(format_error).
