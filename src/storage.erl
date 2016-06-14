%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(storage).
-author("alboo").
-include_lib("traffic_light.hrl").

%% API
-export([
  find/1,
  save/1,
  reset/0
]).


%%--------------------------------------------------------------------
%% @doc
%% Find saved sequence by it uuid
%%
%% @end
%%--------------------------------------------------------------------
find(Uuid) ->
  case mnesia:transaction(fun() -> mnesia:select(sequence, [{#sequence{id = Uuid}, [], ['$_']}]) end) of
    {atomic, [Value]} -> Value;
    {atomic, []} -> undefined;
    Any -> Any
  end.


%%--------------------------------------------------------------------
%% @doc
%% Save sequence info
%%
%% @end
%%--------------------------------------------------------------------
save(Sequence) when is_record(Sequence, sequence) ->
  response(mnesia:transaction(fun() -> mnesia:write(Sequence) end)).


%%--------------------------------------------------------------------
%% @doc
%% Clear all saved sequences
%%
%% @end
%%--------------------------------------------------------------------
reset() ->
  response(mnesia:clear_table(sequence)).


response({atomic, Value}) -> Value;
response(Value) -> Value.
