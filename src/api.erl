%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(api).
-author("alboo").
-include_lib("elli/include/elli.hrl").

%% API
-export([start_link/0]).

start_link() ->
  Config = [
    {mods, [
      {recognizer_api, []}
    ]}
  ],

  %{ok, IPTupled}  = inet_parse:address(config:get("http.host", "127.0.0.1")),
  {ok, IPTupled}  = inet_parse:address("0.0.0.0"),

  elli:start_link([
    {callback, elli_middleware},
    {callback_args, Config},
    {ip, IPTupled},
    %{port, config:get("http.port", 9900)},
    {port, 9900},
    {min_acceptors, 5}
  ]).
