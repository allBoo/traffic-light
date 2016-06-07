%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(recognizer_api).
-author("alboo").

-behaviour(elli_handler).
-include_lib("elli/include/elli.hrl").
-include_lib("traffic_light.hrl").

-define(TRY(Guard, Expr),
  begin
    case (Expr) of
      Guard -> Guard;
      {error, Error} -> ?THROW_ERROR({error, Error});
      __V -> ?THROW_ERROR({error, internal_error})
    end
  end).

  %case Expr of Guard -> Guard; {error, Error} -> ?THROW_ERROR({error, Error}); _ -> ?THROW_ERROR({error, internal_error}) end).

%% API
-export([handle/2, handle_event/3]).

%%
%% ELLI REQUEST CALLBACK
%%

handle(Req, _Args) ->
  %% Delegate to our handler function
  try handle(Req#req.method, elli_request:path(Req), Req)
  catch
    throw:{error, Error} ->
      error_response(Error);
    Type:Error ->
      ?WARN("Unknown error in api module ~p:~p", [Type, Error]),
      error_response(internal_error)
  end.


%% Route METHOD & PATH to the appropriate clause
handle('GET', [<<"sequence">>, <<"create">>], _Req) ->
  {ok, Uuid} = ?TRY({ok, _X}, recognizer_ctrl:create()),
  response(#{<<"sequence">> => list_to_binary(Uuid)});


handle('GET', [<<"clear">>], _Req) ->
  ?TRY(ok, recognizer_ctrl:reset()),
  response(<<"ok">>);


handle('GET', [<<"test_error">>, Uuid, TestError], _Req) ->
  ?TRY(neok, recognizer_ctrl:test_error(binary_to_list(Uuid), binary_to_atom(TestError, utf8))),
  error_response(internal_error);


handle(_, _, _Req) ->
  {404, [], <<"Not Found">>}.



response(Response) ->
  {ok, [],
    jiffy:encode(#{
      <<"status">>   => <<"ok">>,
      <<"response">> => Response
    })
  }.

error_response(Error) ->
  {ok, [],
    jiffy:encode(#{
      <<"status">> => <<"error">>,
      <<"msg">>    => decode_error(Error)
    })
  }.


decode_error(unresolved) -> <<"No solutions found">>;
decode_error(not_found) -> <<"The sequence isn't found">>;
decode_error(no_data) -> <<"There isn't enough data">>;
decode_error(already_finished) -> <<"The red observation should be the last">>;
decode_error(format_error) -> <<"Data format error">>;
decode_error(internal_error) -> <<"Internal error">>;
decode_error(Err) -> ?WARN("Undefined error ~p", [Err]), <<"Undefined error">>.

%%
%% ELLI EVENT CALLBACKS
%%


%% elli_startup is sent when Elli is starting up. If you are
%% implementing a middleware, you can use it to spawn processes,
%% create ETS tables or start supervised processes in a supervisor
%% tree.
handle_event(elli_startup, [], _) -> ok;

%% request_complete fires *after* Elli has sent the response to the
%% client. Timings contains timestamps of events like when the
%% connection was accepted, when request parsing finished, when the
%% user callback returns, etc. This allows you to collect performance
%% statistics for monitoring your app.
handle_event(request_complete, [_Request,
  _ResponseCode, _ResponseHeaders, _ResponseBody,
  _Timings], _) -> ok;

%% request_throw, request_error and request_exit events are sent if
%% the user callback code throws an exception, has an error or
%% exits. After triggering this event, a generated response is sent to
%% the user.
handle_event(request_throw, [_Request, _Exception, _Stacktrace], _) -> ok;
handle_event(request_error, [_Request, _Exception, _Stacktrace], _) -> ok;
handle_event(request_exit, [_Request, _Exception, _Stacktrace], _) -> ok;

%% invalid_return is sent if the user callback code returns a term not
%% understood by elli, see elli_http:execute_callback/1.
%% After triggering this event, a generated response is sent to the user.
handle_event(invalid_return, [_Request, _ReturnValue], _) -> ok;


%% chunk_complete fires when a chunked response is completely
%% sent. It's identical to the request_complete event, except instead
%% of the response body you get the atom "client" or "server"
%% depending on who closed the connection.
handle_event(chunk_complete, [_Request,
  _ResponseCode, _ResponseHeaders, _ClosingEnd,
  _Timings], _) -> ok;

%% request_closed is sent if the client closes the connection when
%% Elli is waiting for the next request on a keep alive connection.
handle_event(request_closed, [], _) -> ok;

%% request_timeout is sent if the client times out when
%% Elli is waiting for the request.
handle_event(request_timeout, [], _) -> ok;

%% request_parse_error fires if the request is invalid and cannot be
%% parsed by erlang:decode_packet/3 or it contains a path Elli cannot
%% parse or does not support.
handle_event(request_parse_error, [_], _) -> ok;

%% client_closed can be sent from multiple parts of the request
%% handling. It's sent when the client closes the connection or if for
%% any reason the socket is closed unexpectedly. The "Where" atom
%% tells you in which part of the request processing the closed socket
%% was detected: receiving_headers, receiving_body, before_response
handle_event(client_closed, [_Where], _) -> ok;

%% client_timeout can as with client_closed be sent from multiple
%% parts of the request handling. If Elli tries to receive data from
%% the client socket and does not receive anything within a timeout,
%% this event fires and the socket is closed.
handle_event(client_timeout, [_Where], _) -> ok;

%% bad_request is sent when Elli detects a request is not well
%% formatted or does not conform to the configured limits. Currently
%% the Reason variable can be any of the following: {too_many_headers,
%% Headers}, {body_size, ContentLength}
handle_event(bad_request, [_Reason], _) -> ok;

%% file_error is sent when the user wants to return a file as a
%% response, but for some reason it cannot be opened.
handle_event(file_error, [_ErrorReason], _) -> ok.
