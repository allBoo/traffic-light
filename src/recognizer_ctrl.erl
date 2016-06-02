%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(recognizer_ctrl).
-author("alboo").
-include_lib("traffic_light.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  create/0,
  observation/2,
  reset/0
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%--------------------------------------------------------------------
%% @doc
%% Resets the internal state
%%
%% @end
%%--------------------------------------------------------------------
-spec(create() ->
  {ok, SequenceId :: atom()} | {error, Reason :: term()}).
create() ->
  gen_server:call(?SERVER, create).


%%--------------------------------------------------------------------
%% @doc
%% Add a new observation
%%
%% @end
%%--------------------------------------------------------------------
-spec(observation(Sequence :: term(), {Color :: green, FirstNumber :: integer(), SecondNumber :: integer()} | {Color :: red}) ->
  {ok}).
observation(Sequence, {green, FirstNumber, SecondNumber}) when is_integer(FirstNumber), is_integer(SecondNumber) ->
  gen_server:call(?SERVER, {observation, {Sequence, green, FirstNumber, SecondNumber}});

observation(Sequence, {red}) ->
  gen_server:call(?SERVER, {observation, {Sequence, red}}).


%%--------------------------------------------------------------------
%% @doc
%% Resets all saved data and stops existing recognizers
%%
%% @end
%%--------------------------------------------------------------------
-spec(reset() ->
  {ok}).
reset() ->
  gen_server:call(?SERVER, reset).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(create, _From, State) ->
  Uuid = create_unique_id(),
  recognizer_sup:start_recognizer(Uuid),
  {reply, {ok, Uuid}, State};


handle_call({observation, {Uuid, green, FirstNumber, SecondNumber}}, _From, State) ->
  {reply, find_and_call(Uuid, add, [{FirstNumber, SecondNumber}]), State};

handle_call({observation, {Uuid, red}}, _From, State) ->
  {reply, find_and_call(Uuid, done, []), State};


handle_call(reset, _From, State) ->
  recognizer_sup:stop_all(),
  storage:reset(),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% create new Uuid and check it unique value
create_unique_id() ->
  Uuid = uuid:uuid_to_string(uuid:get_v4()),
  case uuid_is_unique(Uuid) of
    true -> Uuid;
    _ -> create_unique_id()
  end.

uuid_is_unique(Uuid) ->
  (recognizer:registered(Uuid) =:= undefined) and (storage:find(Uuid) =:= undefined).


find_and_call(Uuid, Fun, Args) ->
  case recognizer:registered(Uuid) of
    Pid when is_pid(Pid) ->
      apply(recognizer, Fun, [Pid | Args]);

    _ ->
      case storage:find(Uuid) of
        Sequence when is_record(Sequence, sequence) ->
          Pid = recognizer_sup:start_recognizer(Sequence),
          apply(recognizer, Fun, [Pid | Args]);

        undefined -> {error, not_found}
      end
  end.
