%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(recognizer).
-author("alboo").

-include_lib("traffic_light.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  reset/1,
  add/2,
  done/1,
  registered/1,
  get_id/1,
  test_error/2
]).

-ifdef(TEST).
-export([
  filter_neighbors/2,
  get_missed/2,
  get_missed_section/2,
  associate_variants/3,
  stl/1
]).
-endif.

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(KEEP_ALIVE, 100000).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Data :: string() | #sequence{}) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Data) ->
  gen_server:start_link(?MODULE, [Data], []).

%%--------------------------------------------------------------------
%% @doc
%% Resets the internal state
%% Only for debugging and testing
%%
%% @end
%%--------------------------------------------------------------------
-spec(reset(Pid :: pid()) ->
  {ok}).
reset(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, reset).


%%--------------------------------------------------------------------
%% @doc
%% Add a new observation
%%
%% @end
%%--------------------------------------------------------------------
add(Pid, {First, Second}) when is_pid(Pid) ->
  gen_server:call(Pid, {add, {First, Second}}).

%%--------------------------------------------------------------------
%% @doc
%% Finish the observation
%%
%% @end
%%--------------------------------------------------------------------
done(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, done).


%%--------------------------------------------------------------------
%% @doc
%% Find pid of registered recognizer process by it uuid
%%
%% @end
%%--------------------------------------------------------------------
registered(Uuid) ->
  reg:find({recognizer, Uuid}).


%%--------------------------------------------------------------------
%% @doc
%% Returns uuid of recognizer by it id
%% only for debug
%%
%% @end
%%--------------------------------------------------------------------
get_id(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, get_id).


%%--------------------------------------------------------------------
%% @doc
%% Tests for exceptions
%%
%% @end
%%--------------------------------------------------------------------
test_error(Pid, Error) when is_pid(Pid) ->
  gen_server:call(Pid, {test_error, Error}).

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
  {ok, State :: #sequence{}} | {ok, State :: #sequence{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Uuid]) when is_list(Uuid) ->
  process_flag(trap_exit, true),
  register(Uuid),
  {ok, #sequence{id = Uuid}, ?KEEP_ALIVE};


init([Sequence]) when is_record(Sequence, sequence) ->
  process_flag(trap_exit, true),
  register(Sequence#sequence.id),
  {ok, Sequence, ?KEEP_ALIVE}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #sequence{}) ->
  {reply, Reply :: term(), NewState :: #sequence{}} |
  {reply, Reply :: term(), NewState :: #sequence{}, timeout() | hibernate} |
  {noreply, NewState :: #sequence{}} |
  {noreply, NewState :: #sequence{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #sequence{}} |
  {stop, Reason :: term(), NewState :: #sequence{}}).


handle_call({add, _}, _From, State) when State#sequence.finished =:= true ->
  {stop, normal, {error, already_finished}, State};

handle_call({add, {First, Second}}, _From, State) ->
  NewState = process_with_sections(First, Second, State),
  StartNumbers = [StartItem#start_item.start || StartItem <- NewState#sequence.start],
  Missing = stl(NewState#sequence.missing),

  if
    length(StartNumbers) == 1 ->
      {stop, normal, {ok, StartNumbers, Missing}, NewState};
    true ->
      {reply, {ok, StartNumbers, Missing}, NewState, ?KEEP_ALIVE}
  end;


handle_call(done, _From, State) when State#sequence.finished =:= true ->
  {stop, normal, {error, already_finished}, State};

handle_call(done, _From, State) when State#sequence.last =:= [] ->
  {stop, normal, {error, no_data}, State};

handle_call(done, _From, State) ->
  First = Second = -1,
  NewState = process_with_sections(First, Second, State),
  StartNumbers = [StartItem#start_item.start || StartItem <- NewState#sequence.start],
  Missing = stl(NewState#sequence.missing),

  {stop, normal, {ok, StartNumbers, Missing}, NewState#sequence{finished = true}};


handle_call(reset, _From, State) ->
  {reply, ok, #sequence{id = State#sequence.id}, ?KEEP_ALIVE};

handle_call(get_id, _From, State) ->
  {reply, State#sequence.id, State, ?KEEP_ALIVE};

handle_call({test_error, Error}, _From, State) ->
  assert(false, Error, State),
  {reply, neok, State, ?KEEP_ALIVE};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown}, State, ?KEEP_ALIVE}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #sequence{}) ->
  {noreply, NewState :: #sequence{}} |
  {noreply, NewState :: #sequence{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sequence{}}).
handle_cast(_Request, State) ->
  {noreply, State, ?KEEP_ALIVE}.

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
-spec(handle_info(Info :: timeout() | term(), State :: #sequence{}) ->
  {noreply, NewState :: #sequence{}} |
  {noreply, NewState :: #sequence{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sequence{}}).

handle_info(timeout, State) ->
  ?LOG("Stop recognizer ~p by timeout", [State#sequence.id]),
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State, ?KEEP_ALIVE}.

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
    State :: #sequence{}) -> term()).
terminate(Reason, State) ->
  ?LOG("Recognizer ~p terminated with reason ~p and state ~p", [State#sequence.id, Reason, State]),
  deregister(State#sequence.id),
  storage:save(State),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #sequence{},
    Extra :: term()) ->
  {ok, NewState :: #sequence{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% register recognizer server with it uuid
register(Uuid) ->
  ok = reg:name({recognizer, Uuid}).

%% deregister recognizer server
deregister(Uuid) ->
  reg:unname({recognizer, Uuid}).


%% main process function
process_with_sections(First, Second, State) ->
  FirstVariants = alphabet:get_matches(First),
  SecondVariants = alphabet:get_matches(Second),

  S = fun(X) when X > 0 -> X; (_) -> 0 end,

  WorkingSection = #sections{
    first = (State#sequence.working)#sections.first bor S(First),
    second = (State#sequence.working)#sections.second bor S(Second)
  },

  %% check for missed sections (if working sections crossing exists)
  Exp = ((State#sequence.missing)#sections.first band WorkingSection#sections.first == 0) and
    ((State#sequence.missing)#sections.second band WorkingSection#sections.second == 0),
  assert(Exp, unresolved, State#sequence{working = WorkingSection}),

  %% generate all available variant of pairs of digits and missing sections
  AllVariants = generate_variants(FirstVariants, SecondVariants, WorkingSection, State#sequence.last),
  assert(AllVariants /= [], unresolved, State#sequence{working = WorkingSection}),

  %% correlate new variants with all possible start positions
  StartData = associate_variants(AllVariants, #sections{first = S(First), second = S(Second)}, State#sequence.start),

  LastNumbers = [StartItem#start_item.last || StartItem <- StartData],

  %% calculate missing sections for each possible start position and globally known
  AllVariantsMissing = [M || {_, M} <- AllVariants],
  AllSequenceMissing = [StartItem#start_item.missing || StartItem <- StartData],

  MissingByVariants = get_missed(AllVariantsMissing, State#sequence.missing),
  Missing = get_missed(AllSequenceMissing, MissingByVariants),

  NewState = State#sequence{
    start = StartData,
    last = LastNumbers,
    missing = Missing,
    working = WorkingSection
  },
  assert(StartData /= [], unresolved, NewState),
  NewState.


%% return true if exists at least one number less than 1 that passed
filter_neighbors(_, []) -> true;
filter_neighbors(Number, Exists) ->
  lists:any(fun(El) -> El - 1 == Number end, Exists).


%% generates all possible variants of numbers on the traffic-light
generate_variants([{0, 0}], [{0, 0}], _, Last) ->
  case filter_neighbors(0, Last) of
    true -> [{0, #sections{}}];
    _ -> []
  end;
generate_variants(FirstVariants, SecondVariants, WorkingSection, Last) ->
  [
    {N1 * 10 + N2, #sections{first = M1, second = M2}} || {N1, M1} <- FirstVariants, {N2, M2} <- SecondVariants,
    %% filter 00
    (N1 /= 0) or (N2 /= 0),
    %% filter by working sections
    M1 band WorkingSection#sections.first == 0,
    M2 band WorkingSection#sections.second == 0,
    %% quick filter only neighboring numbers
    filter_neighbors(N1 * 10 + N2, Last)
  ].


%% calculates missed sections
get_missed(Variants, Current) ->
  MissedOnStep = lists:foldl(fun(Section, Acc) ->
                   #sections{
                     first = Acc#sections.first band Section#sections.first ,
                     second = Acc#sections.second band Section#sections.second
                   }
                 end, #sections{first = 2#1111111, second = 2#1111111}, Variants),
  get_missed_section(Current, MissedOnStep).

get_missed_section(Current, New) ->
  #sections{
    first = Current#sections.first bor New#sections.first,
    second = Current#sections.second bor New#sections.second
  }.

%% correlate new variants with all possible start positions
associate_variants(AllVariants, _, []) ->
  lists:map(fun ({N, M}) ->
              #start_item{
                start = N,
                last = N,
                missing = M
              }
            end, AllVariants);
associate_variants(AllVariants, Current, StartData) ->
  [
    StartItem#start_item{
      last = Number,
      missing = get_missed_section(StartItem#start_item.missing, Missing)
    }
    || {Number, Missing} <- AllVariants, StartItem <- StartData,
    Number + 1 == StartItem#start_item.last,
    %% filter by missing sections
    Current#sections.first band (StartItem#start_item.missing)#sections.first == 0,
    Current#sections.second band (StartItem#start_item.missing)#sections.second == 0
  ].


stl(Sections) ->
  [Sections#sections.first, Sections#sections.second].


assert(true, _, _) -> ok;
assert(false, Reason, State) ->
  ?THROW_GS_ERROR({error, Reason}, State).
