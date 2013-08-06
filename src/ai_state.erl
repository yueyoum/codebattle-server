-module(ai_state).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {worker, id, status, cx, cz, tx, tz}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(WorkerPid, MarineId) ->
    gen_server:start_link(?MODULE, [WorkerPid, MarineId], []).

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
init([WorkerPid, MarineId]) ->
    {ok, #state{worker=WorkerPid, id=MarineId}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({ai_state, Id, Status, Cx, Cz, Tx, Tz}, #state{id=Id} = State) ->
    NewState = State#state{status=Status, cx=Cx, cz=Cz, tx=Tx, tz=Tz},
    {noreply, NewState, 500}.



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
handle_info(timeout, #state{worker=Worker, id=Id, status=Status, cx=Cx, cz=Cz, tx=Tx, tz=Tz} = State) ->
    %% we only need to calculate this marine's position when he is under 'Run' status.
    NewState =
    case Status of
        'Run' ->
            {NewX, NewZ} = cal_run_position(2.5, Cx, Cz, Tx, Tz),
            gen_server:cast(Worker, {ai_state_report, Id, NewX, NewZ, Tx, Tz}),
            State#state{cx=NewX, cz=NewZ};
        _ ->
            State
    end,
    {noreply, NewState, 500}.

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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



cal_run_position(_Unit, Cx, Cz, Tx, Tz) when Cx =:= Tx, Cz =:= Tz ->
    {Cx, Cz};

cal_run_position(Unit, Cx, Cz, Tx, Tz) when Cx =:= Tx ->
    U =
    case Tz > Cz of
        true -> Unit;
        false -> -Unit
    end,
    {Cx, Cz + U};

cal_run_position(Unit, Cx, Cz, Tx, Tz) when Cz =:= Tz ->
    U = 
    case Tx > Cx of
        true -> Unit;
        false -> -Unit
    end,
    {Cx + U, Cz};

cal_run_position(Unit, Cx, Cz, Tx, Tz) ->
    U =
    case Tx > Cx of
        true -> Unit;
        false -> -Unit
    end,

    A = math:atan((Tz-Cz) / (Tx - Cx)),
    X = math:cos(A) * U,
    Z = math:sin(A) * U,
    {Cx + X, Cz + Z}.

