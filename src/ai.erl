-module(ai).

-behaviour(gen_server).

%% API
-export([start_link/1,
         createmarine/3,
         own_marine_ids/1,
         other_marine_ids/1,
         move/4,
         flares/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../include/cb.hrl").

-record(state, {sdk, manager, mapx, mapz, own=dict:new(), others=dict:new()}).

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
start_link(RoomId) ->
    gen_server:start_link(?MODULE, [RoomId], []).


createmarine(Pid, X, Z) ->
    gen_server:call(Pid, {createmarine, X, Z}).

own_marine_ids(Pid) ->
    gen_server:call(Pid, own_marine_ids).

other_marine_ids(Pid) ->
    gen_server:call(Pid, other_marine_ids).

move(Pid, MarineId, X, Z) ->
    gen_server:call(Pid, {move, MarineId, X, Z}).

flares(Pid, MarineId) ->
    gen_server:call(Pid, {flares, MarineId}).


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
init([RoomId]) ->
    {ok, SdkPid} = ai_sdk:start_link(self()),
    % {ok, StateManagerPid} = ai_state_manager:start_link(),
    IP = {127, 0, 0, 1},
    Port = 8888,

    {ok, _Sock} = ai_sdk:connect(SdkPid, IP, Port),

    %% an ai must join a room before any action,
    %% so It's ok that we do this in init function.
    ok = ai_sdk:joinroom(SdkPid, RoomId),
    % {ok, #state{sdk=SdkPid, manager=StateManagerPid}}.
    {ok, #state{sdk=SdkPid}}.


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

handle_call({createmarine, X, Z}, _From, #state{sdk=Sdk} = State) ->
    ok = ai_sdk:createmarine(Sdk, X, Z),
    {reply, ok, State};

handle_call(own_marine_ids, _From, #state{own=Own} = State) ->
    {reply, dict:fetch_keys(Own), State};

handle_call(other_marine_ids, _From, #state{others=Others} = State) ->
    {reply, dict:fetch_keys(Others), State};


handle_call({move, MarineId, X, Z}, _From, #state{sdk=Sdk, own=Own} = State) ->
    Reply =
    case dict:is_key(MarineId, Own) of
        true ->
            ai_sdk:marineoperate(Sdk, MarineId, 'Run', X, Z);
        false ->
            not_found_this_marine
    end,
    {reply, Reply, State};

handle_call({flares, MarineId}, _From, #state{sdk=Sdk, own=Own} = State) ->
    Reply = 
    case dict:is_key(MarineId, Own) of
        true ->
            ai_sdk:marineoperate(Sdk, MarineId, 'Flares');
        false ->
            not_found_this_marine
    end,
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
handle_cast({joinroomresponse, _RoomId, {vector2int, X, Z}, Marines}, State) ->
    Fun = fun(M, D) ->
        RM = utils:marine_proto_to_record(M),
        dict:store(RM#marine.id, RM, D)
    end,
    Own = lists:foldl(Fun, dict:new(), Marines),
    {noreply, State#state{mapx=X, mapz=Z, own=Own}};

handle_cast({createmarineresponse, Marine}, #state{own=Own} = State) ->
    io:format("createmarineresponse, Marine = ~p~n", [Marine]),
    M = utils:marine_proto_to_record(Marine),
    {noreply, State#state{own=dict:store(M#marine.id, M, Own)}};

handle_cast({senceupdate, Marine}, State) when is_list(Marine) ->
    Fun = fun(M, S) -> update_marine(M, S) end,
    NewState = lists:foldl(Fun, State, Marine),
    {noreply, NewState}.


% handle_cast({ai_state_report, Id, Cx, Cz, Tx, Tz}, #state{own=Own, others=Others} = State) ->
%     io:format("ai_state_report: ~p, ~p ~p, ~p, ~p ~n", [Id, Cx, Cz, Tx, Tz]),
%     Marine =
%     case dict:find(Id, Own) of
%         {ok, M} ->
%             {marine, Id, M#marine.name, M#marine.tag, M#marine.hpmax, M#marine.hp,
%             {vector2, Cx, Cz}, M#marine.status, {vector2, Tx, Tz}, M#marine.targetid
%             };
%         error ->
%             case dict:find(Id, Others) of
%                 {ok, M} ->
%                     {marine, Id, M#marine.name, M#marine.tag, M#marine.hpmax, M#marine.hp,
%                     {vector2, Cx, Cz}, M#marine.status, {vector2, Tx, Tz}, M#marine.targetid
%                     };
%                 error ->
%                     throw("ai_state_report id not found")
%             end
%     end,
%     gen_server:cast(self(), {senceupdate, Marine}),
%     {noreply, State}.



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
terminate(_Reason, _State) ->
    io:format("AI terminate, Reason = ~p~n", [_Reason]),
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

update_marine({marine, Id, _, _, _, _} = M, #state{own=Own} = State) ->
    case dict:is_key(Id, Own) of
        true ->
            update_own_marine(M, State);
        false ->
            update_others_marine(M, State)
    end.




update_own_marine({marine, Id, _, _, _, _} = M, #state{own=Own} = State) ->
    NewOwn = dict:store(Id, utils:marine_proto_to_record(M), Own),
    State#state{own=NewOwn}.


update_others_marine(
    {marine, Id, Hp, {vector2, Cx, Cz}, Status, {vector2, Tx, Tz}} = Marine,
    #state{sdk=Sdk, own=Own, others=Others} = State) ->

    NewOthers = dict:store(Id, utils:marine_proto_to_record(Marine), Others),
    %% ai_state_manager:ai_state(Mng, Id, Status, Cx, Cz, Tx, Tz),

    %% just run to this marine
    % Fun = fun(K, _V) ->
    %     ok = ai_sdk:marineoperate(Sdk, K, 'Run', Cx, Cz)
    % end,
    % dict:map(Fun, Own),

    State#state{others=NewOthers}.
