-module(ai).

-behaviour(gen_server).

%% API
-export([start_link/1,
         own_marine_ids/1,
         other_marine_ids/1,
         call_move/4,
         call_flares/2,
         call_gunshoot/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-define(TIMEOUT, 1000 * 10).

-record(vector2, {x=0, z=0}).
-record(marine, {
    id,
    hp,
    position=#vector2{},
    status,
    gunlasttime={{2013, 8, 10}, {0, 0, 0}},
    flares,
    role}).
-record(state, {sdk, mapx, mapz, started=false, own=dict:new(), others=dict:new()}).

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

own_marine_ids(Pid) ->
    gen_server:call(Pid, own_marine_ids).

other_marine_ids(Pid) ->
    gen_server:call(Pid, other_marine_ids).

call_move(Pid, MarineId, X, Z) ->
    gen_server:call(Pid, {move, MarineId, X, Z}).

call_flares(Pid, MarineId) ->
    gen_server:call(Pid, {flares, MarineId}).

call_gunshoot(Pid, MarineId, X, Z) ->
    gen_server:call(Pid, {gunshoot, MarineId, X, Z}).

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
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed({A, B, C}),
    {ok, SdkPid} = ai_sdk:start_link(self()),
    IP = {127, 0, 0, 1},
    Port = 8888,

    {ok, _Sock} = ai_sdk:connect(SdkPid, IP, Port),

    %% an ai must join a room before any action,
    %% so It's ok that we do this in init function.
    ok = ai_sdk:joinroom(SdkPid, RoomId),
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

% handle_call(_Request, _From, State) ->
%     {reply, ok, State}.

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
    {reply, Reply, State};

handle_call({gunshoot, MarineId, X, Z}, _From, #state{sdk=Sdk, own=Own} = State) ->
    Reply = 
    case dict:is_key(MarineId, Own) of
        true ->
            ai_sdk:marineoperate(Sdk, MarineId, 'GunAttack', X, Z);
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
        RM = make_new_marine_record(M),
        dict:store(RM#marine.id, RM, D)
    end,
    Own = lists:foldl(Fun, dict:new(), Marines),
    io:format("joinroomresponse, Marines = ~p~n", [Own]),
    {noreply, State#state{mapx=X, mapz=Z, own=Own}};


handle_cast({senceupdate, Marine}, State) ->
    NewState = action(Marine, State),
    % Timeout = (random:uniform(3) + random:uniform(3)) * 1000,
    {noreply, NewState};

handle_cast(startbattle, State) ->
    % Timeout = (random:uniform(5) + random:uniform(5)) * 1000,
    {noreply, State#state{started=true}}.


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

handle_info(timeout, #state{sdk=Sdk, own=Own} = State) ->
    % io:format("ai timeout, start Flares~n"),
    % Id = lists:nth(1, dict:fetch_keys(Own)),
    % flares(Sdk, Id),
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


action(Marines, #state{sdk=Sdk, own=Own, others=Others} = State) ->
    UpdateFun = fun({marine, Id, _, _, _, _, _, _} = M) ->
        case dict:is_key(Id, Own) of
            true ->
                update_marine_record(M, dict:fetch(Id, Own));
            false ->
                case dict:is_key(Id, Others) of
                    true ->
                        update_marine_record(M, dict:fetch(Id, Others));
                    false ->
                        make_new_marine_record(M)
                end
        end
    end,

    Ms = [UpdateFun(M) || M <- Marines],

    FunMyOwn = fun(M) -> dict:is_key(M#marine.id, Own) end,

    OwnMarines = lists:filter(FunMyOwn, Ms),
    OthersMarines = Marines -- OwnMarines,

    UpdateOwn = fun(M, D) -> dict:store(M#marine.id, M, D) end,
    NewOwn = lists:foldl(UpdateOwn, Own, OwnMarines),

    MyMarines = [V || {_, V} <- dict:to_list(NewOwn)],


    %% If got OthersMarines here, 
    %% means either you have Flares just now,
    %% or it should be others doing Flares, GunAttack,
    %% or others has been hitted,
    %% or others has hitted any other marines.
    io:format("Own ids = ~p~n", [dict:fetch_keys(Own)]),

    % case length(OthersMarines) of
    %     0 -> action_no_others(MyMarines, State);
    %     _ ->
    %         case lists:any(fun(T) -> is_flares(T) end, MyMarines) of
    %             true ->
    %                 action_after_by_own_flares(MyMarines, OthersMarines, State);
    %             false ->
    %                 case lists:any(fun(T) -> is_flares(T) end, OthersMarines) of
    %                     true ->
    %                         action_after_others_flares(MyMarines, lists:nth(1, OthersMarines), State);
    %                     false ->
    %                         case lists:any(fun(T) -> is_gunattack(T) end, OthersMarines) of
    %                             true ->
    %                                 action_after_others_shoot(MyMarines, lists:nth(1, OthersMarines), State);
    %                             false ->
    %                                 action_after_bullet_hitted(MyMarines, OthersMarines, State)
    %                         end
    %                 end
    %         end
    % end.
    State.


action_no_others(OwnMarines, State) ->
    io:format("No OthersMarines~n"),
    State.

action_after_by_own_flares(OwnMarines, OthersMarines, #state{sdk=Sdk} = State) ->
    %% My Marines Flares, then I got all Marines in the battle.
    io:format("Got OthersMarines, due to I have Flares just now~n"),

    Target = choose_marine_random(OthersMarines),
    Fun = fun(#marine{id=Id}) ->
        gun_attack_and_run(Sdk,
                           Id,
                           Target#marine.position#vector2.x,
                           Target#marine.position#vector2.z,
                           random:uniform(50),
                           random:uniform(50))
    end,

    lists:foreach(Fun, OwnMarines),
    State.

action_after_others_flares(OwnMarines, OthersMarine, #state{sdk=Sdk} = State) ->
    %% Other Marines Flares, So He knew my state
    io:format("Got OthersMarines, Other Flares, OwnMarines = ~p~n", [OwnMarines]),
    Target = OthersMarine,
    Fun = fun(#marine{id=Id}) ->
        gun_attack_and_run(Sdk,
                           Id,
                           Target#marine.position#vector2.x,
                           Target#marine.position#vector2.z,
                           random:uniform(50),
                           random:uniform(50))
    end,

    lists:foreach(Fun, OwnMarines),
    State.


action_after_others_shoot(OwnMarines, OthersMarine, #state{sdk=Sdk} = State) ->
    %% Other Marines Shoot, He didn't know My state right now.
    io:format("Got OthersMarines, Other GunAttack~n"),
    Target = OthersMarine,
    Fun = fun(#marine{id=Id}) ->
        gun_attack_and_run(Sdk,
                           Id,
                           Target#marine.position#vector2.x,
                           Target#marine.position#vector2.z,
                           random:uniform(50),
                           random:uniform(50))
    end,

    lists:foreach(Fun, OwnMarines),
    State.

action_after_bullet_hitted(OwnMarines, OthersMarines, State) ->
    %% Buttle has hitted some marine.
    io:format("Got OthersMarines, Bullet Hitted~n"),
    State.


is_flares(#marine{status=Status}) ->
    Status == 'Flares'.

is_gunattack(#marine{status=Status}) ->
    Status == 'GunAttack'.

is_initiative(M) ->
    is_flares(M) orelse is_gunattack(M).

choose_marine_random(Ms) ->
    lists:nth(random:uniform(length(Ms)), Ms).

choose_marine_by_hp(Ms) ->
    Fun = fun(M1, M2) -> M1#marine.hp =< M2#marine.hp end,
    HpMs = lists:sort(Fun, Ms),
    lists:nth(1, HpMs).


gun_attack(Sdk, Id, X, Z) ->
    ok = ai_sdk:marineoperate(Sdk, Id, 'GunAttack', X, Z).

move(Sdk, Id, X, Z) ->
    ok = ai_sdk:marineoperate(Sdk, Id, 'Run', X, Z).

flares(Sdk, Id) ->
    ok = ai_sdk:marineoperate(Sdk, Id, 'Flares').


gun_attack_and_run(Sdk, Id, Gx, Gz, Rx, Rz) ->
    gun_attack(Sdk, Id, Gx, Gz),
    move(Sdk, Id, Rx, Rz).


update_marine_record({marine, Id, Hp, {vector2, X, Z}, Status, _, FlaresAmount, Role}, M) ->
    M#marine{hp=Hp, position=#vector2{x=X, z=Z}, status=Status, flares=FlaresAmount, role=Role}.

make_new_marine_record({marine, Id, Hp, {vector2, X, Z}, Status, _, FlaresAmount, Role}) ->
    #marine{id=Id, hp=Hp, position=#vector2{x=X, z=Z}, status=Status, flares=FlaresAmount, role=Role}.