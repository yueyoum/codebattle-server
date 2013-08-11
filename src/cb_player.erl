-module(cb_player).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../include/cb.hrl").

-record(room, {id, pid}).
-record(state, {sock, marine=dict:new(), room=#room{}, startbattle=false}).
-define(TIMEOUT, 1000 * 600).

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
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


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
init([Socket]) ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed({A, B, C}),
    {ok, #state{sock=Socket}, ?TIMEOUT}.

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
handle_call(all_marines, _From, #state{marine=MyMarines} = State) ->
    Ms = [V || {_, V} <- dict:to_list(MyMarines)],
    {reply, Ms, State}.

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

handle_cast({broadcast, Marine}, #state{sock=Sock, marine=MyMarines} = State) ->
    MyMarineList = [V || {_, V} <- dict:to_list(MyMarines)],
    ok = notify(MyMarineList, Marine, Sock),
    {noreply, State};

handle_cast({broadcast, Role, Marine}, #state{sock=Sock, marine=MyMarines} = State) ->
    case dict:is_key(Marine#marine.id, MyMarines) of
        true -> 
            Data = utils:marine_record_to_proto(Marine, own, Role),
            notify_send(Data, [], Sock);
        false ->
            Data = utils:marine_record_to_proto(Marine, others, Role),
            notify_send([], Data, Sock)
    end,
    {noreply, State};



handle_cast({report, 
    {marinereport,
     toidle,
     {marinestatus, Id, 'Idle', {vector2, X, Z}},
     _,
     _,
     _,
     _}},
     #state{sock=Sock, marine=MyMarines} = State) ->

    NewState =
    case dict:is_key(Id, MyMarines) of
        false -> 
            State;
        true ->
            M = dict:fetch(Id, MyMarines),
            NewM = M#marine{position=#vector2{x=X, z=Z}, status='Idle'},
            ok = notify([NewM], [], Sock),
            State#state{marine=dict:store(Id, NewM, MyMarines)}
    end,
    {noreply, NewState};


handle_cast({report,
    {marinereport,
     damage,
     _,
     {marinestatus, Id1, Status1, {vector2, X1, Z1}},
     {marinestatus, Id2, Status2, {vector2, X2, Z2}},
     _,
     _}},
     #state{marine=MyMarines, room=Room} = State) ->

    NewState =
    case dict:is_key(Id2, MyMarines) of
        false ->
            case dict:is_key(Id1, MyMarines) of
                false ->
                    State;
                true ->
                    M = dict:fetch(Id1, MyMarines),
                    NewM = M#marine{status=Status1, position=#vector2{x=X1, z=Z1}},
                    gen_server:cast(Room#room.pid, {broadcast, 'Attacker', NewM}),
                    State#state{marine = dict:store(Id1, NewM, MyMarines)}
            end;
        true ->
            M = dict:fetch(Id2, MyMarines),
            Hp = M#marine.hp - 10,
            NewM = 
            case Hp > 0 of
                true ->
                    M#marine{position=#vector2{x=X2, z=Z2}, hp=Hp, status=Status2};
                false ->
                    M#marine{position=#vector2{x=X2, z=Z2}, hp=0, status='Dead'}
            end,

            gen_server:cast(Room#room.pid, {broadcast, 'Injured', NewM}),
            gen_server:cast(Room#room.pid, {to_observer, NewM}),
            State#state{marine = dict:store(Id2, NewM, MyMarines)}
    end,

    case check_alive(NewState#state.marine) of
        true -> {noreply, NewState};
        false -> {stop, dead, NewState}
    end;


handle_cast({report,
    {marinereport,
     Report,
     _,
     _,
     _,
     MarineId,
     Marines}},
     #state{marine=MyMarines, room=Room} = State)
     when Report =:= flares; Report =:= flares2 ; Report =:= gunattack ->

    UpdateOwn = fun({marinestatus, Id, Status, {vector2, X, Z}}, D) ->
        case dict:is_key(Id, D) of
            false -> D;
            true ->
                Old = dict:fetch(Id, D),
                New = Old#marine{status=Status, position=#vector2{x=X, z=Z}},
                dict:store(Id, New, D)
        end
    end,

    NewMyMarines = lists:foldl(UpdateOwn, MyMarines, Marines),

    %% update self first, and then if MarineId belongs to me,
    %% send all marines state to self.
    %% otherwise do nothing.

    case Report of
        flares ->
            case dict:is_key(MarineId, NewMyMarines) of
                false -> ok;
                true -> gen_server:cast(Room#room.pid, {flares_report, self(), dict:fetch(MarineId, NewMyMarines)})
            end;
        flares2 ->
            case dict:is_key(MarineId, NewMyMarines) of
                false -> ok;
                true -> gen_server:cast(Room#room.pid, {flares2_report, self()})
            end;
        gunattack ->
            case dict:is_key(MarineId, NewMyMarines) of
                false -> ok;
                true -> gen_server:cast(Room#room.pid, {gunattack_report, self(), dict:fetch(MarineId, NewMyMarines)})
            end
    end,

    {noreply, State#state{marine=NewMyMarines}};




handle_cast(startbattle, #state{sock=Sock} = State) ->
    Msg = api_pb:encode_message({message, startbattle, undefined, undefined, undefined}),
    ok = gen_tcp:send(Sock, Msg),
    {noreply, State#state{startbattle=true}};


handle_cast({'DOWN', Reason}, #state{sock=Sock} = State) ->
    io:format("player receive DOWN message, Reason = ~p~n", [Reason]),
    Win =
    case Reason of
        dead -> true;
        _ -> false
    end,
    ReasonString =
    case Reason of
        R when is_atom(R) -> atom_to_list(R);
        R when is_list(R) -> R
    end,
    Msg = api_pb:encode_message({message,
        endbattle,
        undefined,
        undefined,
        {endbattle, ReasonString, Win}
        }),
    gen_tcp:send(Sock, Msg),
    {stop, normal, State}.



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

handle_info({tcp, Sock, Data}, State) when Sock =:= State#state.sock ->
    try
        {cmd, Cmd, Jrm, Cme, Opt} = api_pb:decode_cmd(Data),
        case Cmd of
            joinroom -> joinroom(Jrm, State);
            createmarine -> createmarine(Cme, State);
            marineoperate -> marineoperate(Opt, State)
        end
    of
        NewState ->
            inet:setopts(Sock, [{active, once}]),
            {noreply, NewState, ?TIMEOUT}
    catch
        _Error:Reason ->
            {Ope, ErrorCode} =
            case Reason of
                {codebattle, C} -> {undefined, C};
                {codebattle, Operate, C}  -> {Operate, C};
                _ -> {undefined, 10}
            end,
            cmdresponse(Ope, ErrorCode),
            {noreply, State, ?TIMEOUT}
    end;

handle_info({tcp_closed, _}, State) ->
    {stop, "Some Player Lost Connection", State};

handle_info({tcp_error, _, _Reason}, State) ->
    {stop, "Some Player Lost Connection", State};

handle_info(timeout, State) ->
    {stop, "Some Player Lost Connection", State}.



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
terminate(_Reason, State) ->
    gen_tcp:close(State#state.sock),
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

joinroom({joinroom, RoomId, Color}, #state{sock=Sock, room=Room} = State) ->
    case Room#room.id of
        undefined ->
            case cb_room_manager:joinroom(self(), RoomId) of
                {ok, {RoomId, RoomPid}} ->
                    RandomMarines = create_random_marines_record(?MARINENUMS, ?MAPX, ?MAPZ),
                    cmdresponse(Sock, {joinroom, {joinroomresponse, 
                                                  RoomId,
                                                  {vector2int, ?MAPX, ?MAPZ},
                                                  [utils:marine_record_to_proto(M) || M <- RandomMarines]}}),

                    Fun = fun(M, D) -> dict:store(M#marine.id, M, D) end,
                    MyMarines = lists:foldl(Fun, dict:new(), RandomMarines),
                    gen_server:cast(RoomPid, {to_observer, createmarine, RandomMarines, Color}),

                    State#state{marine=MyMarines, room=#room{id=RoomId, pid=RoomPid}};
                full ->
                    cmdresponse(Sock, joinroom, 15),
                    State;
                notfound ->
                    cmdresponse(Sock, joinroom, 14),
                    State
            end;
        _ ->
            cmdresponse(Sock, joinroom, 13),
            State
    end.



createmarine(_, State) ->
    throw({codebattle, 11}),
    State.


marineoperate(_, #state{sock=Sock, startbattle=false} = State) ->
    cmdresponse(Sock, marineoperate, 25),
    State;

marineoperate({marineoperate, Id, Status, TargetPosition},
    #state{sock=Sock, marine=OwnMarines, room=Room, startbattle=true} = State) ->
    case Room#room.pid of
        undefined ->
            cmdresponse(Sock, marineoperate, 19),
            State;
        RoomPid ->
            case dict:find(Id, OwnMarines) of
                {ok, M} ->
                    case marine_status_check(M, Status) of
                        {ok, NewM} ->
                            NewTargetPosition =
                            case TargetPosition of
                                undefined -> undefined;
                                {vector2, Tx, Tz} when Tx >= 0, Tx =< ?MAPX, Tz >= 0, Tz =< ?MAPZ-> 
                                    #vector2{x=Tx, z=Tz};
                                _ -> throw({codebattle, marineoperate, 24})
                            end,
                            NewM2 = NewM#marine{status=Status, targetposition=NewTargetPosition},
                            gen_server:cast(RoomPid, {to_observer, NewM2}),
                            State#state{marine=dict:store(Id, NewM2, OwnMarines)};
                        {error, ErrorCode} ->
                            cmdresponse(Sock, marineoperate, ErrorCode),
                            State
                    end;
                error ->
                    cmdresponse(Sock, marineoperate, 20),
                    State
            end
    end.


marine_status_check(#marine{status='Dead'}, _) ->
    {error, 21};

marine_status_check(#marine{gunlasttime=T} = M, 'GunAttack') ->
    case utils:can_make_gun_shoot(T) of
        false -> {error, 22};
        true -> {ok, M#marine{gunlasttime=calendar:now_to_datetime(now())}}
    end;

marine_status_check(#marine{flares=FlaresAmount} = M, 'Flares') ->
    case FlaresAmount > 0 of
        false -> {error, 23};
        true -> {ok, M#marine{flares=FlaresAmount-1}}
    end;

marine_status_check(M, _) ->
    {ok, M}.


cmdresponse(Sock, Cmd, RetCode) when RetCode =/= 0 ->
    Msg = api_pb:encode_message({message,
        cmdresponse,
        {cmdresponse, RetCode, Cmd, undefined, undefined},
        undefined,
        undefined}),
    ok = gen_tcp:send(Sock, Msg),
    ok.

cmdresponse(Sock, {Cmd, Value}) ->
    Msg =
    case Cmd of
        joinroom ->
            api_pb:encode_message({message,
                cmdresponse,
                {cmdresponse, 0, Cmd, Value, undefined},
                undefined,
                undefined
                })
    end,
    ok = gen_tcp:send(Sock, Msg),
    ok.


check_alive(Marines) ->
    lists:any(
        fun(#marine{hp=Hp}) -> Hp > 0 end,
        [V || {_, V} <- dict:to_list(Marines)]
        ).


notify(MyMarineList, Marines, Sock) when is_list(Marines) ->
    OthersData = [utils:marine_record_to_proto(M, others) || M <- Marines],
    MyData = [utils:marine_record_to_proto(M) || M <- MyMarineList],
    notify_send(MyData, OthersData, Sock);


notify(MyMarines, #marine{} = Marine, Sock) when is_list(MyMarines) ->
    notify(MyMarines, [Marine], Sock).

notify_send(OwnData, OthersData, Sock) ->
    Msg = api_pb:encode_message({message, senceupdate,
        undefined,
        {senceupdate, OwnData, OthersData},
        undefined
        }),
    ok = gen_tcp:send(Sock, Msg).


create_random_marines_record(Num, Xmax, Zmax) ->
    [utils:make_new_marine(
        utils:random_int(),
        random:uniform(Xmax),
        random:uniform(Zmax)) || _ <- lists:seq(1, Num)].
