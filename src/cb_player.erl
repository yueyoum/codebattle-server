-module(cb_player).

-behaviour(gen_server).

%% API
-export([start_link/1,
         notify/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../include/cb.hrl").

-record(room, {id, pid}).
-record(state, {sock, marine=dict:new(), room=#room{}, ai=true}).
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
    io:format("New Player~n"),
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
handle_cast({broadcast, _}, #state{ai=true} = State) ->
    {noreply, State};

handle_cast({broadcast, #marine{}=Marine}, #state{sock=Sock, marine=MyMarines, ai=false} = State) ->
    ok = notify(Marine, Sock),
    {noreply, State};


handle_cast({marineoperate, Marine}, #state{sock=Sock} = State) ->
    ok = notify(Marine, Sock),
    {noreply, State};


handle_cast({marinereport, {marineoperate, Id, Status, {vector2, Cx, Cz}, Tp, _Tm}},
    #state{sock=Sock, marine=MyMarines, room=Room} = State) ->
    M = dict:fetch(Id, MyMarines),

    NewTp =
    case Tp of
        undefined -> M#marine.position;
        {vector2, Tx, Tz} -> #vector2{x=Tx, z=Tz}
    end,
    NewM = M#marine{position=#vector2{x=Cx, z=Cz}, status=Status, targetposition=NewTp},

    gen_server:cast(Room#room.pid, {broadcast, NewM}),
    {noreply, State#state{marine=dict:store(Id, NewM, MyMarines)}}.





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
    {cmd, Cmd, Crm, Jrm, Cme, Opt} = api_pb:decode_cmd(Data),
    io:format("Player receive: ~p, ~p, ~p, ~p, ~p~n", [Cmd, Crm, Jrm, Cme, Opt]),

    NewState =
    case Cmd of
        createroom -> createroom(Crm, State);
        marinereport -> marinereport(Opt, State);
        ready -> ready(State);
        joinroom -> joinroom(Jrm, State);
        createmarine -> createmarine(Cme, State);
        marineoperate -> marineoperate(Opt, State)
    end,

    inet:setopts(Sock, [{active, once}]),
    {noreply, NewState, ?TIMEOUT};


handle_info({tcp_closed, _}, State) ->
    io:format("tcp closed~n"),
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    io:format("tcp error, reason: ~p~n", [Reason]),
    {stop, Reason, State};

handle_info(timeout, State) ->
    io:format("timeout..."),
    {stop, normal, State}.



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

ready(State) ->
    State.

createroom({createroom, MapId}, #state{sock=Sock, room=Room} = State) ->
    case Room#room.id of
        undefined ->
            {ok, {RoomId, RoomPid, Token}} = cb_room_manager:createroom(self(), MapId),
            cmdresponse(Sock, {createroom, {createroomresponse, RoomId, Token, {vector2int, 50, 50}}}),
            State#state{room=#room{id=RoomId, pid=RoomPid}, ai=false};
        _ ->
            cmdresponse(Sock, createroom, 1),
            State
    end.

joinroom({joinroom, RoomId, Token}, #state{sock=Sock, room=Room} = State) ->
    case Room#room.id of
        undefined ->
            case cb_room_manager:joinroom(self(), RoomId, Token) of
                {ok, {PlayerType, RoomId, RoomPid}} ->
                    cmdresponse(Sock, {joinroom, {joinroomresponse, RoomId, {vector2int, 50, 50}, []}}),
                    AI =
                    case PlayerType of
                        unity3d -> false;
                        ai -> true
                    end,
                    State#state{room=#room{id=RoomId, pid=RoomPid}, ai=AI};
                notfound ->
                    cmdresponse(Sock, joinroom, 3),
                    State
            end;
        _ ->
            cmdresponse(Sock, joinroom, 2),
            State
    end.



createmarine({createmarine, RoomId, {vector2, X, Z}}, #state{sock=Sock, marine=OwnMarines, room=Room, ai=true} = State) ->
    case Room#room.id of
        undefined ->
            cmdresponse(Sock, createmarine, 5),
            State;
        RoomId ->
            MarineId = utils:random_int(),
            Marine = utils:make_new_marine(MarineId, X, Z),
            cmdresponse(Sock, {createmarine, {createmarineresponse, utils:marine_record_to_proto(Marine)}}),
            % gen_server:cast(Room#room.pid, {new_marine, MarineId, self()}),
            gen_server:cast(Room#room.pid, {broadcast, Marine}),
            State#state{marine=dict:store(MarineId, Marine, OwnMarines)};
        _ ->
            cmdresponse(Sock, createmarine, 6),
            State
    end.


marineoperate({marineoperate, Id, Status, undefined, TargetPosition},
    #state{sock=Sock, marine=OwnMarines, room=Room, ai=true} = State) ->
    case Room#room.pid of
        undefined ->
            cmdresponse(Sock, marineoperate, 10),
            State;
        RoomPid ->
            case dict:find(Id, OwnMarines) of
                {ok, M} ->
                    NewTargetPosition =
                    case TargetPosition of
                        undefined -> M#marine.position;
                        {vector2, Tx, Tz} -> #vector2{x=Tx, z=Tz}
                    end,
                    NewM = M#marine{status=Status, targetposition=NewTargetPosition},
                    gen_server:cast(RoomPid, {broadcast, NewM}),
                    marine_action(RoomPid, NewM, Sock),
                    State#state{marine=dict:store(Id, NewM, OwnMarines)};
                error ->
                    cmdresponse(Sock, marineoperate, 11),
                    State
            end
    end.


marinereport({marineoperate, Id, _, _, _, _} = MarineReport, 
    #state{sock=Sock, room=Room, ai=false} = State) ->
    case Room#room.pid of
        undefined ->
            cmdresponse(Sock, marinereport, 10);
        RoomPid ->
            {ok, ThisMarineOwnerPid} = gen_server:call(RoomPid, {get_marine_owner_pid, Id}),
            gen_server:cast(ThisMarineOwnerPid, {marinereport, MarineReport})
    end,
    State.


cmdresponse(Sock, Cmd, RetCode) when RetCode =/= 0 ->
    Msg = api_pb:encode_message({message,
        cmdresponse,
        {cmdresponse, RetCode, Cmd, undefined, undefined, undefined},
        undefined}),
    ok = gen_tcp:send(Sock, Msg),
    ok.

cmdresponse(Sock, {Cmd, Value}) ->
    Msg =
    case Cmd of
        createroom ->
            api_pb:encode_message({message,
                cmdresponse,
                {cmdresponse, 0, Cmd, Value, undefined, undefined},
                undefined
                });
        joinroom ->
            api_pb:encode_message({message,
                cmdresponse,
                {cmdresponse, 0, Cmd, undefined, Value, undefined},
                undefined
                });
        createmarine ->
            api_pb:encode_message({message,
                cmdresponse,
                {cmdresponse, 0, Cmd, undefined, undefined, Value},
                undefined
                })
    end,
    ok = gen_tcp:send(Sock, Msg),
    ok.



is_own_marine(Id, MyMarines) ->
    dict:is_key(Id, MyMarines).



marine_action(_, #marine{status=Status}, _) when Status =:= 'Idle'; Status =:= 'Run'; Status =:= 'Dead' ->
    ok;

marine_action(RoomPid, M, Sock) ->
    cb_room:marine_action(RoomPid, M, Sock).


notify(Marines, Sock) when is_list(Marines) ->
    Data = [utils:marine_record_to_proto(M) || M <- Marines],
    Msg = api_pb:encode_message({message, senceupdate,
        undefined,
        {senceupdate, Data}
        }),
    ok = gen_tcp:send(Sock, Msg),
    ok;

notify(#marine{} = Marine, Sock) ->
    notify([Marine], Sock).
