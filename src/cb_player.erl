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
-record(state, {sock, marine=#marine{}, room=#room{}}).
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
handle_cast({broadcast, #marine{}=Marine}, #state{sock=Sock} = State) ->
    updateresponse(Sock, Marine),
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

handle_info({tcp, Sock, Data}, State) when Sock =:= State#state.sock ->
    {cmd, Cmd, Crm, Jrm, Cme, Opt} = api_pb:decode_cmd(Data),
    io:format("Player receive: ~p, ~p, ~p, ~p, ~p~n", [Cmd, Crm, Jrm, Cme, Opt]),

    NewState =
    case Cmd of
        createroom -> createroom(Crm, State);
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


createroom({createroom, MapId}, #state{sock=Sock, room=Room} = State) ->
    case Room#room.id of
        undefined ->
            {ok, {RoomId, RoomPid}} = cb_room_manager:createroom(self(), MapId),
            cmdresponse(Sock, {createroom, RoomId}),
            State#state{room=#room{id=RoomId, pid=RoomPid}};
        _ ->
            cmdresponse(Sock, createroom, 1),
            State
    end.

joinroom({joinroom, RoomId}, #state{sock=Sock, room=Room} = State) ->
    case Room#room.id of
        undefined ->
            case cb_room_manager:joinroom(self(), RoomId) of
                {ok, {RoomId, RoomPid}} ->
                    cmdresponse(Sock, joinroom),
                    State#state{room=#room{id=RoomId, pid=RoomPid}};
                notfound ->
                    cmdresponse(Sock, joinroom, 5),
                    State
            end;
        _ ->
            cmdresponse(Sock, joinroom, 4),
            State
    end.



createmarine({createmarine, RoomId, Name}, #state{sock=Sock, marine=_Marine, room=Room} = State) ->
    io:format("createmarine, ReqRoomId = ~p, RoomId = ~p~n", [RoomId, Room#room.id]),
    case Room#room.id of
        undefined ->
            cmdresponse(Sock, createmarine, 2),
            State;
        RoomId ->
            MarineId = utils:random_int(),
            Marine = #marine{id=MarineId, name=Name},
            cmdresponse(Sock, {createmarine, MarineId}),
            io:format("~p, createmarine cmdresponse done~n", [?MODULE]),
            cb_room_manager:broadcast(RoomId, Marine),
            State#state{marine=Marine};
        _ ->
            cmdresponse(Sock, createmarine, 3),
            State
    end.


marineoperate(Data, State) ->
    State.



cmdresponse(Sock, Cmd, RetCode) when RetCode =/= 0 ->
    Msg = api_pb:encode_message({message,
        cmdresponse,
        {cmdresponse, RetCode, Cmd, undefined, undefined},
        undefined}),
    ok = gen_tcp:send(Sock, Msg),
    ok.

cmdresponse(Sock, joinroom) ->
    Msg = api_pb:encode_message({message,
        cmdresponse,
        {cmdresponse, 0, joinroom, undefined, undefined},
        undefined
        }),
    ok = gen_tcp:send(Sock, Msg),
    ok;

cmdresponse(Sock, {Cmd, Value}) ->
    Msg =
    case Cmd of
        createroom ->
            api_pb:encode_message({message,
                cmdresponse,
                {cmdresponse, 0, Cmd, {createroomresponse, Value}, undefined},
                undefined
                });
        createmarine ->
            api_pb:encode_message({message,
                cmdresponse,
                {cmdresponse, 0, Cmd, undefined, {createmarineresponse, Value}},
                undefined
                })
    end,
    ok = gen_tcp:send(Sock, Msg),
    ok.


updateresponse(Sock, Marine) ->
    Msg = api_pb:encode_message({message,
        senceupdate,
        undefined,
        {senceupdate, {marine,
            Marine#marine.id,
            Marine#marine.name,
            "Marine",
            100,
            Marine#marine.hp,
            {vector2, Marine#marine.position#vector2.x, Marine#marine.position#vector2.z},
            {vector2, 0.5, 0.5},
            %%Marine#marine.status
            'Run'
            }}
        }),

    ok = gen_tcp:send(Sock, Msg),
    ok.