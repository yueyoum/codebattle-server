-module(cb_observer).

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
-record(state, {sock, room=#room{}}).

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
    {ok, #state{sock=Socket}}.

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
    {reply, ok, State}.

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

handle_cast({createmarine, Marine, Color}, #state{sock=Sock} = State) ->
    Marines = [utils:marine_record_to_proto(M) || M <- Marine],
    C =
    case Color of
        undefined -> "red";
        Co -> Co
    end,

    Msg = observer_pb:encode_message({message, createmarine,
        undefined,
        undefined,
        {createmarine, C, Marines}
        }),
    ok = gen_tcp:send(Sock, Msg),
    {noreply, State};

handle_cast({broadcast, Marine}, #state{sock=Sock} = State) ->
    ok = notify(Marine, Sock),
    {noreply, State};


handle_cast({'DOWN', _Reason}, State) ->
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
        {cmd, Cmd, Crm, Jrm, Mrt} = observer_pb:decode_cmd(Data),
        case Cmd of
            createroom -> createroom(Crm, State);
            joinroom -> joinroom(Jrm, State);
            marinereport -> marinereport(Mrt, State)
        end
    of
        NewState ->
            inet:setopts(Sock, [{active, once}]),
            {noreply, NewState}
    catch
        _Error:Reason ->
            StopReason =
            case Reason of
                {codebattle, R} -> R;
                _ -> "Invalid Message"
            end,
            {stop, StopReason, State}
    end;


handle_info({tcp_closed, _}, State) ->
    {stop, "Room Owner Lost Connection", State};

handle_info({tcp_error, _, _Reason}, State) ->
    {stop, "Room Owner Lost Connection", State};

handle_info(timeout, State) ->
    {stop, "Room Owner Lost Connection", State}.



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
            {ok, {RoomId, RoomPid, _Token}} = cb_room_manager:createroom(self(), MapId),
            cmdresponse(Sock, {createroom, {createroomresponse, RoomId, {vector2int, 50, 50}}}),
            State#state{room=#room{id=RoomId, pid=RoomPid}};
        _ ->
            cmdresponse(Sock, createroom, 1),
            State
    end.

joinroom(_Data, _State) ->
    %% NOT implemented
    throw({codebattle, "This feature not available"}).


marinereport(Report, #state{room=Room} = State) ->
    case Room#room.pid of
        undefined ->
            ok;
        RoomPid ->
            cb_room:marine_report(RoomPid, Report)
    end,
    State.



cmdresponse(Sock, Cmd, RetCode) when RetCode =/= 0 ->
    Msg = observer_pb:encode_message({message,
        cmdresponse,
        {cmdresponse, RetCode, Cmd, undefined, undefined},
        undefined,
        undefined}),
    ok = gen_tcp:send(Sock, Msg).

cmdresponse(Sock, {Cmd, Value}) ->
    Msg =
    case Cmd of
        createroom ->
            observer_pb:encode_message({message,
                cmdresponse,
                {cmdresponse, 0, Cmd, Value, undefined},
                undefined,
                undefined
                });
        joinroom ->
            observer_pb:encode_message({message,
                cmdresponse,
                {cmdresponse, 0, Cmd, undefined, Value},
                undefined,
                undefined
                })
    end,
    ok = gen_tcp:send(Sock, Msg).


notify(Marines, Sock) when is_list(Marines) ->
    Data = [utils:marine_record_to_proto(M) || M <- Marines],
    Msg = observer_pb:encode_message({message, senceupdate,
        undefined,
        {senceupdate, Data},
        undefined
        }),
    ok = gen_tcp:send(Sock, Msg);

notify(#marine{} = Marine, Sock) ->
    notify([Marine], Sock).

