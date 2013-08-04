-module(ai).

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

-record(state, {sock, roomid, own=#marine{}, others=dict:new()}).

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
    IP = {127, 0, 0, 1},
    Port = 8888,
    {ok, Sock} = gen_tcp:connect(IP, Port,
            [binary, inet, {reuseaddr, true}, {active, once}, {nodelay, true}, {packet, 4}]),
    gen_server:cast(self(), {joinroom, RoomId}),
    {ok, #state{sock=Sock, roomid=RoomId}}.

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
handle_cast({joinroom, RoomId}, #state{sock=Sock} = State) ->
    Request = api_pb:encode_cmd({cmd, joinroom,
        undefined,
        {joinroom, RoomId},
        undefined,
        undefined
        }),
    gen_tcp:send(Sock, Request),
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

handle_info({tcp, Sock, Data}, State) ->
    io:format("Got Server Data~n"),
    Reply =
    case parse_data(Data, State) of
        {ok, NewState} -> {noreply, NewState};
        error -> {stop, error_code, State}
    end,
    inet:setopts(Sock, [{active, once}]),
    Reply;


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


parse_data(Data, State) ->
    {message, MsgType, CmdResponse, SenceUpdate} = api_pb:decode_message(Data),
    case MsgType of
        cmdresponse -> cmdresponse(CmdResponse, State);
        senceupdate -> senceupdate(SenceUpdate, State)
    end.


cmdresponse({cmdresponse, 0, Cmd, _, CreateMarineResponse} = Data, State) ->
    case Cmd of
        joinroom ->
            cmd_create_marine(State),
            {ok, State};
        createmarine ->
            {createmarineresponse, MarineId} = CreateMarineResponse,
            #state{sock=_, own=Own, others=_} = State,
            {ok, State#state{own=Own#marine{id=MarineId}}};
        marineoperate ->
            {ok, State}
    end;

cmdresponse({cmdresponse, Ret, Cmd, _, _} = Data, State) ->
    io:format("Cmd ~p Error, Error Code = ~p~n", [Cmd, Ret]),
    error.

senceupdate({senceupdate, Marine} = Data, State) ->
    {ok, State}.



cmd_create_marine(#state{sock=Sock, roomid=RoomId} = State) ->
    Cmd = api_pb:encode_cmd({cmd, createmarine,
        undefined,
        undefined,
        {createmarine, RoomId, "AIbot"},
        undefined
        }),

    ok = gen_tcp:send(Sock, Cmd),
    ok.
