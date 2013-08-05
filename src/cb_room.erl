-module(cb_room).

-behaviour(gen_server).

%% API
-export([start_link/3,
         join/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include("../include/cb.hrl").
-record(state, {roomid, mapid, owner, players=[], marines=dict:new()}).

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
start_link(OwnerPid, RoomId, MapId) ->
    gen_server:start_link(?MODULE, [OwnerPid, RoomId, MapId], []).

join(PlayerPid) ->
    gen_server:call(self(), {join, PlayerPid}).

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
init([OwnerPid, RoomId, MapId]) ->
    io:format("Create Room: roomid = ~p,  mapid = ~p~n", [RoomId, MapId]),
    {ok, #state{roomid=RoomId, mapid=MapId, owner=OwnerPid, players=[OwnerPid]}}.

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
handle_call({join, PlayerPid}, _From, #state{players=Players} = State) ->
    {reply, ok, State#state{players=[PlayerPid | Players]}};


handle_call({get_marine_owner_pid, MarineId}, _From, #state{marines=Marines} = State) ->
    Reply = dict:find(MarineId, Marines),
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
handle_cast({broadcast, Marine}, #state{players=Players} = State) ->
    io:format("cb_room broadcasting messages, players = ~p~n", [Players]),
    lists:foreach(
        fun(PlayerPid) ->
            gen_server:cast(PlayerPid, {broadcast, Marine})
        end,
        Players
        ),
    {noreply, State};


handle_cast({broadcast, Marine, IgnorePid}, #state{players=Players} = State) ->
    lists:foreach(
        fun(PlayerPid) ->
            case PlayerPid of
                IgnorePid -> ok;
                _ -> gen_server:cast(PlayerPid, {broadcast, Marine})
            end
        end,
        Players
        ),
    {noreply, State};


handle_cast({new_marine, MarineId, PlayerPid}, #state{marines=Marines} = State) ->
    {noreply, State#state{marines=dict:store(MarineId, PlayerPid, Marines)}}.

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
