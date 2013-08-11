-module(cb_room_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
         createroom/2,
         joinroom/2,
         joinroom/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include("../include/cb.hrl").

-record(room, {owner, pid, token}).

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
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

createroom(PlayerPid, MapId) ->
    gen_server:call(?MODULE, {createroom, {PlayerPid, MapId}}).


joinroom(PlayerPid, RoomId) ->
    joinroom(PlayerPid, RoomId, undefined).

joinroom(PlayerPid, RoomId, Token) ->
    gen_server:call(?MODULE, {joinroom, {PlayerPid, RoomId, Token}}).



% broadcast(RoomId, #marine{} = Marine) ->
%     gen_server:call(?MODULE, {broadcast, {RoomId, Marine}}).



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
init([]) ->
    {ok, dict:new()}.

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
handle_call({createroom, {PlayerPid, MapId}}, _From, State) ->
    RoomId = utils:random_int(),
    Token = utils:random_list(),
    {ok, RoomPid} = cb_room_sup:create_room(PlayerPid, RoomId, MapId),
    {reply, {ok, {RoomId, RoomPid, Token}}, dict:store(RoomId, #room{owner=PlayerPid, pid=RoomPid, token=Token}, State)};


handle_call({joinroom, {PlayerPid, RoomId, Token}}, _From, State) ->
    Reply =
    case dict:find(RoomId, State) of
        {ok, #room{pid=RoomPid, token=Token}} ->
            ok = gen_server:call(RoomPid, {join, ob, PlayerPid}),
            {ok, {RoomId, RoomPid}};
        {ok, #room{pid=RoomPid}} ->
            case gen_server:call(RoomPid, {join, ai, PlayerPid}) of
                ok -> {ok, {RoomId, RoomPid}};
                full -> full
            end;
        error ->
            notfound
    end,
    {reply, Reply, State}.


% handle_call({broadcast, {RoomId, Marine}}, _From, State) ->
%     {ok, #room{pid=RoomPid}} = dict:find(RoomId, State),
%     gen_server:cast(RoomPid, {broadcast, Marine}),
%     {reply, ok, State}.


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
handle_cast(_Msg, State) ->
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
