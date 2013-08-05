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

-record(state, {sdk, mapx, mapz, ownids=sets:new(), own=dict:new(), others=dict:new()}).

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
handle_cast({joinroomresponse, _RoomId, {vector2int, X, Z}}, #state{sdk=Sdk} = State) ->
    io:format("get joinroomresponse, x = ~p, z = ~p~n", [X, Z]),
    ok = ai_sdk:createmarine(Sdk, "AiBot", 25, 25),
    {noreply, State#state{mapx=X, mapz=Z}};

handle_cast({createmarineresponse, MarineId}, #state{ownids=OwnIds} = State) ->
    io:format("get createmarineresponse, MarineId = ~p~n", [MarineId]),
    {noreply, State#state{ownids=sets:add_element(MarineId, OwnIds)}};

handle_cast({senceupdate, Marine}, #state{ownids=OwnIds, own=Own, others=Others} = State) ->
    io:format("get senceupdate~n"),
    {marine, Id, _, _, HpMax, Hp, {vector2, X, Z}, Status, _} = Marine,
    NewState =
    case sets:is_element(Id, OwnIds) of
        true ->
            update_own_marine(Id, HpMax, Hp, X, Z, Status, State);
        false ->
            other_marine_action(Marine, State)
    end,
    {noreply, NewState}.


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
terminate(_Reason, State) ->
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


update_own_marine(Id, HpMax, Hp, X, Z, Status, #state{own=Own} = State) ->
    NewOwn = dict:store(
        Id,
        #marine{id=Id, hpmax=HpMax, hp=Hp, position=#vector2{x=X, z=Z}, status=Status},
        Own
        ),

    State#state{own=NewOwn}.

other_marine_action(
    {marine, Id, _, _, HpMax, Hp, {vector2, X, Z}, Status, {vector2, Tx, Tz}},
    #state{sdk=Sdk, own=Own, others=Others} = State) ->

    %% update others first
    NewOthers = dict:store(
        Id,
        #marine{id=Id, hpmax=HpMax, hp=Hp, position=#vector2{x=X, z=Z}, status=Status, targetposition=#vector2{x=Tx, z=Tz}},
        Others
        ),

    %% just run to this marine
    Fun = fun(K, _V) ->
        ok = ai_sdk:marineoperate(Sdk, K, 'Run', X, Z)
    end,

    dict:map(Fun, Own),

    State#state{others=NewOthers}.