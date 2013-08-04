-module(ai).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../include/cd.hrl").

-record(state, {sock, own=#marine{}, others=dict:new()}).

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
    gen_server:start_link(?MODULE, [], []).

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
    IP = {127, 0, 0, 0},
    Port = 8888,
    {ok, Sock} = gen_tcp:connect(IP, Port,
            [binary, inet, {reuseaddr, true}, {active, once}, {nodelay, true}, {packet, 4}]),
    gen_server:cast(self(), init),
    {ok, #state{sock=Sock}}.

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
handle_cast(init, #state{sock=Sock} = State) ->
    Request = initrequest_pb:encode_initrequest(initrequest, {<<"AIbot">>}),
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

handle_info({tcp, Sock, Data}, #state{times=T} = State) when T =:= 0 ->
    % receive InitResponse
    io:format("InitResponse Data = ~p~n", [Data]),
    {initresponse, 0, Sence} = api_pb:decode_initresponse(Data),
    {sence, Width, Height, OwnMarine, OthersMarine} = Sence,
    SenceSize = #vector2{x=Width, z=Height},
    Own = proto_marine_to_record(OwnMarine),
    Others = [proto_marine_to_record(M) || M <- OthersMarine],

    MoveCmd = cmd_move(Own#marine.id, 10, -10),
    gen_tcp:send(Sock, MoveCmd),
    {noreply, State#state{times=T+1, sencesize=SenceSize, own=Own, others=Others}};


handle_info({tcp, Sock, Data}, State) ->
    io:format("receive data~n"),
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



proto_marine_to_record({marine, Id, _, _, _, Hp, {vector2, Px, Pz}, {vector2, Rx, Rz}, Status}) ->
    #marine{id=Id, hp=Hp, position=#vector2{x=Px, z=Pz}, rotation=#vector2{x=Rx, z=Rz}, status=Status}.


cmd_move(id, x, z) ->
    api_pb:encode_cmd({cmd, id, 'Run', {vector2, x, z}}).

