
-module(cb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    AcceptServer = {
            cb_accept, {cb_accept, start_link, []},
            permanent, 2000, worker, [cb_accept]
            },

    PlayerSupervisor = {
            cb_player_sup, {cb_player_sup, start_link, []},
            permanent, infinity, supervisor, [cb_player_sup]
            },

    RoomManager = {
            cb_room_manager, {cb_room_manager, start_link, []},
            permanent, 2000, worker, [cb_room_manager]
            },

    RoomSupervisor = {
            cb_room_sup, {cb_room_sup, start_link, []},
            permanent, infinity, supervisor, [cb_room_sup]
            },

    Restart = {one_for_one, 5, 10},
    {ok, {Restart, [RoomManager, RoomSupervisor, PlayerSupervisor, AcceptServer]}}.

