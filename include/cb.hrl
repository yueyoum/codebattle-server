-record(vector2, {x=0, z=0}).
-record(marine, {
    id,
    hp=100,
    position=#vector2{},
    status='Idle',
    targetposition,
    gunlasttime={{2013, 8, 9}, {0, 0, 0}},
    flares=10}).


-define(PLAYERNUMS, 2).
-define(MARINENUMS, 2).
-define(MAPX, 50).
-define(MAPZ, 50).