-record(vector2, {x=0, z=0}).
-record(marine, {
    id,
    name,
    tag="OwnMarine",
    hpmax=100,
    hp=100,
    position=#vector2{},
    status='Idle',
    targetposition=#vector2{}}).
