-record(vector2, {x=0, z=0}).
-record(marine, {
    id,
    hp=100,
    position=#vector2{},
    status='Idle',
    gunlasttime,
    flares}).
