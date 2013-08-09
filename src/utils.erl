-module(utils).
-export([random_int/0,
        random_list/0,
        marine_proto_to_record/1,
        marine_record_to_proto/1,
        make_new_marine/3]).


-include("../include/cb.hrl").

random_int() ->
    <<N:24>> = crypto:strong_rand_bytes(3),
    N.

random_list() ->
    <<B/binary>> = base64:encode(crypto:strong_rand_bytes(24)),
    binary_to_list(B).


marine_proto_to_record({marine, Id, Hp, {vector2, Cx, Cz}, Status, {vector2, Tx, Tz}}) ->
    #marine{id=Id, hp=Hp, position=#vector2{x=Cx, z=Cz}, status=Status,
            targetposition=#vector2{x=Tx, z=Tz}};

marine_proto_to_record({marine, Id, Hp, {vector2, Cx, Cz}, Status, undefined}) ->
    #marine{id=Id, hp=Hp, position=#vector2{x=Cx, z=Cz}, status=Status}.


marine_record_to_proto(#marine{id=Id,
                               hp=Hp,
                               position=#vector2{x=Cx, z=Cz},
                               status=Status,
                               targetposition=#vector2{x=Tx, z=Tz}}) ->
    {marine, Id, Hp, {vector2, Cx, Cz}, Status, {vector2, Tx, Tz}}.

make_new_marine(Id, X, Z) ->
    #marine{id=Id, position=#vector2{x=X, z=Z}, targetposition=#vector2{x=X, z=Z}}.