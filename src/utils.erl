-module(utils).
-export([random_int/0,
        random_list/0,
        % marine_proto_to_record/1,
        marine_record_to_proto/1,
        marine_record_to_proto/2,
        marine_record_to_proto/3,
        make_new_marine/3,
        can_make_gun_shoot/1]).


-include("../include/cb.hrl").

random_int() ->
    <<N:24>> = crypto:strong_rand_bytes(3),
    N.

random_list() ->
    <<B/binary>> = base64:encode(crypto:strong_rand_bytes(24)),
    binary_to_list(B).


% marine_proto_to_record({marine, Id, Hp, {vector2, Cx, Cz}, Status, _, _, undefined, _}) ->
%     #marine{id=Id, hp=Hp, position=#vector2{x=Cx, z=Cz}, status=Status};

% marine_proto_to_record({marine, Id, Hp, {vector2, Cx, Cz}, Status, _, _, FlaresAmount, _}) ->
%     #marine{id=Id, hp=Hp, position=#vector2{x=Cx, z=Cz}, status=Status, flares=FlaresAmount}.


marine_record_to_proto(M) ->
    marine_record_to_proto(M, own, 'Normal').

marine_record_to_proto(M, Who) ->
    marine_record_to_proto(M, Who, 'Normal').

marine_record_to_proto(#marine{id=Id,
                               hp=Hp,
                               position=#vector2{x=Cx, z=Cz},
                               status=Status,
                               targetposition=TargetPosition,
                               flares=FlaresAmount},
                        Who,
                        Role) ->
    Tp =
    case TargetPosition of
        undefined -> undefined;
        #vector2{x=Tx, z=Tz} -> {vector2, Tx, Tz}
    end,

    case Who of
        own ->
            {marine, Id, Hp, {vector2, Cx, Cz}, Status, Tp, FlaresAmount, Role};
        others ->
            {marine, Id, Hp, {vector2, Cx, Cz}, Status, undefined, undefined, Role}
    end.

make_new_marine(Id, X, Z) ->
    #marine{id=Id, position=#vector2{x=X, z=Z}}.


can_make_gun_shoot(GunLastTime) ->
    {A, {H, M, S}} = calendar:time_difference(
        GunLastTime,
        calendar:now_to_datetime(now())
        ),
    io:format("can_make_gun_shoot, GunLastTime = ~p~n", [GunLastTime]),
    io:format("~p, ~p, ~p, ~p~n", [A, H, M, S]),

    case A of
        Day when Day > 0 -> true;
        Day when Day =:=0 -> H > 0 orelse M > 0 orelse S >= 2;
        _ -> false
    end.
