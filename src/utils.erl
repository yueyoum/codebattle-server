-module(utils).
-export([random_int/0,
        random_list/0]).


random_int() ->
    <<N:24>> = crypto:strong_rand_bytes(3),
    N.

random_list() ->
    <<B/binary>> = base64:encode(crypto:strong_rand_bytes(24)),
    binary_to_list(B).


