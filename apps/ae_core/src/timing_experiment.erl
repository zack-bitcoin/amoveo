-module(timing_experiment).
-export([test/0, hashtable/2, tuple/2, hashtable_read/2, tuple_read/2]).
%-define(size, 1024).
-define(size, 128).
-define(List4, ([0,0,0,0])).
-define(List16, ?List4 ++ ?List4 ++ ?List4 ++ ?List4).
-define(List64, ?List16 ++ ?List16 ++ ?List16 ++ ?List16).
-define(List256, ?List64 ++ ?List64 ++ ?List64 ++ ?List64).
-define(FullTuple, list_to_tuple(?List64 ++ ?List64)).
%list_to_tuple(?List256 ++ ?List256 ++ ?List256 ++ ?List256)).

full_hashtable(D, 0) -> D;
full_hashtable(D, X) -> 
    full_hashtable(dict:store(X, X, D), X-1).
hashtable(_, 1) -> ok;
hashtable(HT, Times) ->
    X = Times rem ?size,
    NHT = dict:store(X, X, HT),
    hashtable(NHT, Times - 1).
hashtable_read(_, 1) -> ok;
hashtable_read(HT, Times) ->
    X = Times rem ?size,
    %X = 5,
    dict:fetch(X, HT),
    hashtable_read(HT, Times - 1).
tuple(_, 1) -> ok;
tuple(Tuple, Times) ->
    X = Times rem ?size,
    NT = setelement(X, Tuple, X),
    tuple(NT, Times - 1).
tuple_read(_, 1) -> ok;
tuple_read(Tuple, Times) ->
    %X = Times rem ?size,
    %X = 5,
    %element(X, Tuple),
    tuple_read(Tuple, Times - 1).
test() ->
    H = full_hashtable(dict:new(), ?size),
    [timer:tc(timing_experiment, hashtable, [H, ?size - 1]),
    timer:tc(timing_experiment, tuple, [?FullTuple, ?size - 1]),
    timer:tc(timing_experiment, hashtable_read, [H, ?size - 1]),
   timer:tc(timing_experiment, tuple_read, [?FullTuple, ?size - 1])
].
    
