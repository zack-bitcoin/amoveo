-module(ae_utils).

%% API
-export([tuples2lists/1]).

%% convert tuples to lists so they can pretend json
tuples2lists(X) when is_tuple(X) ->
    tuples2lists(tuple_to_list(X));
tuples2lists([]) -> [];
tuples2lists([H|T]) ->
    [tuples2lists(H)|tuples2lists(T)];
tuples2lists(X) -> X.