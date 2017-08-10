-module(ae_utils).

%% API
-export([tuples2lists/1,
         binary_to_file_path/2]).

%% convert tuples to lists so they can pretend json
tuples2lists(X) when is_tuple(X) ->
    tuples2lists(tuple_to_list(X));
tuples2lists([]) -> [];
tuples2lists([H|T]) ->
    [tuples2lists(H)|tuples2lists(T)];
tuples2lists(X) -> X.

binary_to_file_path(Code, Binary) ->
    Encoded = base58:binary_to_base58(Binary),
    Dir = file_dir(Code),
    Dir ++ Encoded ++ ".db".

file_dir(blocks) ->
    "blocks/";
file_dir(oracle_questions) ->
    "oracle_questions/".
