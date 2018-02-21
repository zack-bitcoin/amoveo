-module(amoveo_core_compile).
-export([doit/2]).
doit(X, Y) when is_list(Y) ->
    doit(X, list_to_binary(Y));
doit(F, Front) ->
    {ok, Text} = file:read_file(F),
    compiler_chalang:doit(<<Front/binary, Text/binary>>).
