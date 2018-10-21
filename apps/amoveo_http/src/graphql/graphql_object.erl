-module(graphql_object).
-export([execute/4]).

%% Assume we are given a map(). Look up the field in the map. If not
%% present, return the value null.
execute(Context, Object, Field, Args) ->
    io:fwrite(Field),
    io:fwrite(Object),
    {ok, maps:get(Field, Object, null)}.
