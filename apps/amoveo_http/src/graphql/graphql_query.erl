-module(graphql_query).
-include("../../amoveo_core/src/records.hrl").

-export([execute/4]).

execute(Context, Object, <<"block">>, Args) ->
    {ok, #block{height = 0}}.
