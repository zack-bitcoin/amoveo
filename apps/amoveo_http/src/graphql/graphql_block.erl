-module(graphql_block).
-include("../../amoveo_core/src/records.hrl").

-export([execute/4]).

execute(Ctx, #block{ height = Id }, <<"id">>, _Args) ->
    {ok, Id};

execute(Ctx, #block{ height = Height }, <<"height">>, _Args) ->
    {ok, Height}.
