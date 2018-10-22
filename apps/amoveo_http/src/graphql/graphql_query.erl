-module(graphql_query).
-include("../../amoveo_core/src/records.hrl").

-export([execute/4]).

execute(_, _, <<"block">>, #{ <<"height">> := Height }) ->
    B = block:get_by_height(Height),
    {ok, B}.
