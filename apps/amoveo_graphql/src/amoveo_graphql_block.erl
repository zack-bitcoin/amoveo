-module(amoveo_graphql_block).
-include("../../amoveo_core/src/records.hrl").

-export([execute/4]).

execute(_Ctx, #block { height = BlockHeight } = Block, Field, Args) ->
    case Field of
        <<"id">> -> {ok, amoveo_graphql_id:encode({'Block', Block#block.height})}
        % <<"name">> -> {ok, Block#block.name}
    end.

integer(I) when is_integer(I) -> I;
integer(nan) -> null.
