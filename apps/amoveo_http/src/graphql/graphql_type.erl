-module(graphql_type).
-include("../../amoveo_core/src/records.hrl").

-export([execute/1]).

%% tag::resolveType[]
execute(#block{}) -> {ok, 'Block'};
execute(_Otherwise) -> {error, unknown_type}.
%% end::resolveType[]
