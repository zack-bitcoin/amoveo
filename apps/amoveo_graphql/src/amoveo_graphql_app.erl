%%%-------------------------------------------------------------------
%% @doc amoveo_graphql public API
%% @end
%%%-------------------------------------------------------------------

-module(amoveo_graphql_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = amoveo_graphql_sup:start_link(),
    ok = load_schema(),
    % ok = amoveo_graphql_db:wait_for_tables(),
    {ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
%% tag::schemaMapping[]
mapping_rules() ->
    #{
       interfaces => #{ default => amoveo_graphql_type },
       objects => #{
         'Planet' => amoveo_graphql_planet,
         default => amoveo_graphql_object }
     }.
%% end::schemaMapping[]

%% tag::loadSchema[]
load_schema() ->
    {ok, SchemaFile} = application:get_env(amoveo_graphql, schema_file),
    PrivDir = code:priv_dir(amoveo_graphql),
    {ok, SchemaData} = file:read_file(
                         filename:join(PrivDir, SchemaFile)),
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, SchemaData),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.
%% end::loadSchema[]

%% tag::setupRoot[]
setup_root() ->
    Root = {root,
            #{ query => 'Query',
               interfaces => ['Node']
             }},
    ok = graphql:insert_schema_definition(Root),
    ok.
%% end::setupRoot[]
