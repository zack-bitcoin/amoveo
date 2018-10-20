-module(amoveo_http_app).
-behaviour(application).


%% Application callbacks
-export([start/2,
         stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = amoveo_http_sup:start_link(),
    ok = load_schema(),
    ok = start_internal(),
    ok = start_external(),
    {ok, Pid}.

stop(_State) ->
    ok.

start_internal() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [{"/:file", int_file_handler, []},
                  {"/", int_handler, []}
                 ]}]),
    %Port = application:get_env(amoveo_core, internal_port, ?DEFAULT_INTERNAL_PORT),
    {ok, Port} = application:get_env(amoveo_core, internal_port),
    {ok, _} = cowboy:start_http(http_internal, 100,
                                [{ip, {127, 0, 0, 1}}, {port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

start_external() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [{"/graphql", ext_graphql_handler, []},
                  {"/:file", ext_file_handler, []},
                  {"/", ext_handler, []}
                 ]}]),
    {ok, Port} = application:get_env(amoveo_core, port),
    {ok, _} = cowboy:start_http(http, 100,
                                [{ip, {0, 0, 0, 0}}, {port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),
    % {ok, _} = cowboy:start_http(http,
    %                 [{ip, {0, 0, 0, 0}}, {port, Port}],
    %                 #{env => #{dispatch => Dispatch},
    %                   stream_handlers => [cowboy_compress_h, cowboy_stream_h],
    %                  %% Bump the default limit of 8000 to 65b536 to allow us to submit
    %                  %% slightly larger, human readable, query documents. The limit of
    %                  %% 65536 is chosen to allow us to have 8 times bigger documents
    %                  %% than the default where we hit the limit of 8000. If you are
    %                  %% hitting the bumped limit you should probably consider splitting
    %                  %% up your query document into two.
    %                  %%
    %                  %% Caveat: If you are testing on localhost you might not see the
    %                  %% max limit have any effect since the socket might make the entire
    %                  %% HTTP request available when cowboy does a gen_tcp:read(Socket, 0)
    %                  %% and will ignore the limit.
    %                  max_request_line_length => 65536,
    %
    %                  %% Bump the default limit of 4096 on Header lengths to 16384. The
    %                  %% problem is we will eventually get a very large document as a
    %                  %% referrer from GraphiQL and this will break the server side as it
    %                  %% has to process through that header
    %                  max_header_value_length => 16384
    %                  }
    %                 ),
    ok.

mapping_rules() ->
    #{
       interfaces => #{ default => graphql_type },
       objects => #{
         'Block' => graphql_block,
         default => graphql_object }
     }.

load_schema() ->
    {ok, SchemaFile} = application:get_env(amoveo_http, schema_file),
    PrivDir = code:priv_dir(amoveo_http),
    {ok, SchemaData} = file:read_file(
                         filename:join(PrivDir, SchemaFile)),
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, SchemaData),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.

setup_root() ->
    Root = {root,
            #{ query => 'Query'
             }},
    ok = graphql:insert_schema_definition(Root),
    ok.
