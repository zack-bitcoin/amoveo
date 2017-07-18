-module(ae_http_app).
-behaviour(application).

-define(DEFAULT_PORT, 8040).
-define(DEFAULT_INTERNAL_PORT, 8041).
-define(DEFAULT_SWAGGER_PORT, 8042).

%% Application callbacks
-export([start/2,
         stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = ae_http_sup:start_link(),
    start_internal(),
    start_external(),
    start_swagger(),
    {ok, Pid}.

stop(_State) ->
    ok.

start_internal() ->
    Dispatch = 
        cowboy_router:compile(
          [{'_', [{"/:file", int_file_handler, []},
                  {"/", int_handler, []}
                 ]}]),
    Port = application:get_env(ae_core, internal_port, ?DEFAULT_INTERNAL_PORT),
    {ok, _} = cowboy:start_http(http_internal, 100, 
                                [{ip, {127, 0, 0, 1}}, {port, Port}], 
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

start_external() ->
    Dispatch = 
        cowboy_router:compile(
          [{'_', [{"/", ext_handler, []}
                 ]}]),
    Port = application:get_env(ae_core, port, ?DEFAULT_PORT),
    {ok, _} = cowboy:start_http(http, 100, 
                                [{ip, {0, 0, 0, 0}}, {port, Port}], 
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

start_swagger() ->
    Port = application:get_env(ae_core, swagger_port, ?DEFAULT_SWAGGER_PORT),
    Spec = swagger_server:child_spec(swagger, #{
                                       ip => {127, 0, 0, 1}, 
                                       port => Port,
                                       net_opts => [],
                                       logic_handler => ae_http_dispatch
                                      }),
    {ok, _} = supervisor:start_child(ae_http_sup, Spec),
    ok.
