-module(ae_http_app).
-behaviour(application).

%-define(DEFAULT_PORT, 8040).
%-define(DEFAULT_INTERNAL_PORT, 8041).
-define(DEFAULT_SWAGGER_INTERNAL_PORT, 8042).
-define(DEFAULT_SWAGGER_EXTERNAL_PORT, 8043).

%% Application callbacks
-export([start/2,
         stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = ae_http_sup:start_link(),
    ok = start_internal(),
    ok = start_external(),
    ok = start_swagger_internal(),
    ok = start_swagger_external(),
    {ok, Pid}.

stop(_State) ->
    ok.

start_internal() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [{"/:file", int_file_handler, []},
                  {"/", int_handler, []}
                 ]}]),
    %Port = application:get_env(ae_core, internal_port, ?DEFAULT_INTERNAL_PORT),
    {ok, Port} = application:get_env(ae_core, internal_port),
    {ok, _} = cowboy:start_http(http_internal, 100,
                                [{ip, {127, 0, 0, 1}}, {port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

start_external() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [{"/:file", ext_file_handler, []},
                  {"/", ext_handler, []}
                 ]}]),
    %Port = application:get_env(ae_core, port, ?DEFAULT_PORT),
    {ok, Port} = application:get_env(ae_core, port),
    {ok, _} = cowboy:start_http(http, 100,
                                [{ip, {0, 0, 0, 0}}, {port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

start_swagger_internal() ->
    Port = application:get_env(ae_core, swagger_port_internal, ?DEFAULT_SWAGGER_INTERNAL_PORT),
    Spec = swagger_server:child_spec(swagger_int, #{
                                       ip => {127, 0, 0, 1},
                                       port => Port,
                                       net_opts => [],
                                       logic_handler => ae_http_dispatch_int
                                      }),
    {ok, _} = supervisor:start_child(ae_http_sup, Spec),
    ok.

start_swagger_external() ->
    Port = application:get_env(ae_core, swagger_port_external, ?DEFAULT_SWAGGER_EXTERNAL_PORT),
    Spec = swagger_server:child_spec(swagger_ext, #{
                                       ip => {0, 0, 0, 0},
                                       port => Port,
                                       net_opts => [],
                                       logic_handler => ae_http_dispatch_ext
                                      }),
    {ok, _} = supervisor:start_child(ae_http_sup, Spec),
    ok.
