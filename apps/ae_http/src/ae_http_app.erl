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
          [{'_', [{"/:file", ext_file_handler, []},
                  {"/", ext_handler, []}
                 ]}]),
    {ok, Port} = application:get_env(amoveo_core, port),
    {ok, _} = cowboy:start_http(http, 100,
                                [{ip, {0, 0, 0, 0}}, {port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

