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
    
    {ok, IP} = application:get_env(amoveo_core, internal_ip),
    {ok, _} = cowboy:start_http(http_internal, 100,
                                [{ip, IP}, {port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),%,
    %{compress, true}]),
    ok.

start_external() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [{"/ext/:file", get_api_handler, []},
		  {"/:file", ext_file_handler, []},
                  {"/", ext_handler, []}
                 ]}]),
    {ok, Port} = application:get_env(amoveo_core, port),
    {ok, IP} = application:get_env(amoveo_core, external_ip),
    {ok, _} = cowboy:start_http(http, 100,
                                [{ip, IP}, {port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

