-module(serve).
-export([start/0]).

-define(DEFAULT_PORT, 8040).

start() ->
    io:fwrite("start server\n"),

    D_internal = [
	 {'_', [
		{"/:file", main_handler, []},
		{"/", internal_handler, []}
	       ]}
	],
    D = [
	 {'_', [
		%{"/:file", external_handler, []},% we don't know if this could be used to inject code and read parts of the database that shouldn't be read.
		{"/", handler, []}
	       ]}
	],
    Dispatch_internal = cowboy_router:compile(D_internal),
    Dispatch = cowboy_router:compile(D),
    K_internal = [
	 {env, [{dispatch, Dispatch_internal}]}
	],
    K = [
	 {env, [{dispatch, Dispatch}]}
	],

    Port = application:get_env(ae_core, port, ?DEFAULT_PORT),

    %{ok, _} = cowboy:start_http(http_internal, 100, [{ip, {127,0,0,1}},{port, Port+1}], K_internal),
    {ok, _} = cowboy:start_http(http_internal, 100, [{ip, {127,0,0,1}},{port, Port+1}], K_internal),
    {ok, _} = cowboy:start_http(http, 100, [{ip, {0,0,0,0}},{port, Port}], K),
	ok.
