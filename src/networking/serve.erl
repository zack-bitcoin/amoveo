-module(serve).
-export([start/0, start/1, pw/0, pw/1]).
start() -> start(port:check()).
start(Port) ->
  io:fwrite("start server\n"),

  D_internal = [
    {'_', [
      {"/:file", main_handler, []},
      {"/", internal_handler, []}
    ]}
  ],
  D = [
    {'_', [
      {"/:file", external_handler, []},
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
  %{ok, _} = cowboy:start_http(http_internal, 100, [{ip, {127,0,0,1}},{port, Port+1}], K_internal),
  {ok, _} = cowboy:start_http(http_internal, 100, [{ip, {0,0,0,0}},{port, Port+1}], K_internal),
  {ok, _} = cowboy:start_http(http, 100, [{ip, {0,0,0,0}},{port, Port}], K),
  % EXTERNAL trusted aeternity peers
  peers:add([46,101,103,165], 8080),
  peers:add([52, 36, 106, 100], 8080),
  % INTERNAL trusted aeternity peers
  peers:add([127, 0, 0, 1], 8040),
  peers:add([127, 0, 0, 1], 3010),
  peers:add([127, 0, 0, 1], 3020),
  peers:add([127, 0, 0, 1], 3030),
  % show peers
  peers:all(),
  %sync now
  easy:sync(),
  %start mining
  mine:start().

pw() ->  start(port:check()).
pw(X) ->
  port:change(X),
  pw().