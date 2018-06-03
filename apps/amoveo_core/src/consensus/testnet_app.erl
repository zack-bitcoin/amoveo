-module(testnet_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    application:start(inets),
    inets:start(),

    sync:cron(),
    push_block:cron(),

    io:fwrite("starting testnet node"),

    testnet_sup:start_link().


stop(_State) ->
    ok.
