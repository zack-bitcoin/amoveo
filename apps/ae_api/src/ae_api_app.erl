-module(ae_api_app).
-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Res = ae_api_sup:start_link(),
    ok = serve:start(),
    Res.

stop(_State) ->
    ok.
