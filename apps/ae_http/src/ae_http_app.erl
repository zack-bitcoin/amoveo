-module(ae_http_app).
-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Res = ae_http_sup:start_link(),
    ok = serve:start(),
    Res.

stop(_State) ->
    ok.
