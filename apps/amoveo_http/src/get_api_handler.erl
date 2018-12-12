-module(get_api_handler).
-export([init/3, handle/2, terminate/3]).
%example of talking to this handler:
%curl -i http://localhost:3011/ext/getmoneysupply

handle(Req, _) ->
    {F, _} = cowboy_req:path(Req),
    X = doit(F),
    Headers = [{<<"content-type">>, <<"text/html">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, X, Req),
    {ok, Req2, 0}.
init(_Type, Req, _Opts) -> {ok, Req, []}.
terminate(_Reason, _Req, _State) -> ok.
doit(<<"/ext/getmoneysupply">>) ->
    integer_to_binary(element(16, block:top()));
doit(X) -> 
    io:fwrite(X),
    <<"error">>.
    
