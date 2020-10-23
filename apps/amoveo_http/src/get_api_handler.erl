-module(get_api_handler).
-export([init/3, init/2, handle/2, terminate/3, doit/1]).
%example of talking to this handler:
%curl -i http://localhost:3010/ext/getmoneysupply

init(Req, Opts) ->
	handle(Req, Opts).
handle(Req, _) ->
    %{F, _} = cowboy_req:path(Req),
    F = cowboy_req:path(Req),
    X = doit(F),
    Headers = #{ 
      %<<"content-type">> => <<"application/octet-stream">>,
      <<"content-type">> => <<"text/html">>,
      <<"Access-Control-Allow-Origin">> => <<"*">>},
    %Headers = [{<<"content-type">>, <<"text/html">>},
    %{<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, X, Req),
    {ok, Req2, 0}.
init(_Type, Req, _Opts) -> {ok, Req, []}.
terminate(_Reason, _Req, _State) -> ok.
binary_reverse(X) ->
    list_to_binary(lists:reverse(binary_to_list(X))).
doit(<<"/ext/getmoneysupply">>) ->
    X = integer_to_binary(element(16, block:top())),
    Y = binary_reverse(X),
    case Y of
	<<B:64, D/binary>>  ->
	    RB = binary_reverse(<<B:64>>),
	    RD = binary_reverse(D),
	    <<RD/binary, <<".">>/binary, RB/binary>>;
	_ -> <<"small">>
		 end;
doit(X) -> 
    io:fwrite(X),
    <<"error">>.
    
