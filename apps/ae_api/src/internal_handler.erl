
-module(internal_handler).

-export([init/3, handle/2, terminate/3, doit/1]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3011/", [], "application/octet-stream", packer:pack({pubkey})}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011

handle(Req, State) ->
    {ok, Data, _} = cowboy_req:body(Req),
    %io:fwrite("internal handler "),
    %io:fwrite(Data),
    %io:fwrite("\n"),
    true = is_binary(Data),
    A = packer:unpack(Data),
    B = doit(A),
    D = packer:pack(B),
    Headers = [{<<"content-type">>, <<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, D, Req),
    {ok, Req2, State}.
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
doit({Key}) ->
    {ok, easy:Key()};
doit({Key, Arg1}) ->
    {ok, easy:Key(Arg1)};
doit({Key, Arg1, Arg2}) ->
    {ok, easy:Key(Arg1, Arg2)};
doit({Key, A, B, C}) ->
    {ok, easy:Key(A, B, C)};
doit({Key, A, B, C, D}) ->
    {ok, easy:Key(A, B, C, D)};
doit({Key, A, B, C, D, E}) ->
    {ok, easy:Key(A, B, C, D, E)};
doit({Key, A, B, C, D, E, F}) ->
    {ok, easy:Key(A, B, C, D, E, F)};
doit({Key, A, B, C, D, E, F, G}) ->
    {ok, easy:Key(A, B, C, D, E, F, G)};

doit(X) ->
    io:fwrite("don't know how to handle it \n"),
    io:fwrite(packer:pack(X)),
    io:fwrite("\n"),
    {error}.
    
