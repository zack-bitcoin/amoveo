-module(int_file_handler).

-export([init/3, handle/2, terminate/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3011/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011
handle(Req, _) ->
    {F, _} = cowboy_req:path(Req),
    PrivDir0 = 
	case application:get_env(amoveo_core, kind) of
	    {ok, "production"} ->
		code:priv_dir(amoveo_http);
	    _ -> "../../../../apps/amoveo_http/priv"
	end,
    PrivDir = list_to_binary(PrivDir0),
    io:fwrite(PrivDir),
%/Users/zack/Hacking/amoveo/_build/prod/rel/amoveo_core/lib/amoveo_http-0.1.0/priv
    io:fwrite("\n"),
    File = << PrivDir/binary, <<"/web">>/binary, F/binary>>,
    {ok, _Data, _} = cowboy_req:body(Req),
    Headers = [{<<"content-type">>, <<"text/html">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    Text = read_file(File),
    {ok, Req2} = cowboy_req:reply(200, Headers, Text, Req),
    {ok, Req2, File}.
read_file(F) ->
    {ok, File } = file:open(F, [read, binary, raw]),
    {ok, O} =file:pread(File, 0, filelib:file_size(F)),
    file:close(File),
    O.
init(_Type, Req, _Opts) -> {ok, Req, []}.
terminate(_Reason, _Req, _State) -> ok.
