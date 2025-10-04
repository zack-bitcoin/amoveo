-module(block_handler).
-include("../../amoveo_core/src/records.hrl").

-export([init/3, handle/2, terminate/3, init/2]).

init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.

init(Req0, Opts) -> handle(Req0, Opts).	
handle(Req, State) ->
    {ok, _Data, Req2} = cowboy_req:read_body(Req),
    {IP, _} = cowboy_req:peer(Req2),
%    {Start, End} = packer:unpack(Data),

    #{path := Path} = Req2,
    SE = string:slice(Path, 8),
    [Start0, End0] = string:split(SE, "_"),
    Start = binary_to_integer(Start0),
    End = binary_to_integer(End0),
    Headers = #{ <<"content-type">> => <<"application/octet-stream">>,
	       <<"Access-Control-Allow-Origin">> => <<"*">>},
    case request_frequency:doit(IP) of
        ok ->
            ReqA = cowboy_req:stream_reply(200, Req2),
            stream_blocks(Start, End, ReqA, Headers);
        _ -> 
            io:fwrite("spammer's ip: "),
            io:fwrite(packer:pack(IP)),
            io:fwrite("\n"),
            ReqB = cowboy_req:reply(200, Headers, packer:pack({ok, <<"stop spamming the server">>}), req2),
            {ok, ReqB, State}
    end.
stream_blocks(Start, Start, Req, State) ->
    %io:fwrite("stream blocks end\n"),
    Block = list_to_binary(block_db3:read_compressed(Start, Start)),
    S = size(Block),
    cowboy_req:stream_body(<<S:64, Block/binary>>, fin, Req),
    {ok, Req, State};
stream_blocks(Start, End, Req, State) ->
    true = (abs(Start - End) < 1002),
    %io:fwrite("stream blocks "),
    %io:fwrite(integer_to_list(Start)),
    %io:fwrite("\n"),
    Block = list_to_binary(block_db3:read_compressed(Start, Start)),
    S = size(Block),
    cowboy_req:stream_body(<<S:64, Block/binary>>, nofin, Req),
    timer:sleep(50),
    if
        (Start < End) ->
            stream_blocks(Start+1, End, Req, State);
        (Start > End) ->
            stream_blocks(Start-1, End, Req, State)
    end.
