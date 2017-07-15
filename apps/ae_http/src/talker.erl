-module(talker).
-export([talk/2, talk/3]).

-define(RETRY, 5).

talk(Msg, {IP, Port}) ->
    talk(Msg, build_string_peer(IP, Port));

talk(Msg, Peer) ->
    talk_helper(Msg, Peer, ?RETRY).

talk(Msg, IP, Port) ->
    talk(Msg, build_string_peer(IP, Port)).

build_string_peer(IP, Port) ->
    T = inet_parse:ntoa(IP),
    "http://" ++ T ++ ":" ++ integer_to_list(Port) ++ "/".

talk_helper(_, _, 0) ->
    io:fwrite("talk helper fail\n"),
    {error, failed_connect};
talk_helper(Msg, Peer, N) ->
    PM = packer:pack(Msg),
    %io:fwrite("talk helper msg is "),
    %io:fwrite(PM),
    %io:fwrite("\n"),
    case httpc:request(post, {Peer, [], "application/octet-stream", iolist_to_binary(PM)}, [{timeout, 1000}], []) of
        {ok, {_Status, _Headers, []}} ->
            talk_helper(Msg, Peer, N - 1);
        {ok, {_, _, R}} ->
	    io:fwrite("response was \n"),
	    io:fwrite(R),
	    io:fwrite("\n"),
            packer:unpack(R);
        {error, socket_closed_remotely} ->
            talk_helper(Msg, Peer, N - 1);
        {error, timeout} ->
            talk_helper(Msg, Peer, N - 1);
        {error, failed_connect} ->
            talk_helper(Msg, Peer, N - 1);
        {error, {failed_connect, _}} ->
            talk_helper(Msg, Peer, N - 1);
        X -> io:fwrite("talk helper unexpected"),
            io:fwrite(X),
            error
    end.
