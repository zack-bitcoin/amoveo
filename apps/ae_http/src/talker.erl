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
    %io:fwrite("sending message "),
    %io:fwrite(PM),
    %io:fwrite("\n"),
    Msg = packer:unpack(PM),
    case httpc:request(post, {Peer, [], "application/octet-stream", iolist_to_binary(PM)}, [{timeout, 20000}], []) of
        {ok, {Status, _Headers, []}} ->
            io:fwrite("talk_helper weird response \n"),
            talk_helper(Msg, Peer, N - 1);
        {ok, {_, _, R}} ->
            packer:unpack(R);
        {error, socket_closed_remotely} ->
            io:fwrite("talk_helper socket closed remotely \n"),
            talk_helper(Msg, Peer, N - 1);
        {error, timeout} ->
            io:fwrite("talk_helper timeout \n"),
            talk_helper(Msg, Peer, N - 1);
        {error, failed_connect} ->
            io:fwrite("talk_helper failed_connect 0 \n"),
            talk_helper(Msg, Peer, N - 1);
        {error, {failed_connect, _}} ->
            io:fwrite("talk_helper failed_connect 1 \n"),
            talk_helper(Msg, Peer, N - 1);
        X -> io:fwrite("talk helper unexpected"),
            io:fwrite(X),
            error
    end.
