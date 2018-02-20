-module(talker).
-export([talk/2, talk/3, talk_timeout/3]).

-define(RETRY, 3).

talk_timeout(Msg, {IP, Port}, X) ->
    P = build_string_peer(IP, Port),
    talk_helper(Msg, P, ?RETRY, X).

talk(Msg, {IP, Port}) ->
    talk(Msg, build_string_peer(IP, Port));

talk(Msg, Peer) ->
    talk_helper(Msg, Peer, ?RETRY, 20000).

talk(Msg, IP, Port) ->
    talk(Msg, build_string_peer(IP, Port)).
ip2string2(X) ->
    (integer_to_list(X)) ++ (".").
ip2string([A,B,C,D]) ->
    ip2string({A,B,C,D});
ip2string({A,B,C,D}) ->
    ip2string2(A) ++ 
	ip2string2(B) ++ 
	ip2string2(C) ++ 
	integer_to_list(D).
build_string_peer(IP, Port) ->
    T = ip2string(IP),
    P = integer_to_list(Port),
    "http://" ++ T ++ ":" ++ P ++ "/".

talk_helper(_, _, 0, _) ->
    io:fwrite("talk helper fail\n"),
    {error, failed_connect};
talk_helper(Msg, Peer, N, TimeOut) ->
    PM = packer:pack(Msg),
    %io:fwrite("sending message "),
    %io:fwrite(PM),
    %io:fwrite("\n"),
    Msg = packer:unpack(PM),
    case httpc:request(post, {Peer, [], "application/octet-stream", iolist_to_binary(PM)}, [{timeout, TimeOut}], []) of
        {ok, {{_, 500, _}, _Headers, []}} ->
	    io:fwrite("server crashed.\n"),
            talk_helper(Msg, Peer, 0, TimeOut);
        {ok, {Status, _Headers, []}} ->
            io:fwrite("talk_helper weird response \n"),
	    io:fwrite(packer:pack(Status)),
            talk_helper(Msg, Peer, N - 1, TimeOut);
        {ok, {_, _, R}} ->
            packer:unpack(R);
        {error, socket_closed_remotely} ->
            io:fwrite("talk_helper socket closed remotely \n"),
            talk_helper(Msg, Peer, 0, TimeOut);
        {error, timeout} ->
            io:fwrite("talk_helper timeout \n"),
            talk_helper(Msg, Peer, N - 1, TimeOut);
        {error, failed_connect} ->
            io:fwrite("talk_helper failed_connect 0 \n"),
            talk_helper(Msg, Peer, N - 1, TimeOut);
        {error, {failed_connect, _}} ->
            io:fwrite("talk_helper failed_connect 1 \n"),
            %talk_helper(Msg, Peer, N - 1);
	    bad_peer;
        X -> io:fwrite("talk helper unexpected"),
            io:fwrite(X),
            error
    end.
