-module(talker).
-export([talk/2, talk/3, talk_timeout/3]).

-define(RETRY, 3).

talk_timeout(Msg, {IP, Port}, X) ->
    P = build_string_peer(IP, Port),
    talk_helper(Msg, P, ?RETRY, X).

talk(Msg, {IP, Port}) ->
    talk(Msg, build_string_peer(IP, Port));

talk(Msg, Peer) ->
    talk_helper(Msg, Peer, ?RETRY, 1200000).

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
check_print(S) ->
    case sync_mode:check() of
	normal -> ok;
	quick -> io:fwrite(S)
    end.
	    
talk_helper(_, _, 0, _) ->
    check_print("talk helper fail\n"),
    bad_peer;
    %{error, failed_connect};
talk_helper(Msg, Peer, N, TimeOut) ->
    PM = packer:pack(Msg),
    %check_print("sending message "),
    %check_print(PM),
    %check_print("\n"),
    %timer:sleep(500),
    Msg = packer:unpack(PM),
    case httpc:request(post, {Peer, [], "application/octet-stream", iolist_to_binary(PM)}, [{timeout, TimeOut}], []) of
        {ok, {{_, 500, _}, _Headers, []}} ->
	    check_print("server crashed. Will ignore peer. "),
	    check_print(element(1, Msg)),
	    check_print(" \n"),
	    bad_peer;
            %talk_helper(Msg, Peer, 0, TimeOut);
        {ok, {Status, _Headers, []}} ->
            check_print("talk_helper weird response. Attempting to reconnect. \n"),
	    check_print(packer:pack(Status)),
            talk_helper(Msg, Peer, N - 1, TimeOut);
        {ok, {_, _, R}} ->
	    %check_print("talker peer is "),
	    %check_print(Peer),
	    %check_print("\n"),
	    %check_print("talker msg is "),
	    %check_print(packer:pack(Msg)),
	    %check_print("\n"),
	    %check_print("talker response is "),
	    %check_print(R),
	    %check_print("\n"),
	    DoubleOK = packer:pack({ok, ok}),
	    if
		R == DoubleOK -> 0;
		true ->
		    packer:unpack(R)
	    end;
        {error, socket_closed_remotely} ->
            %check_print("talk_helper socket closed remotely. attempting to reconnect \n"),
            talk_helper(Msg, Peer, N - 1, TimeOut);
        {error, timeout} ->
            check_print("talk_helper timeout. attempting to reconnect \n"),
	    check_print(element(1, Msg)),
	    check_print("\n"),
            talk_helper(Msg, Peer, N - 1, TimeOut);
        {error, failed_connect} ->
            check_print("talk_helper failed_connect 0. will ignore this peer.  \n"),
	    bad_peer;
            %talk_helper(Msg, Peer, N - 1, TimeOut);
        {error, {failed_connect, _}} ->
            %check_print("talk_helper failed_connect 1. will ignore this peer. \n"),
	    %check_print(PM),
	    bad_peer;
            %talk_helper(Msg, Peer, N - 1, TimeOut);
        X -> check_print("talk helper unexpected error"),
            check_print(X),
            error
    end.
