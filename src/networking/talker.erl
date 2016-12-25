-module(talker).
-export([talk/1, talk/3, local_talk/1]).

peer(IP, Port) ->
    %{ok, Address} = inet_parse:address(IP),
    L = size(IP),
    T = inet_parse:ntoa(IP),
    Z = case L of    
	    4 -> T;
	    8 -> "[" ++ T ++ "]"
	end,
    "http://" ++ Z ++ ":" ++ integer_to_list(Port) ++ "/".

local_talk(Msg) ->
    Peer = "http://127.0.0.1:3011/",
    talk(Msg, Peer).
talk(Msg) ->
    Peer = "http://127.0.0.1:3010/",
    talk(Msg, Peer).
talk(Msg, Peer) ->
    PM = packer:pack(Msg),
    Msg = packer:unpack(PM),
    case httpc:request(post, {Peer, [], "application/octet-stream", iolist_to_binary(PM)}, [{timeout, 1000}], []) of
	{ok, {_Status, _Headers, []}} -> 
	    talk(Msg, Peer);
	    io:fwrite("talk error 1"),
	    %io:fwrite({Status, Headers}),
	    %{error, undefined};
	{error, socket_closed_remotely} ->
	    io:fwrite("socket closed remotely \n"),
	    talk(Msg, Peer);
	{ok, {_, _, R}} -> 
	    packer:unpack(R);
	{error, timeout} ->
	    io:fwrite("talk error timeout"),
	    {error, timeout};
	{error, {failed_connect, _}} ->
	    {error, failed_connect}
		
    end.
talk(Msg, IP, Port) -> talk(Msg, peer(IP, Port)).

    
      
