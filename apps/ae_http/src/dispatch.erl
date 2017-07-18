-module(dispatch).

-export([handle_request/3]).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('AddAccount', Req, _Context) ->
    Acc = maps:get('Account', Req),
    Pub = maps:get(<<"pubkey">>, Acc),
    Amt = maps:get(<<"amount">>, Acc),
    ok = api:create_account(base64:decode(Pub), Amt),
    {200, [], #{}};

handle_request('GetKeyPair', _Req, _Context) ->
    {Pub, Priv} = api:new_keypair(),
    {200, [], #{
            <<"public">> => base64:encode(Pub),
            <<"private">> => base64:encode(Priv)
           }};

handle_request('GetTop', _Req, _Context) ->
    {top, Hash, Height} = api:top(),
    {200, [], #{
            <<"hash">> => base64:encode(Hash),
            <<"height">> => Height
           }};

handle_request('AddPeer', Req, _Context) ->
    Peer = maps:get('Peer', Req),
    Port = maps:get(<<"port">>, Peer),
    IP = maps:get(<<"ip">>, Peer),
    case inet_parse:address(binary_to_list(IP)) of
        {ok, IP1} -> 
            0 = api:add_peer(IP1, Port),
            {200, [], #{}};
        Err -> 
            lager:error("Failed to parse peer IP ~p: ~p", [IP, Err]),
            {405, [], #{}}
    end;

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.
