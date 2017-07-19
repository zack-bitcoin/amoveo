-module(ae_http_dispatch_int).

-export([handle_request/3]).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('AddAccount', Req, _Context) ->
    Data = maps:get('CreateAccount', Req),
    Pub = maps:get(<<"pubkey">>, Data),
    Amt = maps:get(<<"amount">>, Data),
    ok = api:create_account(base64:decode(Pub), Amt),
    {200, [], #{}};

handle_request('DeleteAccount', Req, _Context) ->
    Data = maps:get('PubKey', Req),
    Pub = maps:get(<<"pubkey">>, Data),
    ok = api:delete_account(base64:decode(Pub)),
    {200, [], #{}};

handle_request('RepoAccount', Req, _Context) ->
    Data = maps:get('PubKey', Req),
    Pub = maps:get(<<"pubkey">>, Data),
    ok = api:repo_account(base64:decode(Pub)),
    {200, [], #{}};

handle_request('FetchAccount', Req, _Context) ->
    Data = maps:get('PubKey', Req),
    Pub = maps:get(<<"pubkey">>, Data),
    case api:account(base64:decode(Pub)) of
        empty -> 
            {404, [], #{}};
        Acc ->
            JSON = #{
              <<"pubkey">> => base64:encode(accounts:pubkey(Acc)),
              <<"balance">> => accounts:balance(Acc),
              <<"nonce">> => accounts:nonce(Acc),
              <<"height">> => accounts:height(Acc),
              <<"bets">> => accounts:bets(Acc),
              <<"shares">> => accounts:shares(Acc)
             },
            {200, [], JSON}
    end;

handle_request('CreateKeyPair', _Req, _Context) ->
    {Pub, Priv} = api:new_keypair(),
    {200, [], #{
            <<"public">> => base64:encode(Pub),
            <<"private">> => base64:encode(Priv)
           }};

handle_request('SetKeyPair', Req, _Context) ->
    Data = maps:get('SetKeyPair', Req),
    Pub = maps:get(<<"public">>, Data),
    Priv = maps:get(<<"private">>, Data),
    Pub1 = base64:decode(Pub),
    Priv1 = base64:decode(Priv),
    BrainWallet = maps:get(<<"brain-wallet">>, Data),
    ok = api:load_key(Pub1, Priv1, BrainWallet),
    {200, [], #{}};

handle_request('FetchPubKey', _Req, _Context) ->
    Pub = api:pubkey(),
    {200, [], #{<<"pubkey">> => base64:encode(Pub)}};

handle_request('GetTop', _Req, _Context) ->
    {top, Hash, Height} = api:top(),
    {200, [], #{
            <<"hash">> => base64:encode(Hash),
            <<"height">> => Height
           }};

handle_request('AddPeer', Req, _Context) ->
    Data = maps:get('Peer', Req),
    Port = maps:get(<<"port">>, Data),
    IP = maps:get(<<"ip">>, Data),
    case inet_parse:address(binary_to_list(IP)) of
        {ok, IP1} -> 
            0 = api:add_peer(erlang:tuple_to_list(IP1), Port),
            {200, [], #{}};
        Err -> 
            lager:error("Failed to parse peer IP ~p: ~p", [IP, Err]),
            {405, [], #{}}
    end;

handle_request('Spend', Req, _Context) ->
    Data = maps:get('Spend', Req),
    Pub = maps:get(<<"pubkey">>, Data),
    Amt = maps:get(<<"amount">>, Data),
    ok = api:spend(base64:decode(Pub), Amt),
    {200, [], #{}};

handle_request('NewChannelWithServer', Req, _Context) ->
    Data = maps:get('NewChannelWithServer', Req),
    IP = maps:get(<<"ip">>, Data),
    Port = maps:get(<<"port">>, Data),
    CID = maps:get(<<"channel-id">>, Data),
    Bal = maps:get(<<"balance">>, Data),
    Limit = maps:get(<<"receive-limit">>, Data),
    Fee = maps:get(<<"fee">>, Data),
    Delay = maps:get(<<"delay">>, Data),
    case inet_parse:address(binary_to_list(IP)) of
        {ok, IP1} -> 
            ok = api:new_channel_with_server(IP1, Port, CID, Bal, Limit, Fee, Delay),
            {200, [], #{}};
        Err -> 
            lager:error("Failed to parse IP ~p: ~p", [IP, Err]),
            {405, [], #{}}
    end;

handle_request('ChannelSpend', Req, _Context) ->
    Data = maps:get('ChannelSpend', Req),
    IP = maps:get(<<"ip">>, Data),
    Port = maps:get(<<"port">>, Data),
    Amt = maps:get(<<"amount">>, Data),
    case inet_parse:address(binary_to_list(IP)) of
        {ok, IP1} -> 
            ok = api:channel_spend(IP1, Port, Amt),
            {200, [], #{}};
        Err -> 
            lager:error("Failed to parse IP ~p: ~p", [IP, Err]),
            {405, [], #{}}
    end;

handle_request('LightningSpend', Req, _Context) ->
    Data = maps:get('LightningSpend', Req),
    IP = maps:get(<<"ip">>, Data),
    Port = maps:get(<<"port">>, Data),
    Pub = maps:get(<<"pubkey">>, Data),
    Amt = maps:get(<<"amount">>, Data),
    Fee = maps:get(<<"fee">>, Data),
    case inet_parse:address(binary_to_list(IP)) of
        {ok, IP1} -> 
            ok = api:lightning_spend(IP1, Port, base64:decode(Pub), Amt, Fee),
            {200, [], #{}};
        Err -> 
            lager:error("Failed to parse IP ~p: ~p", [IP, Err]),
            {405, [], #{}}
    end;

handle_request('PullChannelState', Req, _Context) ->
    Data = maps:get('PullChannelState', Req),
    IP = maps:get(<<"ip">>, Data),
    Port = maps:get(<<"port">>, Data),
    case inet_parse:address(binary_to_list(IP)) of
        {ok, IP1} -> 
            ok = api:pull_channel_state(IP1, Port),
            {200, [], #{}};
        Err -> 
            lager:error("Failed to parse peer IP ~p: ~p", [IP, Err]),
            {405, [], #{}}
    end;

handle_request('Sync', Req, _Context) ->
    Sync = maps:get('Sync', Req),
    Ip = maps:get(<<"ip">>, Sync),
    Port = maps:get(<<"port">>, Sync),
    case inet_parse:address(binary_to_list(Ip)) of
        {ok, IpAddress} ->
            IPSyncFormat = erlang:tuple_to_list(IpAddress),
            ok = api:sync(IPSyncFormat, Port),
            {200, [], #{}};
        {error, einval} ->
            lager:error("Failed to parse IP: ~p", [Ip]),
            {405, [], #{<<"error">> => <<"Invalid IP">>}}
    end;

handle_request('MineBlock', Req, _Context) ->
    MineBlocks = maps:get('MineBlock', Req),
    Count = maps:get(<<"count">>, MineBlocks),
    Times = maps:get(<<"times">>, MineBlocks),
    ok = api:mine_block(Count, Times),
    {200, [], #{}};

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.
