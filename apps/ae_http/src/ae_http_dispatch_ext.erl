-module(ae_http_dispatch_ext).

-export([handle_request/3]).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('GetHeader', Req, _Context) ->
    Data = maps:get('HeaderId', Req),
    BlockId = maps:get(<<"block-id">>, Data),
    Header = block:block_to_header(block:read_int(BlockId)),
    {200, [], #{<<"header">> => base64:encode(Header),
                <<"block-id">> => BlockId}};

handle_request('GetHeaders', Req, _Context) ->
    Data = maps:get('HeaderIds', Req),
    BlockIds = [maps:get(<<"block-id">>, B) || B <- Data],
    Resp = [#{<<"header">> => base64:encode(block:block_to_header(block:read_int(B))),
              <<"block-id">> => B} || B <- BlockIds],
    {200, [], Resp};

handle_request('ChannelSync', #{'ChannelSync' := Data}, _Context) ->
    Pub = maps:get(<<"pubkey">>, Data),
    Sig = maps:get(<<"sig">>, Data),
    Pub1 = base64:decode(Pub),
    Sig1 = base64:decode(Sig),
    Res = channel_feeder:update_to_me(Sig1, Pub1),
    io:format("XXX Res = ~p~n", [Res]),
    {200, [], #{}};

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.
