-module(dispatch).

-export([handle_request/3]).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('AddAccount', Req, _Context) ->
    Acc = maps:get('Account', Req),
    Addr = maps:get(<<"address">>, Acc),
    Amt = maps:get(<<"amount">>, Acc),
    ok = easy:create_account(Addr, Amt),
    {200, [], #{}};

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.
