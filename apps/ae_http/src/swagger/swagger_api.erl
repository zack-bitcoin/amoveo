-module(swagger_api).

-export([request_params/1]).
-export([request_param_info/2]).
-export([populate_request/3]).
-export([validate_response/4]).

-type operation_id() :: atom().
-type request_param() :: atom().

-export_type([operation_id/0]).

-spec request_params(OperationID :: operation_id()) -> [Param :: request_param()].


request_params('ChannelSync') ->
    [
        'ChannelSync'
    ];

request_params('GetHeader') ->
    [
        'HeaderId'
    ];

request_params('GetHeaders') ->
    [
        'HeaderIds'
    ];


request_params('AddAccount') ->
    [
        'CreateAccount'
    ];

request_params('AddPeer') ->
    [
        'Peer'
    ];

request_params('AddSecret') ->
    [
        'Secret'
    ];

request_params('ChannelBalance') ->
    [
    ];

request_params('ChannelSoloClose') ->
    [
    ];

request_params('ChannelSpend') ->
    [
        'ChannelSpend'
    ];

request_params('ChannelTimeout') ->
    [
    ];

request_params('CreateKeyPair') ->
    [
    ];

request_params('DeleteAccount') ->
    [
        'PubKey'
    ];

request_params('FetchAccount') ->
    [
        'PubKey'
    ];

request_params('FetchKeyPair') ->
    [
    ];

request_params('FetchPubKey') ->
    [
    ];

request_params('GetTop') ->
    [
    ];

request_params('LightningSpend') ->
    [
        'LightningSpend'
    ];

request_params('MineBlock') ->
    [
        'MineBlock'
    ];

request_params('NewChannelWithServer') ->
    [
        'NewChannelWithServer'
    ];

request_params('PullChannelState') ->
    [
        'PullChannelState'
    ];

request_params('RepoAccount') ->
    [
        'PubKey'
    ];

request_params('SetKeyPair') ->
    [
        'SetKeyPair'
    ];

request_params('Spend') ->
    [
        'Spend'
    ];

request_params('Sync') ->
    [
        'Sync'
    ];

request_params(_) ->
    error(unknown_operation).

-type rule() ::
    {type, 'binary'} |
    {type, 'integer'} |
    {type, 'float'} |
    {type, 'binary'} |
    {type, 'boolean'} |
    {type, 'date'} |
    {type, 'datetime'} |
    {enum, [atom()]} |
    {max, Max :: number()} |
    {exclusive_max, Max :: number()} |
    {min, Min :: number()} |
    {exclusive_min, Min :: number()} |
    {max_length, MaxLength :: integer()} |
    {min_length, MaxLength :: integer()} |
    {pattern, Pattern :: string()} |
    schema |
    required |
    not_required.

-spec request_param_info(OperationID :: operation_id(), Name :: request_param()) -> #{
    source => qs_val | binding | header | body,
    rules => [rule()]
}.



request_param_info('ChannelSync', 'ChannelSync') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('GetHeader', 'HeaderId') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('GetHeaders', 'HeaderIds') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };


request_param_info('AddAccount', 'CreateAccount') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('AddPeer', 'Peer') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('AddSecret', 'Secret') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('ChannelSpend', 'ChannelSpend') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('DeleteAccount', 'PubKey') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('FetchAccount', 'PubKey') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('LightningSpend', 'LightningSpend') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('MineBlock', 'MineBlock') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('NewChannelWithServer', 'NewChannelWithServer') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('PullChannelState', 'PullChannelState') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('RepoAccount', 'PubKey') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('SetKeyPair', 'SetKeyPair') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('Spend', 'Spend') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info('Sync', 'Sync') ->
    #{
        source =>   body,
        rules => [
            schema,
            required
        ]
    };

request_param_info(OperationID, Name) ->
    error({unknown_param, OperationID, Name}).

-spec populate_request(
    OperationID :: operation_id(),
    Req :: cowboy_req:req(),
    ValidatorState :: jesse_state:state()
) ->
    {ok, Model :: #{}, Req :: cowboy_req:req()} |
    {error, Reason :: any(), Req :: cowboy_req:req()}.

populate_request(OperationID, Req, ValidatorState) ->
    Params = request_params(OperationID),
    populate_request_params(OperationID, Params, Req, ValidatorState, #{}).

populate_request_params(_, [], Req, _, Model) ->
    {ok, Model, Req};

populate_request_params(OperationID, [FieldParams | T], Req0, ValidatorState, Model) ->
    case populate_request_param(OperationID, FieldParams, Req0, ValidatorState) of
        {ok, K, V, Req} ->
            populate_request_params(OperationID, T, Req, ValidatorState, maps:put(K, V, Model));
        Error ->
            Error
    end.

populate_request_param(OperationID, Name, Req0, ValidatorState) ->
    #{rules := Rules, source := Source} = request_param_info(OperationID, Name),
    {Value, Req} = get_value(Source, Name, Req0),
    case prepare_param(Rules, Name, Value, ValidatorState) of
        {ok, Result} -> {ok, Name, Result, Req};
        {error, Reason} ->
            {error, Reason, Req}
    end.

-spec validate_response(
    OperationID :: operation_id(),
    Code :: 200..599,
    Body :: jesse:json_term(),
    ValidatorState :: jesse_state:state()
) -> ok | no_return().


validate_response('ChannelSync', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('GetHeader', 200, Body, ValidatorState) ->
    validate_response_body('Header', 'Header', Body, ValidatorState);
validate_response('GetHeader', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('GetHeaders', 200, Body, ValidatorState) ->
    validate_response_body('Headers', 'Headers', Body, ValidatorState);
validate_response('GetHeaders', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);


validate_response('AddAccount', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('AddPeer', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('AddSecret', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('ChannelBalance', 200, Body, ValidatorState) ->
    validate_response_body('ChannelBalance', 'ChannelBalance', Body, ValidatorState);

validate_response('ChannelSoloClose', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('ChannelSpend', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('ChannelTimeout', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('CreateKeyPair', 200, Body, ValidatorState) ->
    validate_response_body('KeyPair', 'KeyPair', Body, ValidatorState);

validate_response('DeleteAccount', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('FetchAccount', 200, Body, ValidatorState) ->
    validate_response_body('Account', 'Account', Body, ValidatorState);
validate_response('FetchAccount', 404, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);
validate_response('FetchAccount', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('FetchKeyPair', 200, Body, ValidatorState) ->
    validate_response_body('KeyPair', 'KeyPair', Body, ValidatorState);
validate_response('FetchKeyPair', 403, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('FetchPubKey', 200, Body, ValidatorState) ->
    validate_response_body('PubKey', 'PubKey', Body, ValidatorState);

validate_response('GetTop', 200, Body, ValidatorState) ->
    validate_response_body('Top', 'Top', Body, ValidatorState);

validate_response('LightningSpend', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('MineBlock', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('NewChannelWithServer', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('PullChannelState', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('RepoAccount', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('SetKeyPair', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('Spend', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);

validate_response('Sync', 405, Body, ValidatorState) ->
    validate_response_body('', '', Body, ValidatorState);


validate_response(_OperationID, _Code, _Body, _ValidatorState) ->
    ok.

validate_response_body('list', ReturnBaseType, Body, ValidatorState) ->
    [
        validate(schema, ReturnBaseType, Item, ValidatorState)
    || Item <- Body];

validate_response_body(_, ReturnBaseType, Body, ValidatorState) ->
    validate(schema, ReturnBaseType, Body, ValidatorState).

%%%
validate(Rule = required, Name, Value, _ValidatorState) ->
    case Value of
        undefined -> validation_error(Rule, Name);
        _ -> ok
    end;

validate(not_required, _Name, _Value, _ValidatorState) ->
    ok;

validate(_, _Name, undefined, _ValidatorState) ->
    ok;

validate(Rule = {type, 'integer'}, Name, Value, _ValidatorState) ->
    try
        {ok, swagger_utils:to_int(Value)}
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {type, 'float'}, Name, Value, _ValidatorState) ->
    try
        {ok, swagger_utils:to_float(Value)}
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {type, 'binary'}, Name, Value, _ValidatorState) ->
    case is_binary(Value) of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(_Rule = {type, 'boolean'}, _Name, Value, _ValidatorState) when is_boolean(Value) ->
    {ok, Value};

validate(Rule = {type, 'boolean'}, Name, Value, _ValidatorState) ->
    V = binary_to_lower(Value),
    try
        case binary_to_existing_atom(V, utf8) of
            B when is_boolean(B) -> {ok, B};
            _ -> validation_error(Rule, Name)
        end
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {type, 'date'}, Name, Value, _ValidatorState) ->
    case is_binary(Value) of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {type, 'datetime'}, Name, Value, _ValidatorState) ->
    case is_binary(Value) of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {enum, Values}, Name, Value, _ValidatorState) ->
    try
        FormattedValue = erlang:binary_to_existing_atom(Value, utf8),
        case lists:member(FormattedValue, Values) of
            true -> {ok, FormattedValue};
            false -> validation_error(Rule, Name)
        end
    catch
        error:badarg ->
            validation_error(Rule, Name)
    end;

validate(Rule = {max, Max}, Name, Value, _ValidatorState) ->
    case Value >= Max of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {exclusive_max, ExclusiveMax}, Name, Value, _ValidatorState) ->
    case Value > ExclusiveMax of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {min, Min}, Name, Value, _ValidatorState) ->
    case Value =< Min of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {exclusive_min, ExclusiveMin}, Name, Value, _ValidatorState) ->
    case Value =< ExclusiveMin of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {max_length, MaxLength}, Name, Value, _ValidatorState) ->
    case size(Value) =< MaxLength of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {min_length, MinLength}, Name, Value, _ValidatorState) ->
    case size(Value) >= MinLength of
        true -> ok;
        false -> validation_error(Rule, Name)
    end;

validate(Rule = {pattern, Pattern}, Name, Value, _ValidatorState) ->
    {ok, MP} = re:compile(Pattern),
    case re:run(Value, MP) of
        {match, _} -> ok;
        _ -> validation_error(Rule, Name)
    end;

validate(Rule = schema, Name, Value, ValidatorState) ->
    Definition =  list_to_binary("#/definitions/" ++ swagger_utils:to_list(Name)),
    try
        _ = validate_with_schema(Value, Definition, ValidatorState),
        ok
    catch
        throw:[{schema_invalid, _, Error} | _] ->
            Info = #{
                type => schema_invalid,
                error => Error
            },
            validation_error(Rule, Name, Info);
        throw:[{data_invalid, Schema, Error, _, Path} | _] ->
            Info = #{
                type => data_invalid,
                error => Error,
                schema => Schema,
                path => Path
            },
            validation_error(Rule, Name, Info)
    end;

validate(Rule, Name, _Value, _ValidatorState) ->
    error_logger:info_msg("Can't validate ~p with ~p", [Name, Rule]),
    error({unknown_validation_rule, Rule}).

-spec validation_error(Rule :: any(), Name :: any()) -> no_return().

validation_error(ViolatedRule, Name) ->
    validation_error(ViolatedRule, Name, #{}).

-spec validation_error(Rule :: any(), Name :: any(), Info :: #{}) -> no_return().

validation_error(ViolatedRule, Name, Info) ->
    throw({wrong_param, Name, ViolatedRule, Info}).

get_value(body, _Name, Req0) ->
    {ok, Body, Req} = cowboy_req:body(Req0),
    Value = prepare_body(Body),
    {Value, Req};

get_value(qs_val, Name, Req0) ->
    {QS, Req} = cowboy_req:qs_vals(Req0),
    Value = swagger_utils:get_opt(swagger_utils:to_qs(Name), QS),
    {Value, Req};

get_value(header, Name, Req0) ->
    {Headers, Req} = cowboy_req:headers(Req0),
    Value = swagger_utils:get_opt(swagger_utils:to_header(Name), Headers),
    {Value, Req};

get_value(binding, Name, Req0) ->
    {Bindings, Req} = cowboy_req:bindings(Req0),
    Value = swagger_utils:get_opt(swagger_utils:to_binding(Name), Bindings),
    {Value, Req}.

prepare_body(Body) ->
    case Body of
        <<"">> -> <<"">>;
        _ -> jsx:decode(Body, [return_maps])
    end.

validate_with_schema(Body, Definition, ValidatorState) ->
    jesse_schema_validator:validate_with_state(
        [{<<"$ref">>, Definition}],
        Body,
        ValidatorState
    ).

prepare_param(Rules, Name, Value, ValidatorState) ->
    try
        Result = lists:foldl(
            fun(Rule, Acc) ->
                case validate(Rule, Name, Acc, ValidatorState) of
                    ok -> Acc;
                    {ok, Prepared} -> Prepared
                end
            end,
            Value,
            Rules
        ),
        {ok, Result}
    catch
        throw:Reason ->
            {error, Reason}
    end.

binary_to_lower(V) when is_binary(V) ->
    list_to_binary(string:to_lower(swagger_utils:to_list(V))).
