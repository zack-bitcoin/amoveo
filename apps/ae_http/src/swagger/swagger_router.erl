-module(swagger_router).

-export([get_paths/1]).

-type operations() :: #{
    Method :: binary() => swagger_api:operation_id()
}.

-type init_opts()  :: {
    Operations :: operations(),
    LogicHandler :: atom(),
    ValidatorState :: jesse_state:state()
}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: atom()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler) ->
    ValidatorState = prepare_validator(),
    PreparedPaths = maps:fold(
        fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
            [{Path, Handler, Operations} | Acc]
        end,
        [],
        group_paths()
    ),
    [
        {'_',
            [{P, H, {O, LogicHandler, ValidatorState}} || {P, H, O} <- PreparedPaths]
        }
    ].

group_paths() ->
    maps:fold(
        fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
            case maps:find(Path, Acc) of
                {ok, PathInfo0 = #{operations := Operations0}} ->
                    Operations = Operations0#{Method => OperationID},
                    PathInfo = PathInfo0#{operations => Operations},
                    Acc#{Path => PathInfo};
                error ->
                    Operations = #{Method => OperationID},
                    PathInfo = #{handler => Handler, operations => Operations},
                    Acc#{Path => PathInfo}
            end
        end,
        #{},
        get_operations()
    ).

get_operations() ->
    #{ 
        'AddAccount' => #{
            path => "/v1/account",
            method => <<"POST">>,
            handler => 'swagger_account_handler'
        },
        'ChannelSpend' => #{
            path => "/v1/channel-spend",
            method => <<"POST">>,
            handler => 'swagger_channelspend_handler'
        },
        'GetKeyPair' => #{
            path => "/v1/keypair",
            method => <<"GET">>,
            handler => 'swagger_keypair_handler'
        },
        'LightningSpend' => #{
            path => "/v1/lightning-spend",
            method => <<"POST">>,
            handler => 'swagger_lightningspend_handler'
        },
        'LoadKeyPair' => #{
            path => "/v1/load-keypair",
            method => <<"POST">>,
            handler => 'swagger_loadkeypair_handler'
        },
        'NewChannelWithServer' => #{
            path => "/v1/new-channel-with-server",
            method => <<"POST">>,
            handler => 'swagger_newchannelwithserver_handler'
        },
        'AddPeer' => #{
            path => "/v1/peer",
            method => <<"POST">>,
            handler => 'swagger_peer_handler'
        },
        'PullChannelState' => #{
            path => "/v1/pull-channel-state",
            method => <<"POST">>,
            handler => 'swagger_pullchannelstate_handler'
        },
        'Spend' => #{
            path => "/v1/spend",
            method => <<"POST">>,
            handler => 'swagger_spend_handler'
        },
        'GetTop' => #{
            path => "/v1/top",
            method => <<"GET">>,
            handler => 'swagger_top_handler'
        }
    }.

prepare_validator() ->
    R = jsx:decode(element(2, file:read_file(get_swagger_path()))),
    jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]).


get_swagger_path() ->
    {ok, AppName} = application:get_application(?MODULE),
    filename:join(swagger_utils:priv_dir(AppName), "swagger.json").


