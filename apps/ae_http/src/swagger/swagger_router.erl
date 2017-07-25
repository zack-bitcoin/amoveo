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
        'ChannelSync' => #{
            path => "/v1/channel-sync",
            method => <<"POST">>,
            handler => 'swagger_external_handler'
        },
        'GetHeader' => #{
            path => "/v1/header",
            method => <<"POST">>,
            handler => 'swagger_external_handler'
        },
        'GetHeaders' => #{
            path => "/v1/headers",
            method => <<"POST">>,
            handler => 'swagger_external_handler'
        },
        'AddAccount' => #{
            path => "/v1/create-account",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'AddPeer' => #{
            path => "/v1/peer",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'ChannelBalance' => #{
            path => "/v1/channel-balance",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'ChannelSpend' => #{
            path => "/v1/channel-spend",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'CreateKeyPair' => #{
            path => "/v1/create-keypair",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'DeleteAccount' => #{
            path => "/v1/delete-account",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'FetchAccount' => #{
            path => "/v1/fetch-account",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'FetchKeyPair' => #{
            path => "/v1/fetch-keypair",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'FetchPubKey' => #{
            path => "/v1/fetch-pubkey",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'GetTop' => #{
            path => "/v1/top",
            method => <<"GET">>,
            handler => 'swagger_internal_handler'
        },
        'LightningSpend' => #{
            path => "/v1/lightning-spend",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'MineBlock' => #{
            path => "/v1/mine-block",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'NewChannelWithServer' => #{
            path => "/v1/new-channel-with-server",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'PullChannelState' => #{
            path => "/v1/pull-channel-state",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'RepoAccount' => #{
            path => "/v1/repo-account",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'SetKeyPair' => #{
            path => "/v1/set-keypair",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'Spend' => #{
            path => "/v1/spend",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        },
        'Sync' => #{
            path => "/v1/sync",
            method => <<"POST">>,
            handler => 'swagger_internal_handler'
        }
    }.

prepare_validator() ->
    R = jsx:decode(element(2, file:read_file(get_swagger_path()))),
    jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]).


get_swagger_path() ->
    {ok, AppName} = application:get_application(?MODULE),
    filename:join(swagger_utils:priv_dir(AppName), "swagger.json").


