-module(amoveo_http_app).
-behaviour(application).


%% Application callbacks
-export([start/2,
         stop/1]).

-export([add_requred_headers_to_external/4]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = amoveo_http_sup:start_link(),
    ok = start_internal(),
    ok = start_external(),
    {ok, Pid}.

stop(_State) ->
    ok.

start_internal() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [{"/:file", int_file_handler, []},
                  {"/", int_handler, []}
                 ]}]),
    %Port = application:get_env(amoveo_core, internal_port, ?DEFAULT_INTERNAL_PORT),
    {ok, Port} = application:get_env(amoveo_core, internal_port),
    {ok, _} = cowboy:start_http(http_internal, 100,
                                [{ip, {127, 0, 0, 1}}, {port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

start_external() ->
    DispatchOpts =
        case application:get_env(amoveo_core, kind) of
            {ok, "production"} -> {priv_dir, amoveo_http, "external_web", [{mimetypes, cow_mimetypes, all}]};
            _ -> {dir, "../../../../apps/amoveo_http/priv", "external_web", [{mimetypes, cow_mimetypes, all}]}
        end,
    Dispatch =
        cowboy_router:compile(
          [{'_', [{"/", ext_handler, []},
                  {"/[...]", cowboy_static, DispatchOpts}
          ]}]),
    {ok, Port} = application:get_env(amoveo_core, port),
    {ok, _} = cowboy:start_http(http, 100,
                                [{ip, {0, 0, 0, 0}}, {port, Port}],
                                [
                                    {onresponse, fun ?MODULE:add_requred_headers_to_external/4},
                                    {env, [{dispatch, Dispatch}]}
                                ]),
    ok.

add_requred_headers_to_external(Code, Headers, _Body, Req) ->
    Headers2 =
        case lists:keymember(<<"Access-Control-Allow-Origin">>, 1, Headers) of
            false -> [{<<"Access-Control-Allow-Origin">>, <<"*">>} | Headers];
            true -> Headers
        end,
    {Code, Headers2, Req}.
