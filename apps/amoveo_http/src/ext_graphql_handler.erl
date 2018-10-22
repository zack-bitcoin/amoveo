-module(ext_graphql_handler).
-include("../../amoveo_core/src/records.hrl").

-export([init/3, handle/2, terminate/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3010/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '["test"]' http://localhost:3011
%curl -i -d echotxt http://localhost:3010

init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
handle(Req, State) ->
		{Method, Req2} = cowboy_req:method(Req),
		{ok, Req3, State2} = handle_method(Method, Req2, State),
   	{ok, Req3, State2}.

handle_method(<<"POST">>, Req, State) -> handle_post(Req, State);
handle_method(Method, Req, State) ->
		io:fwrite("I can't handle this \n"),
		io:fwrite(packer:pack(Method)),
		{ok, Req2} = cowboy_req:reply(404, [{<<"connection">>, <<"close">>}], Req),
		{ok, Req2, State}.

handle_post(Req, State) ->
		case gather(Req) of
			{error, Reason} ->
					err(400, Reason, Req, State);
			{ok, Req2, Decoded} ->
					run(Decoded, Req2, State)
		end.

run(#{ document := Doc,
       vars := Vars,
       operation_name := OpName }, Req, State) ->
		case graphql:parse(Doc) of
				{ok, AST} ->
						try
								{ok, #{fun_env := FunEnv,
								      ast := AST2 }} = graphql:type_check(AST),
								ok = graphql:validate(AST2),
								Coerced = graphql:type_check_params(FunEnv, OpName, Vars),
								Ctx = #{ params => Coerced, operation_name => OpName },
								Response = graphql:execute(Ctx, AST2),
								Headers = [{<<"content-type">>, <<"application/json">>},
							       {<<"Access-Control-Allow-Origin">>, <<"*">>}],
								{ok, Reply} = cowboy_req:reply(200, Headers, encode_json(Response), Req),
								{ok, Reply, State}
						catch
								throw:Err ->
								    err(400, Err, Req, State)
						end;
				{error, Error} ->
				    err(400, {parser_error, Error}, Req, State)
		end.

encode_json(Term) ->
    jsx:encode(fixup(Term)).

%% Ground types
fixup(Term) when is_number(Term) -> Term;
fixup(Term) when is_atom(Term) -> Term;
fixup(Term) when is_binary(Term) -> Term;
%% Compound types
fixup(Term) when is_list(Term) ->
    [fixup(T) || T <- Term];
fixup(Term) when is_map(Term) ->
    KVs = maps:to_list(Term),
    maps:from_list([{fixup_key(K), fixup(V)} || {K, V} <- KVs]);
fixup(Term) ->
    %% Every other term is transformed into a binary value
    iolist_to_binary(
      io_lib:format("~p", [Term])).

fixup_key(Term) ->
    case fixup(Term) of
        T when is_binary(T) ->
            T;
        T ->
            iolist_to_binary(io_lib:format("~p", [T]))
    end.

gather(Req) ->
		{ok, Body, Req2} = cowboy_req:body(Req),
    Bindings = cowboy_req:bindings(Req2),
    try jsx:decode(Body, [return_maps]) of
        JSON ->
            gather(Req2, JSON, Bindings)
    catch
        error:badarg ->
            {error, invalid_json_body}
    end.

gather(Req, Body, Params) ->
    QueryDocument = document([Params, Body]),
    case variables([Params, Body]) of
        {ok, Vars} ->
            Operation = operation_name([Params, Body]),
            {ok, Req, #{ document => QueryDocument,
                         vars => Vars,
                         operation_name => Operation}};
        {error, Reason} ->
            {error, Reason}
    end.

variables([#{ <<"variables">> := Vars} | _]) ->
  if
      is_binary(Vars) ->
          try jsx:decode(Vars, [return_maps]) of
              null -> {ok, #{}};
              JSON when is_map(JSON) -> {ok, JSON};
              _ -> {error, invalid_json}
          catch
              error:badarg ->
                  {error, invalid_json}
          end;
      is_map(Vars) ->
          {ok, Vars};
      Vars == null ->
          {ok, #{}}
  end;

variables([_ | Next]) ->
    variables(Next);
variables([]) ->
    {ok, #{}}.

operation_name([#{ <<"operationName">> := OpName } | _]) ->
    OpName;
operation_name([_ | Next]) ->
    operation_name(Next);
operation_name([]) ->
    undefined.

document([#{ <<"query">> := Q }|_]) -> Q;
document([_|Next]) -> document(Next);
document([]) -> undefined.

err(Code, Msg, Req, State) ->
    Formatted = iolist_to_binary(io_lib:format("~p", [Msg])),
    Err = #{ type => error,
             message => Formatted },
    Body = jsx:encode(#{ errors => [Err] }),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {ok, Reply} = cowboy_req:reply(Code, Req2),
    {stop, Reply, State}.
