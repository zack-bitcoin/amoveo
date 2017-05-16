%When we sign a block, we record the hash of a secret. Later on, we need to reveal this secret.
%This module holds a bunch of secrets make by this node, stored in a dict by hash.

%needs garbage collection.
-module(secrets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
	 read/1,delete/1,new_lightning/0,check/0,
	 add/2]).
%-define(LOC, "secrets.db").
-define(LOC, constants:secrets()).
-define(none, <<"none">>).
init(ok) ->
    io:fwrite("starting secrets"),
    process_flag(trap_exit, true),
        K = case db:read(?LOC) of
		"" ->
		     dict:new();
		X -> binary_to_term(X)
    end,
    {ok, K}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    db:save(?LOC, term_to_binary(K)),
    io:format("secrets died!"),
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Code, ?none}, X) ->
    {noreply, dict:erase(Code, X)};
handle_cast({add, Code, SS}, X) ->
    io:fwrite(packer:pack({secret_add, Code, SS})),
    io:fwrite("\n"),
    {noreply, dict:store(Code, SS, X)};
handle_cast({delete, Code}, X) ->
    {noreply, dict:erase(Code, X)}.
handle_call(check, _From, X) -> {reply, X, X};
handle_call({read, Code}, _From, X) ->
    io:fwrite(packer:pack({secret_read, Code})),
    io:fwrite("\n"),
    Z = case dict:find(Code, X) of
	    error -> ?none;
	    {ok, Y} -> Y
			   end,
    {reply, Z, X}.
%add(S) ->
%    SH = testnet_hasher:doit(S),
%    add(SH, S).
add(Code, SS) ->
    gen_server:cast(?MODULE, {add, Code, SS}).
new_lightning() -> 
    case free_constants:test_mode() of
	true -> test_new_lightning();
	false -> real_new_lightning()
    end.

test_new_lightning() ->
    {B64SS, B64Code} = {<<"AgAAAAweE8Fgc9G3s5/Zsew=">>, <<"WgAAAAAAOkYAAAAAMgAAAAABAAAAAACEC0dIKAIAAAAMrsdbBnWyK5CbWkYkOhYUFhRGAAAAAAAAAAAAAgAAACcQRwAAAAAyAAAAAAEAAAAAAEiECw==">>},
    SS = base64:decode(B64SS),
    Code = base64:decode(B64Code),
    add(Code, SS),
    {Code, SS}.

real_new_lightning() ->
    S = crypto:strong_rand_bytes(constants:hash_size()),
    SH = testnet_hasher:doit(S),
    ESH = "stack_size int 0 == if
int 50 int 1 int 0 nil crash else then
hash binary 12 " ++
	binary_to_list(base64:encode(SH)) ++ 
	" == swap drop swap drop if
int 0 int 2 int 10000
else
int 50 int 1 int 0
then nil crash",
    ESS = "binary 12 " ++ base64:encode(S),
    Code = compiler_chalang:doit(list_to_binary(ESH)),
    SS = compiler_chalang:doit(list_to_binary(ESS)),
    add(Code, SS),
    {Code, SS}.
check() -> gen_server:call(?MODULE, check).
read(Code) -> gen_server:call(?MODULE, {read, Code}).
delete(SH) -> gen_server:cast(?MODULE, {del, SH}).
