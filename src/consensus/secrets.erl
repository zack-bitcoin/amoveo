%When we sign a block, we record the hash of a secret. Later on, we need to reveal this secret.
%This module holds a bunch of secrets make by this node, stored in a dict by hash.

%needs garbage collection.
-module(secrets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
	 read/1,delete/1,new_lightning/0,check/0,
	 add/1,add/2]).
%-define(LOC, "secrets.db").
-define(LOC, constants:secrets()).
init(ok) ->
     
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
handle_cast({add, Secret, SH}, X) -> {noreply, dict:store(SH, Secret, X)};
handle_cast({delete, SH}, X) -> {noreply, dict:erase(SH, X)}.
handle_call(check, _From, X) -> {reply, X, X};
handle_call({read, SH}, _From, X) -> 
    Z = case dict:find(SH, X) of
	    error -> <<"none">>;
	    {ok, Y} -> Y
			   end,
    {reply, Z, X}.
add(S) ->
    SH = testnet_hasher:doit(S),
    add(SH, S).
add(SH, S) ->
    gen_server:cast(?MODULE, {add, S, SH}).
new_lightning() -> 
    S = crypto:strong_rand_bytes(constants:hash_size()),
    SH = testnet_hasher:doit(S),
    ESH = "hash binary 12 " ++ 
	binary_to_list(base64:encode(SH)) ++ 
	" == swap drop swap drop ",
    ES = "binary 12 " ++ base64:encode(S),
    FSH = compiler_chalang:doit(list_to_binary(ESH)),
    FS = compiler_chalang:doit(list_to_binary(ES)),
    add(FSH, FS),
    {FSH, FS}.
check() -> gen_server:call(?MODULE, check).
read(SH) -> gen_server:call(?MODULE, {read, SH}).
delete(SH) -> gen_server:cast(?MODULE, {del, SH}).
