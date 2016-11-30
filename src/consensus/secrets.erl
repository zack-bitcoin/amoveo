%When we sign a block, we record the hash of a secret. Later on, we need to reveal this secret.
%This module holds a bunch of secrets make by this node, stored in a dict by hash.

%needs garbage collection.
-module(secrets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, read/1,delete/1,new/0,test/0,check/0,dict2bytes/1,bytes2dict/1,add/1]).
%-define(LOC, "secrets.db").
-define(LOC, constants:secrets()).
init(ok) -> 
    process_flag(trap_exit, true),
    K = case db:read(?LOC) of
	"" -> dict:new();
	X -> bytes2dict(X)
    end,
    {ok, K}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    db:save(?LOC, dict2bytes(K)),
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
    SH = hash:doit(S),
    gen_server:cast(?MODULE, {add, S, SH}).
new() -> 
    S = crypto:strong_rand_bytes(32),
    SH = hash:doit(S),
    add(S),
    SH.
check() -> gen_server:call(?MODULE, check).
read(SH) -> gen_server:call(?MODULE, {read, SH}).
delete(SH) -> gen_server:cast(?MODULE, {del, SH}).
dict2bytes(D) ->
    Keys = dict:fetch_keys(D),
    dict2bytes_map(Keys, D, <<>>).
dict2bytes_map([], _, Out) -> Out;
dict2bytes_map([H|Keys], D, Out) -> 
    Val = dict:fetch(H, D),
    32 = size(H),
    32 = size(Val),
    dict2bytes_map(Keys, D, <<H/binary, Val/binary, Out/binary>>).
bytes2dict(B) -> bytes2dict(B, dict:new()).
bytes2dict(<<>>, D) -> D;
bytes2dict(<<Key:256, Val:256, Out/binary>>, D) ->
    bytes2dict(Out, dict:store(<<Key:256>>, <<Val:256>>, D)).
test() ->
    A = hash:doit(1),
    B = hash:doit(2),
    C = hash:doit(3),
    D = hash:doit(4),
    E = dict:new(),
    F = dict:store(A, B, E),
    G = dict:store(C, D, F),
    G = bytes2dict(dict2bytes(G)),
    success.
