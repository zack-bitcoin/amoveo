-module(secrets).
%% This module holds a bunch of secrets make by this node, stored in a dict by hash.
-behaviour(gen_server).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3,
         add/2, add/3, read/1, delete/1, new_lightning/1, check/0, test/0]).
-define(LOC, constants:secrets()).
-define(none, <<"none">>).
-include("../records.hrl").
init(ok) ->
    %io:fwrite("starting secrets\n"),
    process_flag(trap_exit, true),
    K = case db:read(?LOC) of
            "" -> dict:new();
            X -> X
        end,
    {ok, K}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, State) ->
    db:save(?LOC, State),
    io:fwrite("secrets died"),
    ok.
code_change(_, State, _) -> {ok, State}.
handle_call(check, _, State) ->
    {reply, State, State};
handle_call({read, Code}, _, X) ->
    Packed = packer:pack({secret_read, Code}),
    Z = case dict:find(Code, X) of
	    error -> ?none;
	    {ok, Y} -> Y
	end,
    {reply, Z, X}.
handle_cast({add, Code, SS}, X) ->
    State = dict:store(Code, {SS, none}, X),
    db:save(?LOC, State),
    {noreply, State};
handle_cast({add, Code, SS, Amount}, X) ->
    State = dict:store(Code, {SS, Amount}, X),
    db:save(?LOC, State),
    {noreply, State};
handle_cast({delete, Code}, X) ->
    State = dict:erase(Code, X),
    db:save(?LOC, State),
    {noreply, State}.
add(Code, SS) -> gen_server:cast(?MODULE, {add, Code, SS}).
add(Code, SS, Amount) -> gen_server:cast(?MODULE, {add, Code, SS, Amount}).
read(Code) -> gen_server:call(?MODULE, {read, Code}).
delete(SH) -> gen_server:cast(?MODULE, {del, SH}).
new_lightning(Amount) ->
    %delay for canceling is 100
    S = crypto:strong_rand_bytes(constants:hash_size()),
    SH = hash:doit(S),
    PrivDir = code:priv_dir(amoveo_core),
    {ok, Contract} = file:read_file(PrivDir ++ "/lightning.fs"), 
    ESH = " binary 32 " ++ binary_to_list(base64:encode(SH)) ++ " >r " ++ Contract,
    ESS = " binary 32 " ++ binary_to_list(base64:encode(S)),
    Code = compiler_chalang:doit(list_to_binary(ESH)),
    SS = spk:new_ss(compiler_chalang:doit(list_to_binary(ESS)), []),
    add(Code, SS, Amount),
    {Code, SS}.
check() -> gen_server:call(?MODULE, check).

test() ->
    {Code, SS} = new_lightning(0),
    TP = tx_pool:get(),
    Trees = TP#tx_pool.block_trees,
    Height = TP#tx_pool.height,
    Amount = 200,
    Bet = spk:new_bet(Code, Code, Amount),
    SPK = spk:new(1, 2, <<3:256>>, [Bet], 9000, 9000, 1, 1),
    {Amount, _, _} = spk:run(fast, [SS], SPK, Height, 0, Trees),%for sanity check
    success.
    
