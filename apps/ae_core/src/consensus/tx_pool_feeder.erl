-module(tx_pool_feeder).
-behaviour(gen_server).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([absorb/1, absorb_unsafe/1]).
-include("../records.hrl").
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
init(ok) -> {ok, []}.
handle_call({absorb, SignedTx}, _From, State) ->
    absorb_internal(SignedTx),
    {reply, ok, State};
handle_call(_, _, S) -> {reply, S, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> io:fwrite("tx_pool_feeder died\n").
code_change(_, S, _) -> {ok, S}.
is_in(_, []) -> false;
is_in(Tx, [STx2 | T]) ->
    Tx2 = testnet_sign:data(STx2),
    (Tx == Tx2) orelse (is_in(Tx, T)).
absorb_internal(SignedTx) ->
    F = tx_pool:get(),
    Trees = F#tx_pool.trees,
    Height = F#tx_pool.height,
    Txs = F#tx_pool.txs,
    Dict = F#tx_pool.dict,
    Governance = trees:governance(Trees),
    Tx = testnet_sign:data(SignedTx),
    Fee = element(4, Tx),
    Type = element(1, Tx),
    Cost = governance:get_value(Type, Governance),
    {ok, MinimumTxFee} = application:get_env(ae_core, minimum_tx_fee),
    true = Fee > (MinimumTxFee + Cost),
    true = testnet_sign:verify(SignedTx),
    case is_in(testnet_sign:data(SignedTx), Txs) of
        true -> ok;
        false -> absorb_unsafe(SignedTx, Trees, Height, Dict)
    end.
absorb_unsafe(SignedTx, Trees, Height, _Dict) ->
    Querys = proofs:txs_to_querys([SignedTx], Trees),
    Facts = proofs:prove(Querys, Trees),
    Dict2 = proofs:facts_to_dict(Facts, dict:new()),
    NewDict = txs:digest_from_dict([SignedTx], Dict2, Height + 1),
    NewTrees = block:dict_update_trie(Trees, NewDict), 
    tx_pool:absorb_tx(NewTrees, NewDict, SignedTx).
absorb([]) -> ok;%if one tx makes the gen_server die, it doesn't ignore the rest of the txs.
absorb([H|T]) -> absorb(H), absorb(T);
absorb(SignedTx) ->
    TP = tx_pool:get(),
    Trees = TP#tx_pool.trees,
    Txs = TP#tx_pool.txs,
    {ok, PP} = application:get_env(ae_core, prune_txs),
    TN = length(Txs) rem PP,
    if 
	TN == 0 ->
	    B = #block{trees = Trees},
	    block_absorber:synch_prune([B]);
	true -> ok
    end,
    gen_server:call(?MODULE, {absorb, SignedTx}).
absorb_unsafe(SignedTx) ->
    F = tx_pool:get(),
    Trees = F#tx_pool.trees,
    Height = F#tx_pool.height,
    Dict = F#tx_pool.dict,
    absorb_unsafe(SignedTx, Trees, Height, Dict).
