-module(tx_pool_feeder).
-behaviour(gen_server).

%% API
-export([absorb/1,
         absorb_unsafe/1]).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API functions

absorb([]) ->
    %Absorb needs to feed every tx into the gen_server seperately.
    %This way if one tx makes the gen_server die, it doesn't ignore the rest of the txs.
    ok;
absorb([H | T]) ->
    absorb(H),
    absorb(T);
absorb(SignedTx) ->
    gen_server:call(?MODULE, {absorb, SignedTx}).

absorb_unsafe(SignedTx) ->
    {Trees, Height, _} = tx_pool:data(),
    absorb_unsafe(SignedTx, Trees, Height).

absorb_unsafe(SignedTx, Trees, Height) ->
    NewTrees = txs:digest([SignedTx], Trees, Height + 1),
    tx_pool:absorb_tx(NewTrees, SignedTx).
absorb_unsafe_new(SignedTx, Dict, Facts, Trees, Height) ->
    %The trees shows the state after the previous block.
    %Dict holds the state after applying all the recent txs.
    Querys = proofs:txs_to_querys([SignedTx], Trees),
    Querys = proofs:facts_to_querys(Facts),%verify that these facts are trying to prove the validity of this tx.
    %check that the facts are valid.
    true = verify_proofs(Facts, Trees),
    NewDict = txs:digest_from_dict([SignedTx], Dict, Height + 1),
    tx_pool:absorb_tx_facts(NewDict, SignedTx, Facts).
verify_proofs([], _) -> true;
verify_proofs([F|T], Trees) ->
    Type = proofs:tree(F),
    true = is_atom(Type),
    Root = proofs:root(F),
    Root = Type:root_hash(trees:Type(Trees)),
    X = Type:verify_proof(
          Root,
          proofs:key(F),
          proofs:value(F),
          proofs:path(F)),
    if
        X -> verify_proofs(T, Trees);
        true -> false
    end.
            
    
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).


%% gen_server callbacks

init(ok) ->
    {ok, []}.

handle_call({absorb, SignedTx}, _From, State) ->
    absorb_internal(SignedTx),
    {reply, ok, State};
handle_call(_, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = lager:warning("~p died!", [?MODULE]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internals

is_in(_, []) ->
    false;
is_in(STx, [STx2 | T]) ->
    Tx = testnet_sign:data(STx),
    Tx2 = testnet_sign:data(STx2),
    case Tx == Tx2 of
        true ->
            true;
        false ->
            is_in(STx, T)
    end.

absorb_internal(SignedTx) ->
    {Trees, Height, Txs} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Tx = testnet_sign:data(SignedTx),
    Fee = element(4, Tx),
    Type = element(1, Tx),
    Cost = governance:get_value(Type, Governance),
    {ok, MinimumTxFee} = application:get_env(ae_core, minimum_tx_fee),
    true = Fee > (MinimumTxFee + Cost),
    Accounts = trees:accounts(Trees),
    true = testnet_sign:verify(SignedTx),
    Out = case is_in(SignedTx, Txs) of
        true ->
            ok = lager:info("Already have this tx");
        false ->
            absorb_unsafe(SignedTx, Trees, Height)
    end,
    Out. 
