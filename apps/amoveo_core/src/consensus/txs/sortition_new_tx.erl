-module(sortition_new_tx).
-export([go/4, make_dict/10, make_root/1]).
-include("../../records.hrl").

make_dict(Creator, Amount, SID, Entropy, TradingEnds, ResponseDelay, RE, Delay, Validators, Fee) ->
    Acc = trees:get(accounts, Creator),
    VH = make_root(Validators),
    #sortition_new_tx{creator = Creator, nonce = Acc#acc.nonce + 1, amount = Amount, id = SID, fee = Fee, entropy = Entropy, trading_ends = TradingEnds, response_delay = ResponseDelay, rng_ends = RE, delay = Delay, validators = VH}.
make_root(Validators) ->
    Size = 65,
    KeyLength = 5,
    M = mtree:new_empty(KeyLength, Size, 0),
    CFG = mtree:cfg(M),
    L = merklize_make_leaves(0, Validators, CFG),
    {Root, M2} = mtree:store_batch(L, 1, M),
    mtree:root_hash(Root, M2).
    
merklize_make_leaves(_, [], _) -> [];
merklize_make_leaves(N, [H|T], CFG) -> 
    Leaf = leaf:new(N, H, 0, CFG),
    [Leaf|merklize_make_leaves(N+1, T, CFG)].

go(Tx, Dict, NewHeight, NonceCheck) ->
    F28 = forks:get(28),
    true = NewHeight > F28,
    #sortition_new_tx{
    creator = Creator,
    amount = Amount,
    fee = Fee,
    id = SID,
    nonce = Nonce,
    entropy = Entropy,
    trading_ends = TE,
    response_delay = RD,
    rng_ends = RE,
    delay = Delay,
    validators = VH
   } = Tx,
    empty = sortition:dict_get(SID, Dict),
    Facc = accounts:dict_update(Creator, Dict, -Amount-Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    S = sortition:new(SID, Amount, Entropy, Creator,
                      TE, RD, RE, Delay, VH),
    sortition:dict_write(S, Dict2).
    
