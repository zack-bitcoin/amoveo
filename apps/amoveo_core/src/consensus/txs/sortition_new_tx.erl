-module(sortition_new_tx).
-export([go/4, make_dict/9]).
-include("../../records.hrl").

make_dict(Creator, Amount, SID, Entropy, TradingEnds, ResponseDelay, RE, Delay, Fee) ->
    Acc = trees:get(accounts, Creator),
    #sortition_new_tx{creator = Creator, nonce = Acc#acc.nonce + 1, amount = Amount, id = SID, fee = Fee, entropy = Entropy, trading_ends = TradingEnds, response_delay = ResponseDelay, rng_ends = RE, delay = Delay}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    Creator = Tx#sortition_new_tx.creator,
    Amount = Tx#sortition_new_tx.amount,
    Fee = Tx#sortition_new_tx.fee,
    SID = Tx#sortition_new_tx.id,
    Nonce = Tx#sortition_new_tx.nonce,
    empty = sortition:dict_get(SID, Dict),
    Facc = accounts:dict_update(Creator, Dict, -Amount-Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    Entropy = Tx#sortition_new_tx.entropy,
    S = sortition:new(SID, Amount, Entropy, Creator,
                      Tx#sortition_new_tx.trading_ends,
                      Tx#sortition_new_tx.response_delay,
                      Tx#sortition_new_tx.rng_ends,
                      Tx#sortition_new_tx.delay),
    sortition:dict_write(S, Dict2).
    
