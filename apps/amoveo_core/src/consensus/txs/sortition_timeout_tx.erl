-module(sortition_timeout_tx).
-export([go/4, make_dict/5]).
-include("../../records.hrl").
%-record(sortition_timeout_tx, {pubkey, nonce, fee, winner, amount, sortition_id}).

make_dict(Creator, Winner, SID, LN, Fee) ->
    Acc = trees:get(accounts, Creator),
    S = trees:get(sortition, SID),
    Amount = S#sortition.amount,
    #sortition_timeout_tx{pubkey = Creator, nonce = Acc#acc.nonce + 1, amount = Amount, sortition_id = SID, winner = Winner, layer = LN, fee = Fee}.

go(Tx, Dict, NewHeight, NonceCheck) ->
   #sortition_timeout_tx{
    sortition_id = SID,
    pubkey = From,
    nonce = Nonce,
    fee = Fee,
    winner = Winner,
    layer = LN,
    amount = Amount
   } = Tx,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce), %you pay a safety deposit.
    Dict2 = accounts:dict_write(A2, Dict),
    W2 = accounts:dict_update(Winner, Dict2, Amount, none),
    Dict3 = accounts:dict_write(W2, Dict2),
    S = sortition:dict_get(SID, Dict3),
    #sortition{
                rng_value = RNGValue,
                top_candidate = TCID_0,
                closed = Closed,
                last_modified = LM,
                delay = Delay,
                amount = Amount
              } = S,
    TCID = sortition_claim_tx:layer_salt(TCID_0, LN),
    Closed = 0,
    true = (NewHeight - Delay) > LM,
    TC = candidates:dict_get(TCID, Dict3),
    #candidate{
                sortition_id = SID,
                winner = Winner
              } = TC,
    false = (Winner == <<0:520>>),
    S2 = S#sortition{
           closed = 1
          },
    sortition:dict_write(S2, Dict3).
    
    
    
