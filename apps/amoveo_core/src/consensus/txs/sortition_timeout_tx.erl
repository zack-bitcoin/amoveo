-module(sortition_timeout_tx).
-export([go/4, make_dict/6, cid_maker/1]).
-include("../../records.hrl").

make_dict(Creator, Winner, Winner2, SID, LN, Fee) ->
    Acc = trees:get(accounts, Creator),
    S = trees:get(sortition, SID),
    Amount = S#sortition.amount,
    #sortition_timeout_tx{pubkey = Creator, nonce = Acc#acc.nonce + 1, amount = Amount, sortition_id = SID, winner = Winner, winner2 = Winner2, layer = LN, fee = Fee}.
cid_maker(Tx) ->
   #sortition_timeout_tx{
           sortition_id = SID,
           winner = Winner, 
           winner2 = Winner2
          } = Tx,
    hash:doit(
      <<SID/binary,
        Winner/binary,
        Winner2/binary>>).

go(Tx, Dict, NewHeight, NonceCheck) ->
   #sortition_timeout_tx{
    sortition_id = SID,
    pubkey = From,
    nonce = Nonce,
    fee = Fee,
    winner = Winner,
    winner2 = Winner2,
    layer = LN,
    amount = Amount
   } = Tx,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce), %you pay a safety deposit.
    Dict2 = accounts:dict_write(A2, Dict),
    S = sortition:dict_get(SID, Dict2),
    #sortition{
                rng_value = RNGValue,
                top_candidate = TCID_0,
                closed = 0,
                last_modified = LM,
                delay = Delay,
                amount = Amount
              } = S,
    TCID = sortition_claim_tx:layer_salt(TCID_0, LN),
    true = (NewHeight - Delay) > LM,
    TC = candidates:dict_get(TCID, Dict2),
    #candidate{
                sortition_id = SID,
                winner = Winner,
                winner2 = Winner2,
                recovery_spend = RS
              } = TC,
    false = (Winner == <<0:520>>),
    {Amount2, Dict3} = 
        case RS of
            <<1:520>> -> 1=2;%double spend of the recovery process, money is destroyed.
            <<0:520>> -> {Amount, Dict2};%recover process not used.
            _ -> 
                    %pay out majority to RS, and a fraction to winner/s.
                SFSR = governance:dict_get_value(sortition_final_spend_refund, Dict2),
                %A1 = Amount * 9 div 10,
                A1 = Amount * (10000 - SFSR) div 10000,
                W3 = accounts:dict_update(RS, Dict2, A1, none),
                {Amount * SFSR div 10000,
                 accounts:dict_write(W3, Dict2)}
                
        end,
            
    Dict4 = 
        case Winner2 of
            <<0:520>> ->
                W2 = accounts:dict_update(Winner, Dict2, Amount2, none),
                accounts:dict_write(W2, Dict3);
            _ ->
                CID = cid_maker(Tx),
                Bal = Amount2 div 2,
                NewChannel = channels:new(CID, Winner, Winner2, Bal, Bal, NewHeight, Delay),%same as delay in sortition.
                channels:dict_write(NewChannel, Dict3)
        end,
    S2 = S#sortition{
           closed = 1
          },
    sortition:dict_write(S2, Dict4).
    
    
    
