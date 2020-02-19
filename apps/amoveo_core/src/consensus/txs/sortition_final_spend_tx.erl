-module(sortition_final_spend_tx).
-export([go/4, make_dict/4, make_final/3]).
-include("../../records.hrl").

make_final(From, To, SID) ->
    keys:sign(
      #final_spend{
         from = From,
         from2 = <<0:520>>,
         to = To,
         sortition_id = SID}).

make_dict(From, ClaimID, SignedSpend, Fee) ->
    Acc = trees:get(accounts, From),
    #sortition_final_spend_tx{
                     from = From, nonce = Acc#acc.nonce + 1,
                     fee = Fee,
                     claim_id = ClaimID, 
                     %sortition_id = SID,
                     signed_spend = SignedSpend
                    }.
   
go(Tx, Dict, NewHeight, _) -> 
    #sortition_final_spend_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    claim_id = ClaimID,
    %sortition_id = SID,
    signed_spend = SignedSpend
   } = Tx,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(A2, Dict),
    C = candidates:dict_get(ClaimID, Dict2),
    #candidate{
                sortition_id = SID,
                winner = W1,
                winner2 = W2,
                recovery_spend = OldRS
              } = C,
    S = sortition:dict_get(SID, Dict2),
    #sortition{
                closed = 0
              } = S,
    Spend = testnet_sign:data(SignedSpend),
    #final_spend{
                  to = FinalReceiver,
                  from = W1,
                  from2 = W2,
                  sortition_id = SID
                } = Spend,

    true = testnet_sign:verify_sig(Spend, SignedSpend#signed.sig, W1),
    case W2 of
        <<0:520>> -> ok;
        _ -> true = testnet_sign:verify_sig(Spend, SignedSpend#signed.sig2, W2)
    end,
    C2 = case OldRS of
             <<1:520>> ->
                 io:fwrite("this candidate is already invalid for double-spending, there is nothing left to do for it."),
                 OldRS = 5;
             <<0:520>> ->
                 C#candidate{recovery_spend = FinalReceiver};
             _ -> 
                 %mark it as invalid because of the attempted double spend
                 C#candidate{recovery_spend = <<1:520>>}
         end,
    candidates:dict_write(C2, Dict2).
