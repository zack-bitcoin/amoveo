-module(sortition_final_spend_tx).
-export([go/4, make_dict/5, make_final/5]).
-include("../../records.hrl").


make_final(From, To, SID, Contract, Prove) ->
    keys:sign(
      #final_spend{
         from = From,
         from2 = <<0:520>>,
         to = To,
         sortition_id = SID,
         contract = Contract,
         prove = Prove}).

make_dict(From, ClaimID, SignedSpend, Evidence, Fee) ->
    Acc = trees:get(accounts, From),
    #sortition_final_spend_tx{
                     from = From, nonce = Acc#acc.nonce + 1,
                     fee = Fee,
                     claim_id = ClaimID, 
                     %sortition_id = SID,
                     signed_spend = SignedSpend,
                     evidence = Evidence
                    }.
   
go(Tx, Dict, NewHeight, _) -> 
    #sortition_final_spend_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    claim_id = ClaimID,
    %sortition_id = SID,
    signed_spend = SignedSpend,
    evidence = Evidence
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
                  sortition_id = SID,
                  contract = Contract,
                  prove = Prove
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
    Dict3 = candidates:dict_write(C2, Dict2),
    
    Funs = governance:dict_get_value(fun_limit, Dict3),
    Vars = governance:dict_get_value(var_limit, Dict3),
    OpGas = governance:dict_get_value(time_gas, Dict3),
    RamGas = governance:dict_get_value(space_gas, Dict3),
    Facts = spk:prove_facts(Prove, 
                            Dict3, 
                            NewHeight),
    State = chalang:new_state(NewHeight, 0, 0),
    io:fwrite(packer:pack([Vars])),
    io:fwrite("\n"),
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, Evidence, Contract, State, constants:hash_size(), 2, false),
    Contract2 = <<Facts/binary,
                  Contract/binary>>,
    Data2 = chalang:run5(Evidence, Data),
    Data3 = chalang:run5(Contract2, Data2),
    [<<1:32>>|_] = chalang:stack(Data3), %check that the smart contract in the waiver returns true.
    

    Dict3.
