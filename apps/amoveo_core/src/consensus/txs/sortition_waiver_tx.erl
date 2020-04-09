-module(sortition_waiver_tx). 
-export([go/4, make_dict/6, make_waiver/4]).
-include("../../records.hrl").
%-record(sortition_waiver_tx, {pubkey, nonce, fee, sortition_id, signed_waiver}).
%-record(waiver, {pubkey, sortition_id, contract}).

make_waiver(Who, Pub2, SID, Contract) ->
    #waiver{pubkey = Who,
            pubkey2 = Pub2, 
            sortition_id = SID,
            contract = Contract}.

make_dict(From, Fee, SID, LN, SignedWaiver, SS) ->
    Acc = trees:get(accounts, From),
    #sortition_waiver_tx{pubkey = From, nonce = Acc#acc.nonce + 1, sortition_id = SID, fee = Fee, signed_waiver = SignedWaiver, script_sig = SS, layer = LN}.
    
go(Tx, Dict, NewHeight, NonceCheck) ->
    #sortition_waiver_tx{
    pubkey = From,
    nonce = Nonce,
    sortition_id = SID,
    fee = Fee,
    signed_waiver = SignedWaiver,
    script_sig = SS,
    layer = LN
   } = Tx,
    F28 = forks:get(28),
    true = NewHeight > F28,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(A2, Dict),

    Waiver = testnet_sign:data(SignedWaiver),
    #waiver{
             pubkey = Loser,
             pubkey2 = Loser2,
             sortition_id = SID,%for the correct sortition chain.
             contract = Contract
           } = Waiver,
    true = testnet_sign:verify_sig(Waiver, SignedWaiver#signed.sig, Loser),
    case Loser2 of
        <<0:520>> -> ok;
        P2 ->
            true = testnet_sign:verify_sig(Waiver, SignedWaiver#signed.sig2, P2)
    end,
    %true = testnet_sign:verify(SignedWaiver),


    S = sortition:dict_get(SID, Dict2),
    #sortition{
                top_candidate = TCID_0
              } = S,
    TCID = sortition_claim_tx:layer_salt(TCID_0, LN),
    TC = candidates:dict_get(TCID, Dict2),
    #candidate{
                next_candidate = NC,
                winner = Loser,%matches with who signed the waiver.
                winner2 = Loser2%matches with who signed the waiver.
              } = TC,
    S2 = S#sortition{
           top_candidate = NC,
           last_modified = NewHeight,
           many_candidates = S#sortition.many_candidates - 1
          },
    Dict3 = sortition:dict_write(S2, Dict2),

    Funs = governance:dict_get_value(fun_limit, Dict3),
    Vars = governance:dict_get_value(var_limit, Dict3),
    OpGas = governance:dict_get_value(time_gas, Dict3),
    RamGas = governance:dict_get_value(space_gas, Dict3),
    Evidence = spk:prove_facts(SS#ss.prove, 
                               Dict3, 
                               NewHeight),
    State = chalang:new_state(NewHeight, 0, 0),
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, SS#ss.code, Contract, State, constants:hash_size(), 2, false),
    Contract2 = <<Evidence/binary,
                  Contract/binary>>,

    case chalang:run5(SS#ss.code, Data) of
        {error, Error1} -> 
            io:fwrite("in sortition waiver tx, script sig has an error\n"),
            Dict2;
        Data2 ->
            case chalang:run5(Contract2, Data2) of
                {error, _} -> 
                    io:fwrite("in sortition waiver tx, contract has an error\n"),
                    Dict2;
                Data3 ->
                    case chalang:stack(Data3) of
                        [<<1:32>>|_] -> Dict3;
                        _ -> Dict2
                    end
            end
    end.

    %TODO. similar to spk:chalang_error_handling, if the contract is invalid or takes too much time, we need to handle that case. the tx should still be valid, we should still charge the fee, but don't update any sortition chain or claims.
%Data2 = chalang:run5(SS#ss.code, Data),
%    Data3 = chalang:run5(Contract2, Data2),
%    [<<1:32>>|_] = chalang:stack(Data3), %check that the smart contract in the waiver returns true.

%    Dict3.
