-module(sortition_evidence_tx).
-export([go/4, make_dict/6, make_waiver/4]).
-include("../../records.hrl").
%-record(sortition_evidence_tx, {pubkey, nonce, fee, sortition_id, signed_waiver}).
%-record(waiver, {pubkey, sortition_id, contract}).

make_waiver(Who, Pub2, SID, Contract) ->
    #waiver{pubkey = Who,
            pubkey2 = Pub2, 
            sortition_id = SID,
            contract = Contract}.

make_dict(From, Fee, SID, LN, SignedWaiver, SS) ->
    Acc = trees:get(accounts, From),
    #sortition_evidence_tx{pubkey = From, nonce = Acc#acc.nonce + 1, sortition_id = SID, fee = Fee, signed_waiver = SignedWaiver, script_sig = SS, layer = LN}.
    
go(Tx, Dict, NewHeight, NonceCheck) ->
    F28 = forks:get(28),
    true = NewHeight > F28,
    #sortition_evidence_tx{
    pubkey = From,
    nonce = Nonce,
    sortition_id = SID,
    fee = Fee,
    signed_waiver = SignedWaiver,
    script_sig = SS,
    layer = LN
   } = Tx,
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
    false = (Loser == <<0:520>>),
    %TODO, sortition_evidence_tx can also refer to any layer built on this candidate, if the sortition chain has recursive child sortition chains.
    S2 = S#sortition{
           top_candidate = NC,
           last_modified = NewHeight
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
    Data2 = chalang:run5(SS#ss.code, Data),
    Data3 = chalang:run5(Contract2, Data2),
    [<<1:32>>|_] = chalang:stack(Data3), %check that the smart contract in the waiver returns true.

    Dict3.
