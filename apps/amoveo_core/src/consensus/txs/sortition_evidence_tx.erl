-module(sortition_evidence_tx).
-export([go/4, make_dict/5, make_waiver/3]).
-include("../../records.hrl").
%-record(sortition_evidence_tx, {pubkey, nonce, fee, sortition_id, signed_waiver}).
%-record(waiver, {pubkey, sortition_id, contract}).

make_waiver(Who, SID, Contract) ->
    #waiver{pubkey = Who,
            sortition_id = SID,
            contract = Contract}.

make_dict(From, Fee, SID, SignedWaiver, SS) ->
    Acc = trees:get(accounts, From),
    #sortition_evidence_tx{pubkey = From, nonce = Acc#acc.nonce + 1, sortition_id = SID, fee = Fee, signed_waiver = SignedWaiver, script_sig = SS}.
    
go(Tx, Dict, NewHeight, NonceCheck) ->
    #sortition_evidence_tx{
    pubkey = From,
    nonce = Nonce,
    sortition_id = SID,
    fee = Fee,
    signed_waiver = SignedWaiver,
    script_sig = SS
   } = Tx,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(A2, Dict),

    true = testnet_sign:verify(SignedWaiver),
    Waiver = testnet_sign:data(SignedWaiver),
    #waiver{
             pubkey = Loser,
             sortition_id = SID,%for the correct sortition chain.
             contract = Contract
           } = Waiver,


    S = sortition:dict_get(SID, Dict2),
    #sortition{
                top_candidate = TCID
              } = S,

    TC = candidates:dict_get(TCID, Dict2),
    #candidate{
                next_candidate = NC,
                winner = Loser%matches with who signed the waiver.
              } = TC,
    
    S2 = S#sortition{
           top_candidate = NC,
           last_modified = NewHeight
          },
    Dict3 = sortition:dict_write(S2, Dict2),

    OpGas = 10000,
    RamGas = 10000,
    Vars = 1000,
    Funs = 1000,
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
