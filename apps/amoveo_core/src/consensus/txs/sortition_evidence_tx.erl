-module(sortition_evidence_tx).
-export([go/4, make_dict/5]).
-include("../../records.hrl").
%-record(sortition_evidence_tx, {pubkey, nonce, fee, sortition_id, signed_waiver}).
make_dict(From, Fee, SID, Loser, SignedWaiver) ->
    Acc = trees:get(accounts, From),
    #sortition_evidence_tx{pubkey = From, nonce = Acc#acc.nonce + 1, sortition_id = SID, fee = Fee, signed_waiver = SignedWaiver}.
    
go(Tx, Dict, NewHeight, NonceCheck) ->
    #sortition_evidence_tx{
    pubkey = From,
    nonce = Nonce,
    sortition_id = SID,
    fee = Fee,
    signed_waiver = SW
   } = Tx,
    Reward = governance:dict_get_value(?MODULE, Dict)*2,
    A2 = accounts:dict_update(From, Dict, Reward-Fee, Nonce),
    Dict2 = accounts:dict_write(A2, Dict),
    %verify that the waiver was signed by who is currently first in line to win the sortition chain lottery.
    %verify that the waiver is for the correct sortition ID.
    %check that the smart contract in the waiver returns true.
    %remove them from being first in line. Make sure they can't re-submit the same evidence.
    %give a reward to whoever made the evidence tx, destroying a bigger deposit from whoever made the sortition_claim tx.

    Ownership = ok,
    Contract = ok,
    NewHeight = ok,
    Evidence = ok,
    CH = ownership:contract(Ownership),
    CH = hash:doit(Contract),
    OpGas = 10000,
    RamGas = 10000,
    Vars = 1000,
    Funs = 1000,
    State = chalang:new_state(NewHeight, 0, 0),
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, Evidence, Contract, State, constants:hash_size(), 2, false),
    Data2 = chalang:run5(Evidence, Data),
    Data3 = chalang:run5(Contract, Data2),
    [<<1:32>>|_] = chalang:stack(Data3),
    ok.
