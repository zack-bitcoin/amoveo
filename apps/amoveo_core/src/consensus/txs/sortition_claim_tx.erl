-module(sortition_claim_tx).
-export([go/4, make_dict/12]).
-include("../../records.hrl").

-record(owner, {pubkey, contract}).

make_dict(From, Winner, SID, EID, Proof, VR, Ownership, ClaimID, Contract, Evidence, TCID, Fee) ->
    Acc = trees:get(accounts, From),
    #sortition_claim_tx{from = From, winner = Winner, nonce = Acc#acc.nonce + 1, sortition_id = SID, fee = Fee, proof = Proof, evidence_id = EID, validators_root = VR, ownership = Ownership, claim_id = ClaimID, contract = Contract, evidence = Evidence, top_candidate = TCID}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #sortition_claim_tx{
    from = From,
    winner = Winner,
    nonce = Nonce,
    sortition_id = SID,
    fee = Fee,
    evidence_id = EID,
    proof = Proof,
    validators_root = ValidatorRoot,
    ownership = Ownership,
    claim_id = ClaimID,
    contract = Contract,
    top_candidate = TCID,
    evidence = Evidence
   } = Tx,
    SID = ownership:sid(Ownership),
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce), %you pay a safety deposit.
    Dict2 = accounts:dict_write(A2, Dict),
    S = sortition:dict_get(SID, Dict),
    #sortition{
                rng_value = RNGValue,
                top_candidate = TCID,
                validators = ValidatorsRoot
              } = S,
    false = (RNGValue == <<0:256>>),%the rng value has been supplied.
    E = sortition_blocks:dict_get(EID, Dict),
    #sortition_block{
                      state_root = OwnershipRoot,
                      validators = ValidatorsRoot,
                      height = NewClaimHeight
             } = E,
    OldClaimHeight = 
        case TCID of
            <<0:256>> -> none;%integers are always less than atoms.
            _ ->
                TC = candidate:dict_get(TCID, Dict),
                #candidate{
                            height = X
                          } = TC,
                X
        end,
    true = NewClaimHeight < OldClaimHeight,%you can only do this tx if your new candidate will have the highest priority.
    %ValidatorsRoot = sortition_new_tx:make_root(Validators),%this shows that this list of validators must be correct.
    true = ownership:is_between(Ownership, RNGValue),

    %TODO, show that `ownership` is the only entree in the range RNGStart-RNGEnd

    ownership:verify(Ownership, OwnershipRoot, Proof),
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

    S2 = S#sortition{
           top_candidate = ClaimID
          },
    Dict3 = sortition:dict_write(S2, Dict2),
    NC = candidates:new(ClaimID, SID, 0, Winner, NewClaimHeight, TCID),%this will need to be a list of candidates eventually.
    Dict4 = candidates:dict_write(NC, Dict3).

