-module(sortition_claim_tx).
-export([go/4, make_dict/12]).
-include("../../records.hrl").

-record(owner, {pubkey, contract}).

make_dict(From, Winner, SID, EID, Proof, Validators, Sigs, Ownership, ClaimID, Contract, Evidence, Fee) ->
    Acc = trees:get(accounts, From),
    #sortition_claim_tx{from = From, winner = Winner, nonce = Acc#acc.nonce + 1, sortition_id = SID, fee = Fee, proof = Proof, evidence_id = EID, validators = Validators, signatures = Sigs, ownership = Ownership, claim_id = ClaimID, contract = Contract, evidence = Evidence}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #sortition_claim_tx{
    from = From,
    winner = Winner,
    nonce = Nonce,
    sortition_id = SID,
    fee = Fee,
    evidence_id = EID,
    proof = Proof,
    validators = Validators,
    signatures = Sigs,
    ownership = Ownership,
    claim_id = ClaimID,
    contract = Contract,
    evidence = Evidence
   } = Tx,
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
    TC = candidate:dict_get(TCID, Dict),
    #candidate{
                height = OldClaimHeight
              } = TC,
    true = NewClaimHeight < OldClaimHeight,%you can only do this tx if your new candidate will have the highest priority.
    ValidatorsRoot = sortition_new_tx:make_root(Validators),%this shows that this list of validators must be correct.
    true = ownership:is_between(Ownership, RNGValue),
    %one plan is to have a little smart contract at every step of the merkle proof verification.
    ownership:verify(Ownership, OwnershipRoot, Proof),
    CH = ownership:contract(Ownership),
    CH = hash:doit(Contract),
    %contract is an scriptpubkey,
    %evidence is a scriptsig
    {1, _, _} = spk:dict_run(fast, Evidence, Contract, NewHeight, 0, Dict),

    S2 = S#sortition{
           top_candidate = ClaimID
          },
    Dict3 = sortition:dict_write(S2, Dict2),
    NC = candidate:new(ClaimID, SID, 0, Winner, NewClaimHeight, TCID),%this will need to be a list of candidates eventually.
    Dict4 = candidate:dict_write(NC, Dict3).

