-module(sortition_claim_tx).
-export([go/4, make_dict/11]).
-include("../../records.hrl").

make_dict(From, Winner, SID, EID, Proof, Validators, Sigs, OR, ClaimID, ID, Fee) ->
    Acc = trees:get(accounts, From),
    #sortition_claim_tx{from = From, winner = Winner, nonce = Acc#acc.nonce + 1, sortition_id = SID, fee = Fee, proof = Proof, evidence_id = EID, validators = Validators, signatures = Sigs, ownership_root = OR, claim_id = ClaimID, id = ID}.

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
    ownership_root = OwnershipRoot,
    claim_id = ClaimID,
    id = ID
   } = Tx,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce), %you pay a safety deposit.
    Dict2 = accounts:dict_write(A2, Dict),
    S = sortition:dict_get(SID, Dict),
    #sortition{
                top_rng = TRNG,
                bottom_rng = BRNG,
                rng_value = RNGV,
                top_candidate = TCID,
                validators = ValidatorsRoot
              } = S,
    false = (RNGV == <<0:256>>),%the rng value has been supplied.
    E = sortition_blocks:dict_get(EID, Dict),
    #sortition_block{
                      state_root = OwnershipRoot,
                      validators = ValidatorsRoot,
                      sid = SID,
                      height = NewClaimHeight
             } = E,
    TC = candidate:dict_get(TCID, Dict),
    #candidate{
                height = OldClaimHeight
              } = TC,
    true = NewClaimHeight < OldClaimHeight,%you can only do this tx if your new candidate will have the highest priority.
    ValidatorsRoot = sortition_new_tx:make_root(Validators),%this shows that this list of validators must be correct.
    EHash = hash:doit([OwnershipRoot, Sigs]),%Ehash should be the root of a merkle tree. show that stored in the key SID, is the value {ownershiproot, sigsroot} TODO
    true = verify_all(Validators, Sigs, OwnershipRoot),


    %update proof of existence tx type.
    %a bunch of validators can all sign a single {SID, ownership_root_hash, integer} object
    %we store the merkle root of the list of validator along with the object.
    %key is generated from hash({validators_root_hash, integer}), that way the same group of validators can't agree to different ownership roots in the same sortition chain at the same side-chain-block height.


    % -use OwnershipRoot to prove that you were given control of the winning part of the sortition chain.
    %  * show that OwnershipRoot is the root of a merkel tree containing the fact that this pubkey contols the winning portion.
    %  * unlock any smart contract needed to receive ownership.


    S2 = S#sortition{
           top_candidate = ClaimID
          },
    Dict3 = sortition:dict_write(S2, Dict2),
    NC = candidate:new(ID, SID, 0, Winner, NewClaimHeight, TCID),%this will need to be a list of candidates eventually.
    Dict4 = candidate:dict_write(NC, Dict3).

%load_candidates(CID0, TCID, LN
verify_all([],[],_) -> true;
verify_all([V|VT], [S|ST], R) -> 
    A = sign:verify_sig(R, S, V),
    A and verify_all(VT, ST, R).
