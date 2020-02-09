-module(sortition_claim_tx).
-export([go/4, make_dict/9, make_proofs/1]).
-include("../../records.hrl").

-record(owner, {pubkey, contract}).
-record(owner_layer, {sortition_id, proof, sortition_block_id, validators_root}).

make_proofs([]) -> [];
make_proofs([X|T]) -> 
    #owner_layer{
             sortition_id = SID,
             sortition_block_id = SBID
            } = X,
    [{sortition, SID},
     {sortition_blocks, SBID}] ++
        make_proofs(T).

make_dict(From, SID, EID, Proof, VR, Ownership, ClaimID, TCID, Fee) ->
    Acc = trees:get(accounts, From),
    OL = #owner_layer{sortition_id = SID, proof = Proof, sortition_block_id = EID, validators_root = VR},
    #sortition_claim_tx{from = From, nonce = Acc#acc.nonce + 1, 
                        fee = Fee, 
                        ownership = Ownership, 
                        claim_id = ClaimID, 
                        top_candidate = TCID, proof_layers = [OL]}.
%sortition_id, Proof, evidence_id, validators_root will all need to become lists.
%maybe we should store them in groups of 4 together.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #sortition_claim_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    ownership = Ownership,
    claim_id = ClaimID,
    top_candidate = TCID,
    proof_layers = ProofLayers
   } = Tx,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce), %you pay a safety deposit.
    Dict2 = accounts:dict_write(A2, Dict),



    [OL] = ProofLayers,
    #owner_layer{
                  sortition_id = SID,
                  proof = Proof,
                  sortition_block_id = EID,
                  validators_root = ValidatorsRoot
                } = OL,
    SID = ownership:sid(Ownership),
    S = sortition:dict_get(SID, Dict2),
    #sortition{
                rng_value = RNGValue,
                top_candidate = TCID,
                validators = ValidatorsRoot
              } = S,
    false = (RNGValue == <<0:256>>),%the rng value has been supplied.
    E = sortition_blocks:dict_get(EID, Dict2),
    #sortition_block{
                      state_root = OwnershipRoot,
                      validators = ValidatorsRoot,
                      height = NewClaimHeight
             } = E,
    OwnershipRoot = ownership:verify(Ownership, Proof),
    empty = candidates:dict_get(ClaimID, Dict2),
    Priority = ownership:priority(Ownership),
    Winner = ownership:pubkey(Ownership),
    NC = candidates:new(ClaimID, SID, 0, Winner, NewClaimHeight, Priority, TCID),%this will need to be a list of candidates eventually.
    Dict3 = candidates:dict_write(NC, Dict2),
    




    OldClaimHeight = 
        case TCID of
            <<0:256>> -> none;%integers are always less than atoms.
            _ ->
                TC = candidates:dict_get(TCID, Dict3),
                #candidate{
                            height = CH,
                            priority = CP
                          } = TC,
                %TC#candidate.height
                (CH*256) + CP
        end,
    true = ((NewClaimHeight*256) + Priority) < OldClaimHeight,%you can only do this tx if your new candidate will have the highest priority.
    <<Pstart:256>> = ownership:pstart(Ownership),
    <<PV:256>> = RNGValue,
    <<Pend:256>> = ownership:pend(Ownership),
    true = Pstart =< PV,
    true = PV < Pend,
    S2 = S#sortition{
           top_candidate = ClaimID,
           last_modified = NewHeight
          },
    sortition:dict_write(S2, Dict3).


