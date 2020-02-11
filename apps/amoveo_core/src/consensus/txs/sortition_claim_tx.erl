-module(sortition_claim_tx).
-export([go/4, make_dict/9, make_proofs/1]).
-include("../../records.hrl").

-record(owner, {pubkey, contract}).
-record(owner_layer, {sortition_id, proof, sortition_block_id, validators_root, ownership}).

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
    OL = #owner_layer{sortition_id = SID, proof = Proof, sortition_block_id = EID, validators_root = VR, ownership = Ownership},
    #sortition_claim_tx{from = From, nonce = Acc#acc.nonce + 1, 
                        fee = Fee, 
                        %ownership = Ownership, 
                        claim_id = ClaimID, sortition_id = SID,
                        top_candidate = TCID, proof_layers = [OL]}.
%sortition_id, Proof, evidence_id, validators_root will all need to become lists.
%maybe we should store them in groups of 4 together.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #sortition_claim_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
    %ownership = Ownership,
    claim_id = ClaimID,
    top_candidate = TCID,
    proof_layers = ProofLayers,
    sortition_id = SID
   } = Tx,
    A2 = accounts:dict_update(From, Dict, -Fee, Nonce), %you pay a safety deposit.
    Dict2 = accounts:dict_write(A2, Dict),
    S = sortition:dict_get(SID, Dict2),
    #sortition{
                rng_value = RNGValue,
                top_candidate = TCID,
                validators = ValidatorsRoot
              } = S,
    false = (RNGValue == <<0:256>>),%the rng value has been supplied

    Dict3 = sortition_recursion(0, ProofLayers, ClaimID, RNGValue, TCID, Dict2),

    S2 = S#sortition{
           top_candidate = ClaimID,
           last_modified = NewHeight
          },
    Dict4 = sortition:dict_write(S2, Dict3).


sortition_recursion(_, [], _, _, _, Dict) -> Dict;
sortition_recursion(LayerNumber, [OL|T], ClaimID, RNGValue, TCID, Dict2) ->
    LayerClaimID = layer_salt(ClaimID, LayerNumber),%TODO: salt with layer number
    #owner_layer{
                  sortition_id = SID,
                  proof = Proof,
                  sortition_block_id = EID,
                  validators_root = ValidatorsRoot,
                  ownership = Ownership
                } = OL,
    %TODO: if not last layer, ownership.pubkey = <<0:(65*8)>>, and ownership.sortition_id is new validators_root
    %this connects the layers together, the proof of one points to the root of the validators which we use to verify proofs for the next layer.
    E = sortition_blocks:dict_get(EID, Dict2),
    #sortition_block{
                      state_root = OwnershipRoot,
                      validators = ValidatorsRoot,
                      height = NewClaimHeight
             } = E,
    <<Pstart:256>> = ownership:pstart(Ownership),
    <<PV:256>> = RNGValue,
    <<Pend:256>> = ownership:pend(Ownership),
    true = Pstart =< PV,
    true = PV < Pend,
    SID = ownership:sid(Ownership),
    OwnershipRoot = ownership:verify(Ownership, Proof),
    empty = candidates:dict_get(LayerClaimID, Dict2),
    Priority = ownership:priority(Ownership),
    Winner = ownership:pubkey(Ownership),
    NC = candidates:new(LayerClaimID, SID, LayerNumber, Winner, NewClaimHeight, Priority, TCID),%this will need to be a list of candidates eventually.
    Dict3 = candidates:dict_write(NC, Dict2),
    case TCID of
        <<0:256>> -> ok;
        _ ->
            TCID2 = layer_salt(TCID, LayerNumber),
            TC = candidates:dict_get(TCID2, Dict3),
            #candidate{
                        height = CH,
                        priority = CP
                      } = TC,
            true = ((NewClaimHeight * 256) + 
                        Priority) < 
                ((CH*256) + CP)%you can only do this tx if your new candidate will have the highest priority.
    end, 
    sortition_recursion(LayerNumber + 1,
                        T,
                        ClaimID,
                        RNGValue,
                        TCID, 
                        Dict3).

layer_salt(ClaimID, 0) ->
    ClaimID.
