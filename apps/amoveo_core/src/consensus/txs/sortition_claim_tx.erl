-module(sortition_claim_tx).
-export([go/4, make_dict/6, make_proofs/1, make_owner_layer/5, layer_salt/2]).
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

make_owner_layer(SID, Proof, EID, VR, Ownership) ->
    #owner_layer{sortition_id = SID, proof = Proof, sortition_block_id = EID, validators_root = VR, ownership = Ownership}.

make_dict(From, L, SID, ClaimID, TCID, Fee) ->
    Acc = trees:get(accounts, From),
    %OL = #owner_layer{sortition_id = SID, proof = Proof, sortition_block_id = EID, validators_root = VR, ownership = Ownership},
    S = trees:get(sortition, SID),
    TCID2 = S#sortition.top_candidate,
    io:fwrite(packer:pack([TCID, TCID2])),
    io:fwrite("\n"),
    TCID = TCID2,
    #sortition_claim_tx{from = From, nonce = Acc#acc.nonce + 1, 
                        fee = Fee, 
                        claim_id = ClaimID, sortition_id = SID,
                        top_candidate = TCID, proof_layers = L}.
%sortition_id, Proof, evidence_id, validators_root will all need to become lists.
%maybe we should store them in groups of 4 together.

go(Tx, Dict, NewHeight, NonceCheck) ->
    F28 = forks:get(28),
    true = NewHeight > F28,
    #sortition_claim_tx{
    from = From,
    nonce = Nonce,
    fee = Fee,
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
    true = priority_check(TCID, 0, ProofLayers, Dict2),
    Dict3 = merkle_verify(0, ProofLayers, ClaimID, RNGValue, TCID, ValidatorsRoot, Dict2),%creates the candidates for this claim.
    S2 = S#sortition{
           top_candidate = ClaimID,
           last_modified = NewHeight
          },
    Dict4 = sortition:dict_write(S2, Dict3).
priority_check(<<0:256>>, _, _, _) -> true;
priority_check(TCID, LayerNumber, [H|T], Dict2) ->
    #owner_layer{
                sortition_id = _SID,
                proof = _Proof,
                sortition_block_id = EID,
                validators_root = ValidatorsRoot,
                ownership = Ownership
               } = H,
    TCID2 = layer_salt(TCID, LayerNumber),
    TC = candidates:dict_get(TCID2, Dict2),
    #candidate{
                height = CH,
                priority = CP
              } = TC,
    E = sortition_blocks:dict_get(EID, Dict2),
    #sortition_block{
                      state_root = _OwnershipRoot,
                      validators = ValidatorsRoot,
                      height = NewClaimHeight
             } = E,
    P1 = (NewClaimHeight * 256) + ownership:priority(Ownership),
    P2 = (CH*256) + CP,
    if
        P1 == P2 -> priority_check(TCID, LayerNumber+1, T, Dict2);
        P1 < P2 -> true;
        true -> false
    end.
    %you can only do this tx if your new candidate will have the highest priority.

merkle_verify(_, [], _, _, _, _, Dict) -> 
    Dict;
merkle_verify(LayerNumber, [OL|T], ClaimID, RNGValue, TCID, ValidatorsRoot, Dict2) ->
    %also creates the candidates.
    LayerClaimID = layer_salt(ClaimID, LayerNumber),
    #owner_layer{
                  sortition_id = SID,
                  proof = Proof,
                  sortition_block_id = EID,
                  validators_root = ValidatorsRoot,
                  ownership = Ownership
                } = OL,
    NextVR = case T of
                 [] -> false = (ownership:pubkey(Ownership) == <<0:520>>);
                 _ -> true = ownership:pubkey(Ownership) == <<0:520>>,
                      ownership:sid(Ownership)%this connects the layers together, the proof of one points to the root of the validators which we use to verify proofs for the next layer.
             end,

%    NextVR = if
%                 not(T == []) ->
%                     true = ownership:pubkey(Ownership) == 
%                         <<0:520>>,
%                     ownership:sid(Ownership); 
                 
%                     false = (ownership:pubkey(Ownership) == <<0:520>>)
%                 true -> ok
%             end,
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
    %SID = ownership:sid(Ownership),
    OwnershipRoot = ownership:verify(Ownership, Proof),
    empty = candidates:dict_get(LayerClaimID, Dict2),
    Priority = ownership:priority(Ownership),
    Winner = ownership:pubkey(Ownership),
    Winner2 = ownership:pubkey2(Ownership),
    Contracts2 = ownership:contracts(Ownership),
    %TODO, put this list of contracts into a merkle tree, and store the root in the candidate.
    Size = 32,
    KeyLength = 2,
    M = mtree:new_empty(KeyLength, Size, 0),
    CFG = mtree:cfg(M),
    L = make_leaves(0, Contracts2, CFG),
    RH = case L of
             [] -> <<0:256>>;
             _ -> 
                 {Root, M2} = 
                     mtree:store_batch(L, 1, M),
                 mtree:root_hash(Root, M2)
         end,
    %RH = mtree:root_hash(Root, M2),
    NC = candidates:new(LayerClaimID, SID, LayerNumber, Winner, Winner2, NewClaimHeight, Priority, TCID, RH),
    Dict3 = candidates:dict_write(NC, Dict2),
    merkle_verify(
      LayerNumber + 1,
      T,
      ClaimID,
      RNGValue,
      TCID, 
      NextVR,
      Dict3).
make_leaves(_, [], _) -> [];
make_leaves(N, [H|T], CFG) ->
    Leaf = leaf:new(N, H, 0, CFG),
    [Leaf|make_leaves(N+1, T, CFG)].

    

layer_salt(ClaimID, 0) -> ClaimID;
layer_salt(ClaimID, N) -> 
    hash:doit(<<N:32, ClaimID/binary>>).
