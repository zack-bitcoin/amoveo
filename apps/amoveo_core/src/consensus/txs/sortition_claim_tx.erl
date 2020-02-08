-module(sortition_claim_tx).
-export([go/4, make_dict/9]).
-include("../../records.hrl").

-record(owner, {pubkey, contract}).

make_dict(From, SID, EID, Proof, VR, Ownership, ClaimID, TCID, Fee) ->
    Acc = trees:get(accounts, From),
    #sortition_claim_tx{from = From, nonce = Acc#acc.nonce + 1, sortition_id = SID, fee = Fee, proof = Proof, evidence_id = EID, validators_root = VR, ownership = Ownership, claim_id = ClaimID, top_candidate = TCID}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #sortition_claim_tx{
    from = From,
    nonce = Nonce,
    sortition_id = SID,
    fee = Fee,
    evidence_id = EID,
    proof = Proof,
    validators_root = ValidatorRoot,
    ownership = Ownership,
    claim_id = ClaimID,
    top_candidate = TCID
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
    Winner = ownership:pubkey(Ownership),
    OldClaimHeight = 
        case TCID of
            <<0:256>> -> none;%integers are always less than atoms.
            _ ->
                TC = candidate:dict_get(TCID, Dict),
                #candidate{
                            height = CH,
                            priority = CP
                          } = TC,
                %TC#candidate.height
                (CH*256) + CP
        end,
    Priority = ownership:priority(Ownership),
    true = ((NewClaimHeight*256) + Priority) < OldClaimHeight,%you can only do this tx if your new candidate will have the highest priority.
    <<Pstart:256>> = ownership:pstart(Ownership),
    <<PV:256>> = RNGValue,
    <<Pend:256>> = ownership:pend(Ownership),
    true = Pstart =< PV,
    true = PV < Pend,

    OwnershipRoot = ownership:verify(Ownership, Proof),

    S2 = S#sortition{
           top_candidate = ClaimID
          },
    Dict3 = sortition:dict_write(S2, Dict2),
    NC = candidates:new(ClaimID, SID, 0, Winner, NewClaimHeight, Priority, TCID),%this will need to be a list of candidates eventually.
    Dict4 = candidates:dict_write(NC, Dict3).

