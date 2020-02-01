-module(sortition_claim_tx).
-export([go/4, make_dict/5]).
-include("../../records.hrl").
%-record(sortition_claim_tx, {winner, nonce, fee, sortition_id, proof}).

make_dict(Creator, SID, EID, Proof, Fee) ->
    Acc = trees:get(accounts, Creator),
    #sortition_claim_tx{winner = Creator, nonce = Acc#acc.nonce + 1, sortition_id = SID, fee = Fee, proof = Proof, evidence_id = EID}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #sortition_claim_tx{
    winner = From,
    nonce = Nonce,
    sortition_id = SID,
    fee = Fee,
    evidence_id = EID,
    proof = Proof
   } = Tx,
    S = sortition:dict_get(SID, Dict),
    #sortition{
                rng_value = RNGV,
                top_candidate = TCID,
                validators = ValidatorsRoot
              } = S,
    false = (RNGV == <<0:256>>),%the rng value has been supplied.
    E = existence:dict_get(EID, Dict),
    #exist{
                hash = EHash,
                height = NewClaimHeight
             } = E,
    TC = candidate:dict_get(TCID, Dict),
    #candidate{
                height = OldClaimHeight
              } = TC,
    true = NewClaimHeight < OldClaimHeight,%you can only do this if your new candidate will have the highest priority.
    %prove the height at which you had a claim.
    % -show that all the validators has signed ehash.
    % -use EHash to prove that you were given control of the winning part of the sortition chain.
    %  * show that ehash is the root of a merkel tree containing the fact that this pubkey contols the winning portion.
    %  * unlock any smart contract needed to receive ownership.


    %add to the head of the list of candidates, with height "newclaimheight".
    %you pay a safety deposit.
   ok. 
