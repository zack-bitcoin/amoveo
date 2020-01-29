
-module(sortition_claim_tx).
-export([go/4, make_dict/4]).
-include("../../records.hrl").
%-record(sortition_new_tx, {creator, nonce, fee, amount, sortition_id, expiration}).

make_dict(Creator, Amount, SID, Fee) ->
    Acc = trees:get(accounts, Creator),
    #sortition_new_tx{creator = Creator, nonce = Acc#acc.nonce + 1, amount = Amount, id = SID, fee = Fee}.

go(Tx, Dict, NewHeight, NonceCheck) ->
   ok. 
