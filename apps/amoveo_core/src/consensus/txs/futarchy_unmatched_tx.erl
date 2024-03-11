-module(futarchy_unmatched_tx).
-export([go/4, make_dict/3]).

% if you had an unmatched trade when the futarchy resolve tx happened, this is how you get your money out.
% once the futarchy market is inactive, all the unmatched trades in both the reverted and unreverted branches get converted back to veo.
-include("../../records.hrl").

make_dict(Pubkey, UID, Fee) ->
    Unmatched = trees:get(futarchy_unmatched, UID),
    #futarchy_unmatched
        {
          futarchy_id = FID, owner = _Owner, decision = D, 
          goal = G, revert_amount = Amount
        } = Unmatched,
    Account = trees:get(accounts, Pubkey),
    #futarchy_unmatched_tx
        {
          pubkey = Pubkey, nonce = Account#acc.nonce+1,
          fee = Fee, fid = FID, bet_id = UID, amount = Amount
        }.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #futarchy_unmatched_tx{
    pubkey = Pubkey, nonce = Nonce0, fee = Fee, amount = Amount,
    fid = FID, bet_id = UID
   } = Tx,
    Nonce = nonce_check:doit(
              NonceCheck, 
              Nonce0),
    Acc = accounts:dict_update(
            Pubkey, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Acc, Dict),
    Futarchy = futarchy:dict_get(FID, Dict2),
    #futarchy
        {
          active = 0
        } = Futarchy,
    Unmatched = futarchy_unmatched:dict_get(UID, Dict2),
    #futarchy_unmatched
        {
          futarchy_id = FID, owner = Owner, revert_amount = Amount
        } = Unmatched,
    true = Amount > 0,
    OwnerAccount = accounts:dict_update(
                     Owner, Dict2, Amount, none),
    Dict3 = accounts:dict_write(OwnerAccount, Dict2),
   
    Unmatched2 = Unmatched#futarchy_unmatched{revert_amount = 0},
    Dict4 = futarchy_unmatched:dict_write(Unmatched2, Dict3).
    
    
