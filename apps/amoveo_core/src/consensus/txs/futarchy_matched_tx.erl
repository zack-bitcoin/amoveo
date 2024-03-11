-module(futarchy_matched_tx).
-export([go/4, make_dict/4]).

%* if you had made a matched trade in the order book that did not get reverted, this is how you convert your money into a subcurrency in a binary market who's result is determined by the goal oracle.

-include("../../records.hrl").

make_dict(Pubkey, MID, Revert, Fee) ->
    case Revert of
        1 -> ok;
        0 -> ok
    end,
    Matched = trees:get(futarchy_matched, MID),
    #futarchy_matched
        {
          futarchy_id = FID,
          win_amount = Amount,
          owner = Owner
        } = Matched,
    Futarchy = trees:get(futarchy, FID),
    {CID, OID} = futarchy_resolve_tx:fid2cid(Futarchy),
    Account = trees:get(account, Pubkey),
    #futarchy_matched_tx
        {
          pubkey = Pubkey, nonce = Account#acc.nonce+1,
          fee = Fee, amount = Amount, bet = MID, revert = Revert
        }.
go(Tx, Dict, NewHeight, NonceCheck) ->
    #futarchy_matched_tx{
    pubkey = Pubkey, nonce = Nonce0, fee = Fee, amount = Amount,
    bet = MID, revert = Revert
   } = Tx,
    
    Nonce = nonce_check:doit(
              NonceCheck, 
              Nonce0),
    Matched = trees:get(futarchy_matched, MID),
    #futarchy_matched
        {
          futarchy_id = FID,
          owner = Owner,
          decision = Decision,
          revert_amount = RevertAmount,
          win_amount = WinAmount,
          goal = Goal
        } = Matched,
    true = (WinAmount > 0) or (RevertAmount > 0),
    Futarchy = trees:get(futarchy, FID),
    {CID, OID, _} = futarchy_resolve_tx:fid2cid(Futarchy),
    #futarchy{
               active = 0
             } = Futarchy,
    Oracle = trees:get(oracle, OID),
    #oracle{
             result = OracleResult
           } = Oracle,
    case Decision of
        1 -> OracleResult = 1;
        0 -> OracleResult = 2
    end,
    Acc = accounts:dict_update(
            Pubkey, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Acc, Dict),

    Reverted = not(((Decision == 1) and (OracleResult == 1)) 
                   or ((Decision == 0) and (OracleResult == 2))),

    Dict3 = if
                Reverted ->
                    %bet was reverted, so pay them back the original veo they had bet with.
                    Amount = RevertAmount,
                    Revert = 1,
                    OwnerAcc = accounts:dict_update(
                                 Owner, Dict2, RevertAmount, 
                                 none),
                    accounts:dict_write(OwnerAcc, Dict2);
                true ->
                    %bet was matched, and not reverted. So give them the subcurrency that they had purchased.
                    Amount = WinAmount,
                    Revert = 0,
                    futarchy_resolve_tx:spend_or_create_sub(
                      Dict2, Owner, CID, WinAmount, Decision)
            end,
    Matched2 = Matched#futarchy_matched{
                 win_amount = 0,
                 revert_amount = 0
                },
    Dict4 = futarchy_matched:dict_write(
              Matched2, Dict3).
