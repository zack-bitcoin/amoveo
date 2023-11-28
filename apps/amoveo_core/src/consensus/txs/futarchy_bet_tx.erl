-module(futarchy_bet_tx).
-export([go/4, make_dict/7, set_orders/4, orders/3]).

-include("../../records.hrl").

make_dict(Pubkey, FID, Decision, Goal, 
          Amount, LimitPrice, Fee) ->
    Account = trees:get(accounts, Pubkey),
    <<_:256>> = FID,
    case Decision of
        0 -> ok;
        1 -> ok
    end,
    case Goal of
        0 -> ok;
        1 -> ok
    end,
    true = is_integer(Amount),
    true = (Amount > 0),
    true = is_integer(LimitPrice),
    true = (LimitPrice > 0),
    #futarchy_bet_tx{
             pubkey = Pubkey,
             nonce = Account#acc.nonce+1,
             fee = Fee, fid = FID,
             limit_price = LimitPrice,
             amount = Amount,
             decision = Decision,
             goal = Goal}.
orders(1, 1, F) -> F#futarchy.true_yes_orders;
orders(1, 0, F) -> F#futarchy.true_no_orders;
orders(0, 1, F) -> F#futarchy.false_yes_orders;
orders(1, 1, F) -> F#futarchy.false_no_orders.

set_orders(1, 1, F, TID) ->
    F#futarchy{true_yes_orders = TID};
set_orders(1, 0, F, TID) ->
    F#futarchy{true_no_orders = TID};
set_orders(0, 1, F, TID) ->
    F#futarchy{false_yes_orders = TID};
set_orders(0, 0, F, TID) ->
    F#futarchy{false_no_orders = TID}.

                   
go({signed, Tx, _Sig, {TIDAhead, TIDBehind}}, 
   Dict, NewHeight, NonceCheck) ->
    #futarchy_bet_tx{
    pubkey = Pubkey, fid = FID, nonce = Nonce0,
    fee = Fee, limit_price = LimitPrice,
    amount = Amount, decision = Decision, 
    goal = Goal} = Tx,
    true = NewHeight > (forks:get(54)),
    case Goal of
        1 -> ok;
        0 -> ok
    end,
    case Decision of
        1 -> ok;
        0 -> ok
    end,
    true = (Amount > 0),
    true = (LimitPrice > 0),
    true = is_integer(Amount),
    true = is_integer(LimitPrice),
    Nonce = nonce_check:doit(
              NonceCheck, 
              Nonce0),
    Acc = accounts:dict_update(
             Pubkey, Dict, 
            -Fee - Amount, 
            Nonce),
    Dict2 = accounts:dict_write(Acc, Dict),
    Futarchy = futarchy:dict_get(FID, Dict2),
    #futarchy{active = 1, last_batch_height = LUH} =
        Futarchy,
    NewFU0 = #futarchy_unmatched{
      owner = Pubkey,
      futarchy_id = FID,
      decision = Decision,
      goal = Goal,
      revert_amount = Amount,
      limit_price = LimitPrice,
      ahead = TIDAhead,
      behind = TIDBehind
     },
    NewFU = futarchy_unmatched:make_id(NewFU0, NewHeight),
    TID = NewFU#futarchy_unmatched.id,
    empty = futarchy_unmatched:dict_get(TID, Dict2),
    Dict3 = futarchy_unmatched:dict_write(
              NewFU, Dict2),
    Dict4 = 
        case TIDAhead of
            <<0:256>> ->
                <<0:256>> = orders(Decision, Goal, Futarchy),
                Futarchy2 = set_orders(Decision, Goal, Futarchy, TID),
                futarchy:dict_write(Futarchy2, Dict3);
            <<_:256>> ->
                BetAhead = 
                    futarchy_unmatched:dict_get(TIDAhead, Dict2),
                #futarchy_unmatched{
                   goal = Goal, decision = Decision, 
                   limit_price = PriceA} = BetAhead,
                TIDBehind = BetAhead#futarchy_unmatched.behind,
                true = PriceA >= LimitPrice,
                BetAhead2 = 
                    BetAhead#futarchy_unmatched{
                      behind = TID
                     },
                futarchy_unmatched:dict_write(
                  BetAhead2, Dict3)
        end,

    Dict5 = 
        case TIDBehind of
            <<0:256>> -> Dict4;
            <<_:256>> ->
                BetBehind = 
                    futarchy_unmatched:dict_get(
                      TIDBehind, Dict2),
                #futarchy_unmatched{
                    goal = Goal, decision = Decision,
                    limit_price = PriceB} = BetBehind,
                TIDAhead = BetBehind#futarchy_unmatched.ahead,
                true = LimitPrice > PriceB,
                BetBehind2 = 
                    BetBehind#futarchy_unmatched{
                      ahead = TID
                     },
                futarchy_unmatched:dict_write(
                  BetBehind2, Dict4)
        end,
    Dict6 = case LUH of
                0 ->
                    Futarchy3 = futarchy:dict_get(FID, Dict5),
                    Futarchy4 = Futarchy3#futarchy{last_batch_height = NewHeight},
                    futarchy:dict_write(Futarchy4, Dict5);
                _ -> Dict5
            end,
    Dict6.
