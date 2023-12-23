-module(futarchy_bet_tx).
-export([go/4, make_dict/8, set_orders/4, orders/3]).

-include("../../records.hrl").

-define(e2_16, 65536). %this is "one" in a certain perspective. we can store rationals from 1/this to this/1.
-define(e2_32, 4294967296). %the maximum number that can be expressed in thes system. is around 65536.

make_dict(Pubkey, FID, Decision, Goal, 
          Amount, LimitPrice, FutarchyNonce, 
          Fee) ->
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
    %the mining pool should update the meta data to store matched_tids for this bet.
    #futarchy_bet_tx{
             pubkey = Pubkey,
             nonce = Account#acc.nonce+1,
             fee = Fee, fid = FID,
             limit_price = LimitPrice,
             amount = Amount,
             decision = Decision,
             futarchy_nonce = FutarchyNonce,
             goal = Goal}.
orders(1, 1, F) -> F#futarchy.true_yes_orders;
orders(1, 0, F) -> F#futarchy.true_no_orders;
orders(0, 1, F) -> F#futarchy.false_yes_orders;
orders(0, 0, F) -> F#futarchy.false_no_orders.

set_orders(1, 1, F, TID) ->
    F#futarchy{true_yes_orders = TID};
set_orders(1, 0, F, TID) ->
    F#futarchy{true_no_orders = TID};
set_orders(0, 1, F, TID) ->
    F#futarchy{false_yes_orders = TID};
set_orders(0, 0, F, TID) ->
    F#futarchy{false_no_orders = TID}.

go({signed, Tx, _Sig, {0, TIDAhead, TIDBehind}}, 
   Dict, NewHeight, NonceCheck) ->
    %This is the case where your trade is added to the order book.
    ok;
go({signed, Tx, _Sig, {1, TIDsMatched, TIDPartlyMatched}}, 
   Dict, NewHeight, NonceCheck) ->
    %This is the case where your trade is completely matched.
    ok;
                   
go({signed, Tx, _Sig, _}, 
   Dict, NewHeight, NonceCheck) ->
    #futarchy_bet_tx{
    pubkey = Pubkey, fid = FID, nonce = Nonce0,
    fee = Fee, limit_price = LimitPrice,
    amount = Amount, decision = Decision, 
    futarchy_nonce = FutarchyNonce,
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
    #futarchy{active = 1, nonce = FutarchyNonce} =
        Futarchy,

    {Q1, Q2, LMSRBeta} = 
        case Decision of
            1 -> {Futarchy#futarchy.shares_true_yes,
                  Futarchy#futarchy.shares_true_no,
                  Futarchy#futarchy.liquidity_true};
            0 -> {Futarchy#futarchy.shares_false_yes,
                  Futarchy#futarchy.shares_false_no,
                  Futarchy#futarchy.liquidity_false}
        end,

    {OurOrders, TheirOrders} = 
        case {Decision, Goal} of
            {1,1}->{Futarchy#futarchy.true_yes_orders, 
                     Futarchy#futarchy.true_no_orders};
            {1,0}->{Futarchy#futarchy.true_no_orders,
                    Futarchy#futarchy.true_yes_orders};
            {0,1}->{Futarchy#futarchy.false_yes_orders,
                    Futarchy#futarchy.false_no_orders};
            {0,0}->{Futarchy#futarchy.false_no_orders,
                    Futarchy#futarchy.false_yes_orders}
        end,
    {Dict3, AmountMatched} = 
        case match(TheirOrders, OurOrders, Amount, 
                   LimitPrice, Goal, 
                   Dict2, LMSRBeta, Q1, Q2, 
                  Decision) of%this creates the matched trades and puts it in the Dict. It updates the futarchy with the new orders, and because of the more shares purchased.
            {finished, DictA} ->
                {DictA, Amount};
            {more, Amount2, DictC} ->
                %add an unmatched trade to OurOrders
                NewFU0 = #futarchy_unmatched{
                  owner = Pubkey,
                  futarchy_id = FID,
                  decision = Decision,
                  goal = Goal,
                  revert_amount = Amount-Amount2,
                  limit_price = LimitPrice
%                  ahead = TIDAhead,
%                  behind = TIDBehind
                 },
%                {DictB, A
                NewFU = futarchy_unmatched:make_id(NewFU0, NewHeight),
                TID = NewFU#futarchy_unmatched.id,
                empty = futarchy_unmatched:dict_get(TID, DictC),
                {NewOrders, DictD} = insert(NewFU, OurOrders, DictC),
                %todo. update futarchy to point to neworders.
%                 DictD = futarchy_unmatched:dict_write(
%                           NewFU, DictC),
                 {DictD, Amount - Amount2}
                     
            
        end,
   %todo. add a matched trade for the new order that is being created.
    FM = #futarchy_matched{
      owner = Pubkey,
      futarchy_id = FID,
      decision = Decision,
      revert_amount = AmountMatched,
      win_amount = ok
     },

    %increment the futarchy nonce.
    
    ok.

insert(_, _, _) ->
    ok.

match(TheirOrders, OurOrders, Amount, Price0, Goal,
      Dict, Beta, Q1, Q2, Decision) ->
    Price = case Decision of
                1 -> Price0;
                0 -> ?e2_16 - Price0
            end,
    FU =futarchy_unmatched:dict_get(TheirOrders, Dict),
    #futarchy_unmatched{id = FUID,
                        limit_price = LP,
                        ahead = <<0:256>>,
                        behind = NFUID} = FU,
    %todo. figure out what the current lmsr price is. we need to keep pulling liquidity out of the lmsr as we match more trades.
    C = Price * LP div ?e2_16,
    if
        (C > ?e2_16) ->
            %todo we match at least some orders.
            ok;
        true ->
            %we don't match any orders
            {more, Amount, Dict}
    end.
                    






