-module(futarchy_bet_tx).
-export([go/4, make_dict/8, set_orders/4, orders/3, hash/1, 
         one/0, one_square/0, prices_match/2]).

-include("../../records.hrl").

-define(e2_16, 65536). %we can store rationals from 1/this to this/1. 2^16
% integer X is rational X/65536. smallest value is 1/65536, biggest is 65536/1.
-define(e2_32, 4294967296). %the maximum number that can be expressed in thes system. 2^32

one() -> ?e2_16.
one_square() -> ?e2_32.

prices_match(A, B) ->
    %if A encodes Ra, and B encodes Rb, then
    % A*B = Ra * Rb * ?e2_32
    % (A*B > ?e2^32) <==> (Ra * Rb > 1)
    (A * B) > ?e2_32.
    

make_dict(Pubkey, FID, Decision, Goal, 
          Amount, LimitPrice, FutarchyHash, 
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
             futarchy_hash = FutarchyHash,
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

go({signed, Tx, _Sig, Proved}, 
   Dict, NewHeight, NonceCheck) ->
    #futarchy_bet_tx{
    pubkey = Pubkey, fid = FID, nonce = Nonce0,
    fee = Fee, limit_price = LimitPrice,
    amount = Amount, decision = Decision, 
    futarchy_hash = RootHash,
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
    #futarchy{active = 1, root_hash = RootHash} =
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
    
    NewUnmatched = 
        #futarchy_unmatched{
      %id, ahead, behind
      owner = Pubkey,
      futarchy_id = FID, decision = Decision,
      goal = Goal, revert_amount = Amount,
      limit_price = LimitPrice},

    Dict3 = 
        case Proved of 
            {0, TIDAhead, TIDBehind} ->
  %This is the case where your trade is added to the order book.
                FBehind=futarchy_unmatched:dict_get(TIDBehind,Dict2),
                FAhead= futarchy_unmatched:dict_get(TIDAhead, Dict2),
                #futarchy_unmatched
                    {
                      ahead = TIDAhead,
                      limit_price = LPBehind,
                      decision = Decision,
                      goal = Goal
                    } = FBehind,
                #futarchy_unmatched
                    {
                      behind = TIDBehind,
                      limit_price = LPAhead,
                      decision = Decision,
                      goal = Goal
                    } = FAhead,
                true = LimitPrice >= LPBehind,
                true = LimitPrice =< LPAhead,
                NewUnmatched2 = 
                    NewUnmatched#futarchy_unmatched{
                      ahead = TIDAhead,
                      behind = TIDBehind},
                NewUnmatched3= futarchy_unmatched:make_id(NewUnmatched2),
                TID = NewUnmatched2#futarchy_unmatched.id, 
                empty = futarchy_unmatched:dict_get(TID, Dict2),
                Dict3a = futarchy_unmatched:dict_write(NewUnmatched3, Dict2),
                FBehind2 = FBehind#futarchy_unmatched{ahead = TID},
                FAhead2 = FAhead#futarchy_unmatched{behind = TID},
                Dict4a = futarchy_unmatched:dict_write(FBehind2, Dict3a),
                Dict5a = futarchy_unmatched:dict_write(FAhead2, Dict4a),
                Dict5a;
            {1, TIDsMatched, TIDPartiallyMatched, Q1b, Q2b} ->
    %This is the case where your trade is completely matched.
                %check that they bought one kind of shares.
                bought_one(Q1, Q2, Q1b, Q2b),
                TheirOrders = hd(TIDsMatched),

                  %remove the tidsmatched from the db.
                {TIDPartiallyMatched, Dict3b, MatchedAmount} = 
                    remove_firsts(TIDsMatched, Dict2),
                
                %check that the worst-priced order is still within the bounds of the price we wanted to pay.
                price_check([TIDPartiallyMatched], 
                            LimitPrice),
                FUb = case TIDPartiallyMatched of
                          <<0:256>> -> #futarchy_unmatched{limit_price = 0};
                          _ -> futarchy_unmatched:dict_get(
                                TIDPartiallyMatched, Dict3b)
                      end,
                #futarchy_unmatched
                    {
                      owner = Pubkey, futarchy_id = FID,
                      decision = D, goal = G,
                      revert_amount = FURA,
                      limit_price = FULP
                     } = FUb,
                LPb = FUb#futarchy_unmatched.limit_price,
                %Q1, Q2 -> Q1b, Q2b
                AD = lmsr:change_in_market(LMSRBeta, Q1, Q2, Q1b, Q2b),
                Rest0 = Amount - AD - MatchedAmount,
                Rest = Rest0,
                false = Rest < 0,
                %Rest = max(0, Rest0),
                %if Rest is >0, then we are partially matching the next trade. otherwise, we finish matching in the market maker's zone.
                Dict3b_5 = 
                    if
                        (Rest > 0) -> 
                            %make sure that a trade exists to match with
                            false = (<<0:256>> == TIDPartiallyMatched),
             %make sure that we can match with this trade
                            %true = (LimitPrice * LPb) > ?e2_32,
                            true = prices_match(LimitPrice, LPb),
                            
       %check that the price in the partially matched order matches with Q1b and Q2b.
                            almost_equal(
                              LPb, lmsr:price(LMSRBeta, 
                                              Q1b, Q2b)),
                            FUb2 = FUb#futarchy_unmatched{
                                     revert_amount = FURA - Rest
                                    },
                            futarchy_unmatched:dict_write(
                              FUb2, Dict3b);
                        true ->
                            %check that the next trade in the market, if it exists, is outside of our price range.
                            %false = (LimitPrice * LPb) > ?e2_32,
                            false = prices_match(LimitPrice, LPb),
                            Dict3b
                    end,
                

                FMID = hash:doit(<<FID/binary, Pubkey/binary, Nonce0:32>>),
                PartMatch = AD + MatchedAmount,
                FMb = #futarchy_matched{
                  id = FMID,
                  owner = Pubkey,
                  futarchy_id = FID,
                  decision = D,
                  goal = G,
                  revert_amount = PartMatch,
                  win_amount = PartMatch * ?e2_16 div FULP
                      },
                empty = futarchy_matched:dict_get(FMID, Dict3b_5),
                Dict4b = futarchy_matched:dict_write(FMb, Dict3b_5),
                Futarchyb = futarchy:dict_get(FID, Dict4b),
                Futarchyb2 = 
                    case Decision of
                        1 -> Futarchyb#futarchy{
                               shares_true_yes = Q1b,
                               shares_true_no = Q2b};
                        0 -> Futarchyb#futarchy{
                               shares_false_yes = Q1b,
                               shares_false_no = Q2b}
                        end,
                Futarchyb3 = 
                    case {Decision, Goal} of
                        {1, 1} -> 
                            Futarchyb2#futarchy{
                              true_no_orders = 
                                  TIDPartiallyMatched};
                        {1, 0} -> 
                            Futarchyb2#futarchy{
                              true_yes_orders = 
                                  TIDPartiallyMatched};
                        {0, 1} -> 
                            Futarchyb2#futarchy{
                              false_no_orders = 
                                  TIDPartiallyMatched};
                        {0, 0} -> 
                            Futarchyb2#futarchy{
                              false_yes_orders = 
                                  TIDPartiallyMatched}
                    end,
                Dict5b = futarchy:dict_write(Futarchyb3, Dict4b),
                Dict5b;
            {2, TIDsMatched, TIDAfterMatched, NewTopTID, FMIDc, Q1b, Q2b} ->
    %This is the case where your trade is partially matched.
                %this is the same as saying that the price went out of range during the lmsr step.
                %check that they bought one kind of shares.
                bought_one(Q1, Q2, Q1b, Q2b),
                TheirOrders = hd(TIDsMatched),

                %check that the worst-priced order still is within the bounds of the price we wanted to pay.
                TIDAfterMatched = price_check(TIDsMatched, LimitPrice),

                  %remove the tidsmatched from the db.
                {_, Dict3c, MatchedAmountc} = 
                    remove_firsts(TIDsMatched, Dict2),

                %check that the next trade in the market is at a worse price than we are willing to match at, or that there is no trade after this one.
                LastFutarchyU = futarchy_unmatched:dict_get(TIDAfterMatched, Dict3c),
                TIDAfterMatched = LastFutarchyU#futarchy_unmatched.behind,
                case TIDAfterMatched of
                    <<0:256>> -> ok;
                    _ ->

                        AfterMatched=futarchy_unmatched:dict_get(
                                       TIDAfterMatched, Dict2),
                        AMLP=AfterMatched#futarchy_unmatched.limit_price,
                        %make sure that we don't match this trade.
                        false = prices_match(LimitPrice, AMLP),
                        %false = (LimitPrice * AMLP) > ?e2_32,
                        ok
                end,

                #futarchy_unmatched
                    {
                  decision = D,
                  goal = G,
                  limit_price = LastPrice
                 } = LastFutarchyU,

                %MatchedAmountc = Amount,
                LMSRPrice = lmsr:price(LMSRBeta, Q1b, Q2b),
                true = (LMSRPrice =< LimitPrice),
                NewTopTID = price_check(TIDsMatched, LMSRPrice),

                Futarchyc2 = case Decision of
                                 1 -> Futarchy#futarchy{
                                        shares_true_yes = Q1b,
                                        shares_true_no = Q2b};
                                 0 -> Futarchy#futarchy{
                                       shares_false_yes = Q1b,
                                       shares_false_no = Q2b}
                             end,
                LMSRAmount = lmsr:change_in_market(LMSRBeta, Q1, Q2, Q1b, Q2b),
                FMIDc = hash:doit(<<FID/binary, Pubkey/binary, NewHeight:32>>),
                PartMatchc = LMSRAmount + MatchedAmountc,
                FMc = #futarchy_matched{
                  id = FMIDc,
                  owner = Pubkey,
                  futarchy_id = FID,
                  decision = D,
                  goal = G,
                  revert_amount = PartMatchc,
                  win_amount = PartMatchc * ?e2_16 div LimitPrice
                 },
                empty = futarchy_id:dict_get(FMIDc, Dict3c),
                Dict3c_5 = futarchy_matched:dict_write(FMc, Dict3c),
                NewUM0 = #futarchy_unmatched{
                  owner = Pubkey, futarchy_id = FID, decision=D,goal=G, 
                  revert_amount = Amount - LMSRAmount - MatchedAmountc, 
                  limit_price = LimitPrice, ahead = <<0:256>>,
                  behind = OurOrders
                 },
                NewUM = futarchy_unmatched:make_id(NewUM0),
                NewTID = NewUM#futarchy_unmatched.id,
                empty = futarchy_unmatched:dict_get(NewTID, Dict3c_5),
                Dict4c = futarchy_unmatched:dict_write(NewUM, Dict3c_5),
                Futarchyc3 = 
                    case {Decision, Goal} of
                        {1, 1} -> Futarchyc2#futarchy{
                                    true_yes_orders = NewTID,
                                    true_no_orders = NewTopTID};
                        {1, 0} -> Futarchyc2#futarchy{
                                    true_no_orders = NewTID,
                                    true_yes_orders = NewTopTID};
                        {0, 1} -> Futarchyc2#futarchy{
                                    false_yes_orders = NewTID,
                                    false_no_orders = NewTopTID};
                        {0, 0} -> Futarchyc2#futarchy{
                                    false_no_orders = NewTID,
                                    false_yes_orders = NewTopTID}
                    end,
                Dict5c = futarchy:dict_write(Futarchyc3, Dict4c),
                Dict5c
        end,
    Dict3.

price_check(TIDsMatched, LimitPrice) ->
    LastTID = hd(lists:reverse(TIDsMatched)),
    LastFutarchyU = 
        futarchy_unmatched:dict_get(LastTID),
    LPc = LastFutarchyU#futarchy_unmatched.limit_price,
    %true = (LimitPrice * LPc) >= ?e2_32,
    true = prices_match(LimitPrice, LPc),
    LastFutarchyU#futarchy_unmatched.behind.%their top
    

almost_equal(A, B) ->
    io:fwrite({A, B}),
    true = (abs(A - B) < 100),
    ok.

hash(F = #futarchy_bet_tx{pubkey = Pub, nonce = Nonce, limit_price = LP, amount = A, decision = D, goal = G, futarchy_hash = FH}) ->
    B = <<Pub/binary, Nonce:32, LP:64, A:64, D, G, FH/binary>>,
    hash:doit(B).

remove_firsts(A, B) ->
    remove_firsts(A, B, 0).
remove_firsts([TID], Dict, Amount) ->
    FU = futarchy_unmatched:dict_get(TID, Dict),
    {Dict2, A} = remove_first(TID, Dict),
    {FU#futarchy_unmatched.behind, Dict2, A+Amount};
remove_firsts([TID1|[TID2|T]], Dict, Amount) ->
    %check that they are connected.
    FU1 = futarchy_unmatched:dict_get(TID1, Dict),
    FU2 = futarchy_unmatched:dict_get(TID2, Dict),
    TID2 = FU1#futarchy_unmatched.behind,
    TID1 = FU2#futarchy_unmatched.ahead,
    {Dict2, A} = remove_first(TID1, Dict),
    Amount2 = A + Amount,
    remove_firsts([TID2|T], Dict2, Amount2).
remove_first(TID, Dict) ->
    %removes first order from the order book, creates a matched trade in it's place.

    %delete the part that got matched.
    FU = futarchy_unmatched:dict_get(TID, Dict),
    #futarchy_unmatched
        {
          ahead = <<0:256>>,
          behind = Behind,
          limit_price = LP,
          owner = Owner,
          futarchy_id = FID,
          decision = D,
          goal = G,
          revert_amount = RA
        } = FU,
    
    FU2 = FU#futarchy_unmatched{
            revert_amount = 0,
            ahead = <<0:256>>,
            behind = <<0:256>>
           },
    Dict2 = futarchy_unmatched:dict_write(FU2, Dict),

    %next create the matched trade.
    <<TIDN:256>> = TID,
    TID1 = <<(TIDN+1):256>>,%when an unmatched gets entirely matched, just use the next higher id.

    FM = #futarchy_matched{id = TID1, owner = Owner, futarchy_id = FID,
                           decision = D, goal = G, revert_amount = RA,
                           win_amount = RA * ?e2_16 div LP
                          },
    Dict3 = futarchy_matched:dict_write(FM, Dict2),

    %update the linked list, so we aren't pointing to deleted data.
    BFU = futarchy_unmatched:dict_get(Behind, Dict3),
    BFU2 = BFU#futarchy_unmatched{
             ahead = <<0:256>>
            },
    Dict4 = futarchy_unmatched:dict_write(BFU2, Dict3),
    {Dict4, RA}.
    
bought_one(Q1, Q2, Q1b, Q2b) ->
     %check that they bought one kind of shares.
    case {Q1, Q2} of
        {Q1b, _} -> ok;
        {_, Q2b} -> ok
    end,
    true = Q1 =< Q1b,
    true = Q2 =< Q2b.
    



