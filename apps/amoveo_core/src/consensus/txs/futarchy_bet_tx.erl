-module(futarchy_bet_tx).
-export([go/4, make_dict/8, orders/3, hash/1, 
         one/0, one_square/0, prices_match/2,
         futarchy_matched_id_maker/3]).

-include("../../records.hrl").

-define(e2_16, 65536). %we can store rationals from 1/this to this/1. 2^16
% integer X is rational X/65536. smallest value is 1/65536, biggest is 65536/1.
-define(e2_32, 4294967296). %the maximum number that can be expressed in thes system. 2^32

one() -> ?e2_16.
one_square() -> ?e2_32.

prices_match(A, B) ->
    %if A encodes Ra, and B encodes Rb, then
    % A*B = Ra * Rb * ?e2_32 ^2
    % (A*B > ?e2^32^2) <==> (Ra * Rb > 1)
    (A * B) > ?e2_32 * ?e2_32.
    

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

%set_orders(1, 1, F, TID) ->
%    F#futarchy{true_yes_orders = TID};
%set_orders(1, 0, F, TID) ->
%    F#futarchy{true_no_orders = TID};
%set_orders(0, 1, F, TID) ->
%    F#futarchy{false_yes_orders = TID};
%set_orders(0, 0, F, TID) ->
%    F#futarchy{false_no_orders = TID}.

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
    true = is_integer(Nonce0),
    Nonce = Nonce0,
%    Nonce = nonce_check:doit(
%              NonceCheck, 
%              Nonce0),
    Acc = accounts:dict_update(
             Pubkey, Dict, 
            -Fee - Amount, 
            Nonce),
    Dict2 = accounts:dict_write(Acc, Dict),
    Futarchy = futarchy:dict_get(FID, Dict2),
    #futarchy{active = Active, root_hash = RootHash2} =
        Futarchy,
    Active = 1,
    if
        not(RootHash2 == RootHash) ->
            io:fwrite({RootHash, RootHash2});
        true -> ok
    end,

    Q1 = q1(Decision, Futarchy),
    Q2 = q2(Decision, Futarchy),
    LMSRBeta = lmsr_beta(Decision, Futarchy),
    OurOrders = our_orders(Decision, Goal, Futarchy),
    TheirOrders = their_orders(Decision, Goal, Futarchy),

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
                case FBehind of
                    error -> ok;
                    _ ->
                        #futarchy_unmatched
                            {
                      limit_price = LPBehind,
                      decision = Decision,
                      goal = Goal
                     } = FBehind,
                        true = LimitPrice >= LPBehind
                end,
                case FAhead of
                    error -> ok;
                    #futarchy_unmatched{} ->
                        #futarchy_unmatched
                            {
                      limit_price = LPAhead,
                      decision = Decision,
                      goal = Goal
                     } = FAhead,
                        true = LimitPrice =< LPAhead;
                    _ ->
                        io:fwrite({FAhead})
                end,
                NewUnmatched2 = 
                    NewUnmatched#futarchy_unmatched{
                      ahead = TIDAhead,
                      behind = TIDBehind},
                NewUnmatched3= futarchy_unmatched:make_id(NewUnmatched2, NewHeight),
                TID = NewUnmatched3#futarchy_unmatched.id, 
                
                case futarchy_unmatched:dict_get(TID, Dict2) of
                    empty -> ok;
                    error -> 
                        io:fwrite("futarchy bet tx tid is\n"),
                        io:fwrite({TID, NewUnmatched3}),
                        1=2
                end,
                empty = futarchy_unmatched:dict_get(TID, Dict2),
                Dict3a = futarchy_unmatched:dict_write(NewUnmatched3, Dict2),
                Dict4a = case FBehind of
                             error -> Dict3a;
                             _ ->
                                 FBehind2 = FBehind#futarchy_unmatched{ahead = TID},
                                 futarchy_unmatched:dict_write(FBehind2, Dict3a)
                         end,
                Dict5a = case FAhead of
                             error -> Dict4a;
                             _ ->
                                 FAhead2 = FAhead#futarchy_unmatched{behind = TID},
                                 futarchy_unmatched:dict_write(FAhead2, Dict4a)
                         end,

                Futarchya = futarchy:dict_get(FID, Dict5a),
                Futarchya2 = case TIDAhead of
                                 <<0:256>> -> 
                                     %then update the futarchy to point to this.
                                     io:fwrite("updating futarchy order book, decision goal \n"),
                                     io:fwrite(integer_to_list(Decision)),
                                     io:fwrite(" "),
                                     io:fwrite(integer_to_list(Goal)),
                                     io:fwrite("\n"),
                                     update_orders(Decision, Goal, TID, Futarchya);
                                 _ -> Futarchya
                             end,
                                 
                Dict6a = futarchy:dict_write(Futarchya2, Dict5a),
                Dict6a;
            {1, TIDsMatched, TIDPartiallyMatched, Q1b, Q2b} ->
    %This is the case where your trade is completely matched.
                %check that they bought one kind of shares.
                bought_one(Q1, Q2, Q1b, Q2b),
                TheirOrders = 
                    case TIDsMatched of
                        [] -> <<0:256>>;
                        _ -> hd(TIDsMatched)
                    end,

                  %remove the tidsmatched from the db.
                {TIDPartiallyMatched, Dict3b, MatchedAmount, 
                 WinAmount} = 
                    remove_firsts(TIDsMatched, Dict2),
                %check that the worst-priced order is still within the bounds of the price we wanted to pay.
                %if TIDPartiallyMatched is <<0:256>>, then we aren't matching any trade, we are completely matched by the lmsr.
                %false = <<0:256>> == TIDPartiallyMatched,
                

                %price_check([TIDPartiallyMatched], 
                %            LimitPrice, Dict2),
%                FUb = futarchy_unmatched:dict_get(
%                        TIDPartiallyMatched, Dict3b),
%                #futarchy_unmatched
%                    {
%                      owner = Pubkey, futarchy_id = FID,
%                      decision = Decision, goal = G,
%                      revert_amount = FURA,
%                      limit_price = FULP
%                     } = FUb,
                G = case Goal of
                        0 -> 1;
                        1 -> 0
                    end,
                %Q1, Q2 -> Q1b, Q2b
                AD = lmsr:change_in_market(LMSRBeta, Q1, Q2, Q1b, Q2b),
                Rest0 = Amount - AD - MatchedAmount,
                Rest = Rest0,
                %false = Rest < 0,
                %Rest = max(0, Rest0),
                %if Rest is >0, then we are partially matching the next trade. otherwise, we finish matching in the market maker's zone.
                AlmostZero = almost_zero(Rest, Amount),
                Dict3b_5 = 
                    if
                        %(Rest > 0) -> 
                        not(AlmostZero) ->
                            io:fwrite("partial match\n"),
                            %make sure that a trade exists to match with
                            %io:fwrite({Rest, Amount}),
                            false = (<<0:256>> == TIDPartiallyMatched),
                            price_check([TIDPartiallyMatched], 
                                        LimitPrice, Dict2),
                            
       %check that the price in the partially matched order matches with Q1b and Q2b.
                            FUb = futarchy_unmatched:dict_get(
                                    TIDPartiallyMatched, Dict3b),
                            #futarchy_unmatched
                                {
                                  owner = Pubkey, futarchy_id = FID,
                                  decision = Decision, goal = G,
                                  revert_amount = FURA,
                                  limit_price = FULP
                                } = FUb,
             %make sure that we can match with this trade
                            true = prices_match(LimitPrice, FULP),
                            almost_equal(
                              FULP, lmsr:price(LMSRBeta, 
                                              Q1b, Q2b)),
                            FUb2 = FUb#futarchy_unmatched{
                                     revert_amount = FURA - Rest
                                    },
                            futarchy_unmatched:dict_write(
                              FUb2, Dict3b);
                        true ->
                            io:fwrite("no partial match\n"),
                            Dict3b
                    end,
                

                %FMID = hash:doit(<<FID/binary, Pubkey/binary, Nonce0:32>>),
                FMID = futarchy_matched_id_maker(
                         FID, Pubkey, Nonce0),
                io:fwrite("making futarchy matched: "),
                io:fwrite(packer:pack(FMID)),
                io:fwrite("\n"),
                PartMatch = AD + MatchedAmount,
                FMb = #futarchy_matched{
                  id = FMID,
                  owner = Pubkey,
                  futarchy_id = FID,
                  decision = Decision,
                  goal = G,
                  revert_amount = PartMatch,
                  win_amount = WinAmount + (Q1b + Q2b - Q1 - Q2)
                      },
                empty = futarchy_matched:dict_get(FMID, Dict3b_5),
                Dict4b = futarchy_matched:dict_write(
                           FMb, Dict3b_5),
                Futarchyb = futarchy:dict_get(FID, Dict4b),
                Futarchyb2 = update_shares(
                               Decision, Q1b, Q2b, Futarchyb),
                Futarchyb3 = update_orders(
                               Decision, Goal, 
                               TIDPartiallyMatched, Futarchyb2),
                Dict5b = futarchy:dict_write(Futarchyb3, Dict4b),
                Dict5b;
            %{2, TIDsMatched, TIDAfterMatched??, NewTopTID, Q1b, Q2b} ->
            {2, TIDsMatched, NewTopTID, Q1b, Q2b} ->
    %This is the case where your trade is partially matched.
                %this is the same as saying that the price went out of range during the lmsr step.
                %check that they bought one kind of shares.
                bought_one(Q1, Q2, Q1b, Q2b),
                TheirOrders = hd(TIDsMatched),

                %check that the worst-priced order still is within the bounds of the price we wanted to pay.
                TIDAfterMatched = 
                    price_check(TIDsMatched, LimitPrice, Dict2),

                  %remove the tidsmatched from the db.
                {_, Dict3c, MatchedAmountc, WinAmount2} = 
                    remove_firsts(TIDsMatched, Dict2),

                %check that the next trade in the market is at a worse price than we are willing to match at, or that there is no trade after this one.
                LastFutarchyU = futarchy_unmatched:dict_get(
                                  TIDAfterMatched, Dict3c),
                io:fwrite({LastFutarchyU, TIDAfterMatched}),
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
                NewTopTID = price_check(TIDsMatched, LMSRPrice, Dict3c),
                Futarchyc2 = set_shares(
                               Decision, Futarchy, Q1, Q2),
                LMSRAmount = lmsr:change_in_market(LMSRBeta, Q1, Q2, Q1b, Q2b),
                FMIDc = futarchy_matched_id_maker(FID, Pubkey, Nonce0),
                PartMatchc = LMSRAmount + MatchedAmountc,
                FMc = #futarchy_matched{
                  id = FMIDc,
                  owner = Pubkey,
                  futarchy_id = FID,
                  decision = D,
                  goal = G,
                  revert_amount = PartMatchc,
                  %win_amount = PartMatchc * ?e2_16 div LimitPrice
                  win_amount = (Q1b + Q2b - Q1 - Q2) + WinAmount2 
                 },
                empty = futarchy_id:dict_get(FMIDc, Dict3c),
                Dict3c_5 = futarchy_matched:dict_write(
                             FMc, Dict3c),
                NewUM0 = #futarchy_unmatched{
                  owner = Pubkey, decision=D, goal=G, 
                  revert_amount = Amount - LMSRAmount - MatchedAmountc, 
                  limit_price = LimitPrice, ahead = <<0:256>>,
                  behind = OurOrders, futarchy_id = FID
                 },
                NewUM = futarchy_unmatched:make_id(NewUM0),
                NewTID = NewUM#futarchy_unmatched.id,
                empty = futarchy_unmatched:dict_get(NewTID, Dict3c_5),
                Dict4c = futarchy_unmatched:dict_write(NewUM, Dict3c_5),
                %todo. verify that set_orders is putting the right pointers in place.
                1=2,
                Futarchyc3 = set_orders(
                               Decision, Goal, Futarchyc2, 
                               NewTID, NewTopTID),
                Dict5c = futarchy:dict_write(Futarchyc3, Dict4c),
                Dict5c
        end,
    io:fwrite("futarchy bet tx finished\n"),
    Dict3.

almost_zero(Rest, Amount) ->
    abs(Rest * 1000) < Amount.

price_check(TIDsMatched, LimitPrice, Dict) ->
    LastTID = hd(lists:reverse(TIDsMatched)),
    false = (LastTID == <<0:256>>),
    LastFutarchyU = 
        futarchy_unmatched:dict_get(LastTID, Dict),
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

%decision, futarchy
q1(1, Futarchy) -> Futarchy#futarchy.shares_true_yes;
q1(0, Futarchy) -> Futarchy#futarchy.shares_false_yes.
q2(1, Futarchy) -> Futarchy#futarchy.shares_true_no;
q2(0, Futarchy) -> Futarchy#futarchy.shares_false_no.
lmsr_beta(1, Futarchy) -> Futarchy#futarchy.liquidity_true;
lmsr_beta(0, Futarchy) -> Futarchy#futarchy.liquidity_false.

%decision, goal, futarchy
our_orders(1, 1, Futarchy) -> Futarchy#futarchy.true_yes_orders;
our_orders(1, 0, Futarchy) -> Futarchy#futarchy.true_no_orders;
our_orders(0, 1, Futarchy) -> Futarchy#futarchy.false_yes_orders;
our_orders(0, 0, Futarchy) -> Futarchy#futarchy.false_no_orders.

their_orders(1, 1, Futarchy) -> Futarchy#futarchy.true_no_orders;
their_orders(1, 0, Futarchy) -> Futarchy#futarchy.true_yes_orders;
their_orders(0, 1, Futarchy) -> Futarchy#futarchy.false_no_orders;
their_orders(0, 0, Futarchy) ->Futarchy#futarchy.false_yes_orders.

set_orders(1, 1, Futarchy, OurOrders, TheirOrders) ->
    Futarchy#futarchy{true_yes_orders = OurOrders, 
                      true_no_orders = TheirOrders};
set_orders(1, 0, Futarchy, OurOrders, TheirOrders) ->
    Futarchy#futarchy{true_no_orders = OurOrders, 
                      true_yes_orders = TheirOrders};
set_orders(0, 1, Futarchy, OurOrders, TheirOrders) ->
    Futarchy#futarchy{false_yes_orders = OurOrders, 
                      false_no_orders = TheirOrders};
set_orders(0, 0, Futarchy, OurOrders, TheirOrders) ->
    Futarchy#futarchy{false_no_orders = OurOrders, 
                      false_yes_orders = TheirOrders}.

set_shares(1, Futarchy, Q1, Q2) ->
    Futarchy#futarchy{shares_true_yes = Q1,
                      shares_true_no = Q2};
set_shares(0, Futarchy, Q1, Q2) ->
    Futarchy#futarchy{shares_false_yes = Q1,
                      shares_false_no = Q2}.


update_shares(1, Q1, Q2, Futarchy) ->
    Futarchy#futarchy{
      shares_true_yes = Q1,
      shares_true_no = Q2
     };
update_shares(0, Q1, Q2, Futarchy) ->
    Futarchy#futarchy{
      shares_false_yes = Q1,
      shares_false_no = Q2
     }.

%decision, goal, new pointer, futarchy
update_orders(1, 1, P, F) -> F#futarchy{true_yes_orders = P};
update_orders(1, 0, P, F) -> F#futarchy{true_no_orders = P};
update_orders(0, 1, P, F) -> F#futarchy{false_yes_orders = P};
update_orders(0, 0, P, F) -> F#futarchy{false_no_orders = P}.
                  
    
remove_firsts([], Dict) ->
    {<<0:256>>, Dict, 0, 0};
remove_firsts(A, B) ->
    remove_firsts(A, B, 0, 0).
remove_firsts([TID], Dict, Amount, WA) ->
    FU = futarchy_unmatched:dict_get(TID, Dict),
    {Dict2, A, W} = remove_first(TID, Dict),
    {FU#futarchy_unmatched.behind, Dict2, A+Amount, WA + W};
remove_firsts([TID1|[TID2|T]], Dict, Amount, WA) ->
    %check that they are connected.
    FU1 = futarchy_unmatched:dict_get(TID1, Dict),
    FU2 = futarchy_unmatched:dict_get(TID2, Dict),
    TID2 = FU1#futarchy_unmatched.behind,
    TID1 = FU2#futarchy_unmatched.ahead,
    {Dict2, A, W} = remove_first(TID1, Dict),
    Amount2 = A + Amount,
    remove_firsts([TID2|T], Dict2, Amount2, WA + W).
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

    WinAmount = RA * ?e2_16 div LP,
    
    FU2 = FU#futarchy_unmatched{
            revert_amount = 0,
            ahead = <<0:256>>,
            behind = <<0:256>>
           },
    Dict2 = futarchy_unmatched:dict_write(FU2, Dict),

    %next create the matched trade.
    <<TIDN:256>> = TID,
    TID1 = <<(TIDN+1):256>>,%when an unmatched gets entirely matched, just use the next higher id.

    FM = #futarchy_matched
        {id = TID1, owner = Owner, futarchy_id = FID,
         decision = D, goal = G, revert_amount = RA,
         win_amount = WinAmount
        },
    io:fwrite("futarchy bet tx matched a trade. "),
    io:fwrite(integer_to_list(RA)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(WinAmount)),
    io:fwrite("\n"),
    Dict3 = futarchy_matched:dict_write(FM, Dict2), %{futarchy_matched, 

    %update the linked list, so we aren't pointing to deleted data.
    Dict4 = case Behind of
                <<0:256>> ->
                    Dict3;
                true ->
                    BFU = futarchy_unmatched:dict_get(Behind, Dict3),
                    BFU2 = BFU#futarchy_unmatched{
                             ahead = <<0:256>>
                            },
                    futarchy_unmatched:dict_write(BFU2, Dict3)
            end,
    {Dict4, RA, WinAmount}.
    
bought_one(Q1, Q2, Q1b, Q2b) ->
     %check that they bought one kind of shares.
    case {Q1, Q2} of
        {Q1b, _} -> ok;
        {_, Q2b} -> ok;
        _ -> io:fwrite({Q1, Q2, Q1b, Q2b})
    end,
    true = Q1 =< Q1b,
    true = Q2 =< Q2b.

futarchy_matched_id_maker(
  FID, Pubkey, Nonce) ->    
    <<_:256>> = FID,
    <<_:520>> = Pubkey,
    hash:doit(<<3,7,3,8,2,4,5,FID/binary, 
                Pubkey/binary, Nonce:32>>).



