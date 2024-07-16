-module(futarchy_bet_tx).
-export([go/4, make_dict/8, orders/3, hash/1, 
         one/0, one_square/0, prices_match/2
        ]).

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
    (A * B) >= ((?e2_32 * ?e2_32) div 4).
    

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
    FU = #futarchy_unmatched{
      owner = Pubkey, futarchy_id = FID, decision = Decision,
      goal = Goal, limit_price = LimitPrice, 
      revert_amount = Amount
     },
    Height = block:height(),
    UID = (futarchy_unmatched:make_id(FU, Height))#futarchy_unmatched.id,
    Nonce = Account#acc.nonce+1,
    MID = futarchy_matched:taker_id(FID, Nonce, Pubkey),
    {UID, MID, #futarchy_bet_tx{
       pubkey = Pubkey,
       nonce = Nonce,
       fee = Fee, fid = FID,
       limit_price = LimitPrice,
       amount = Amount,
       decision = Decision,
       futarchy_hash = FutarchyHash,
       goal = Goal}}.
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
    io:fwrite("futarchy bet tx go start\n"),
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
    io:fwrite("accounting: pay Fee and Amount from Pubkey\n"),
    io:fwrite(integer_to_list(-Fee - Amount)),
    io:fwrite("\n"),
    Acc = accounts:dict_update(
             Pubkey, Dict, 
            -Fee - Amount, 
            Nonce),
    io:fwrite("write account\n"),
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
                io:fwrite("write futarchy_unmatched\n"),
                Dict3a = futarchy_unmatched:dict_write(NewUnmatched3, Dict2),
                Dict4a = case FBehind of
                             error -> Dict3a;
                             _ ->
                                 FBehind2 = FBehind#futarchy_unmatched{ahead = TID},
                                 io:fwrite("write futarchy_unmatched\n"),
                                 futarchy_unmatched:dict_write(FBehind2, Dict3a)
                         end,
                Dict5a = case FAhead of
                             error -> Dict4a;
                             _ ->
                                 FAhead2 = FAhead#futarchy_unmatched{behind = TID},
                                 io:fwrite("write futarchy_unmatched\n"),
                                 futarchy_unmatched:dict_write(FAhead2, Dict4a)
                         end,

                Futarchya = futarchy:dict_get(FID, Dict5a),
                Futarchya2 = case TIDAhead of
                                 <<0:256>> -> 
                                     %then update the futarchy to point to this.
                                     %io:fwrite("updating futarchy order book, decision goal \n"),
                                     %io:fwrite(integer_to_list(Decision)),
                                     %io:fwrite(" "),
                                     %io:fwrite(integer_to_list(Goal)),
                                     %io:fwrite("\n"),
                                     update_orders(Decision, Goal, TID, Futarchya);
                                 _ -> Futarchya
                             end,
                                 
                io:fwrite("write futarchy\n"),
                Dict6a = futarchy:dict_write(Futarchya2, Dict5a),
                Dict6a;
            {1, TIDsMatched, TIDPartiallyMatched, Q1b, Q2b} ->
    %This is the case where your trade is completely matched.
                %check that they bought one kind of shares.
                io:fwrite("futarchy_bet_tx qs\n"),
                io:fwrite("q1: "),
                io:fwrite(integer_to_list(Q1)),
                io:fwrite("\n"),
                io:fwrite("q2: "),
                io:fwrite(integer_to_list(Q2)),
                io:fwrite("\n"),
                io:fwrite("q1b: "),
                io:fwrite(integer_to_list(Q1b)),
                io:fwrite("\n"),
                io:fwrite("q2b: "),
                io:fwrite(integer_to_list(Q2b)),
                io:fwrite("\n"),
                %bought_one(Q1, Q2, Q1b, Q2b),
                case TIDsMatched of
                    [] -> ok;
                    _ -> TheirOrders = hd(TIDsMatched)
                end,

                  %remove the tidsmatched from the db.
                {_, Dict3b, MatchedAmount, 
                 WinAmount} = 
                    remove_firsts(TIDsMatched, Dict2),
                %io:fwrite({TIDPartiallyMatched, TIDPartiallyMatched2, TIDsMatched}),
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
                PartMatch = Rest + AD + MatchedAmount,%todo this should also include the money from the partially matched order.
                Dict3b_5 = 
                    if
                        %(Rest > 0) -> 
                        not(AlmostZero) ->
                            %io:fwrite("partial match\n"),
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
                                  owner = Owner, futarchy_id = FID,
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
                            io:fwrite("accounting: lose Rest from futarchy_unmatched. \n"),
                            io:fwrite(integer_to_list(-Rest)),
                            io:fwrite("\n"),
                            io:fwrite("write futarchy_unmatched\n"),
                            Dict3c = futarchy_unmatched:dict_write(
                                       FUb2, Dict3b),
                            FMID2 = futarchy_matched:maker_id(
                                      TIDPartiallyMatched, Dict3b),
                %the owner of the partially matched trade needs to have a new futarchy_matched element created for them, using a maker_id.
                            FMb2 = #futarchy_matched{
                              id = FMID2,
                              owner = Owner,
                              futarchy_id = FID,
                              decision = Decision,
                              goal = Goal,
                              revert_amount = Rest,
                              win_amount = Rest * one_square() div FULP
                             },
                            empty = futarchy_matched:dict_get(
                                      FMID2, Dict3c),
                            io:fwrite("accounting: gain Rest from futarchy_matched. (counts as zero) \n"),
                            io:fwrite(integer_to_list(Rest)),
                            io:fwrite("\n"),
                            io:fwrite("write futarchy_matched\n"),
                            Dict3d = futarchy_matched:dict_write(
                                       FMb2, Dict3c),
                            Dict3d;
                        true ->
                            io:fwrite("no partial match\n"),
                            Dict3b
                    end,

                

                %FMID = hash:doit(<<FID/binary, Pubkey/binary, Nonce0:32>>),
                FMID = futarchy_matched:taker_id(
                         FID, Nonce0, Pubkey),
                %io:fwrite("making futarchy matched: "),
                %io:fwrite(packer:pack(FMID)),
                %io:fwrite("\n"),

                %io:fwrite({PartMatch, AD, MatchedAmount}),%0,0,0

                %todo, when we update futarchy matched, we need to add volume to the contract. todo, we probably need to do this for version {2, ... } as well.

                FMb = #futarchy_matched{
                  id = FMID,
                  owner = Pubkey,
                  futarchy_id = FID,
                  decision = Decision,
                  goal = G,
                  revert_amount = PartMatch,
                  win_amount = WinAmount + (Q1b + Q2b - Q1 - Q2)
                      },
                empty = futarchy_matched:dict_get(FMID, Dict3b),
                io:fwrite("accounting: gain PartMatch from futarchy_matched. \n"),
                io:fwrite(integer_to_list(PartMatch)),
                io:fwrite("\n"),
                io:fwrite("write futarchy_matched\n"),
                Dict4b = futarchy_matched:dict_write(
                           FMb, Dict3b),
                Futarchyb = futarchy:dict_get(FID, Dict4b),
%                Futarchyb2 = update_shares(
%                               Decision, Q1b, Q2b, Futarchyb),
                Futarchyb2 = set_shares(
                               Decision, Futarchyb, Q1b, Q2b),%probably an error here with the shares TODO
                Futarchyb3 = update_orders(
                               Decision, Goal, 
                               TIDPartiallyMatched, Futarchyb2),
                io:fwrite("accounting: edit futarchy \n"),
                io:fwrite(integer_to_list(Q1)),
                io:fwrite(" "),
                io:fwrite(integer_to_list(Q2)),
                io:fwrite("\n"),
                io:fwrite(integer_to_list(Q1b)),
                io:fwrite(" "),
                io:fwrite(integer_to_list(Q2b)),
                io:fwrite("\n"),
                io:fwrite("write futarchy\n"),
                Dict5b = futarchy:dict_write(Futarchyb3, Dict4b),
                io:fwrite("accounting futarchy \n"),
                %io:fwrite({Futarchy, Futarchyb3}),
                io:fwrite(integer_to_list(block:sum_amounts_helper(futarchy, Futarchyb3, 0, 0, 0) - block:sum_amounts_helper(futarchy, Futarchy, 0, 0, 0))),
                io:fwrite("\n"),
                Dict5b;
            %{2, TIDsMatched, TIDAfterMatched??, NewTopTID, Q1b, Q2b} ->
            {2, TIDsMatched, %all of these were matched.
             NewTopTID, %the new pointer in futarchy.
             Q1b, Q2b} ->
    %This is the case where your trade is partially matched.
                %this is the same as saying that the price went out of range during the lmsr step.
                %check that they bought one kind of shares.
                bought_one(Q1, Q2, Q1b, Q2b),
                TheirOrders = hd(TIDsMatched),

                %check that the worst-priced order still is within the bounds of the price we wanted to pay.
                %TIDAfterMatched = 
                NewTopTID = 
                    price_check(TIDsMatched, LimitPrice, Dict2),

                  %remove the tidsmatched from the db.
                {_, Dict3c, MatchedAmountc, WinAmount2} = 
                    remove_firsts(TIDsMatched, Dict2),

                %check that the next trade in the market is at a worse price than we are willing to match at, or that there is no trade after this one.
                %io:fwrite({LastFutarchyU, TIDAfterMatched}),%error, <<0:256>>
                case NewTopTID of
                    <<0:256>> -> ok;
                    _ ->
                        LastFutarchyU = futarchy_unmatched:dict_get(
                                          %TIDAfterMatched, Dict3c),
                                          NewTopTID, Dict3c),
                        TIDAfterMatched = LastFutarchyU#futarchy_unmatched.behind,

                        AfterMatched=futarchy_unmatched:dict_get(
                                       TIDAfterMatched, Dict2),
                        AMLP=AfterMatched#futarchy_unmatched.limit_price,
                        %make sure that we don't match this trade.
                        false = prices_match(LimitPrice, AMLP),
                        %false = (LimitPrice * AMLP) > ?e2_32,
                        ok
                end,

                D = case Decision of
                        1 -> 0;
                        0 -> 1
                    end,
                G = case Goal of
                        1 -> 0;
                        0 -> 1
                    end,

%                #futarchy_unmatched
%                    {
%                  decision = D,
%                  goal = G,
%                  limit_price = LastPrice
%                 } = LastFutarchyU,

                %MatchedAmountc = Amount,
                LMSRPrice = lmsr:price(LMSRBeta, Q1b, Q2b),
                %io:fwrite({LMSRPrice, LimitPrice}),%{{rat,289415,813704},2168958484}
                % shares/veo, between 0 and 1.
                % limitprice is /?e2_32
                %true = (LMSRPrice =< LimitPrice),
                false = lmsr:less_than(
                          {rat, LimitPrice, ?e2_32},
                          LMSRPrice),
                NewTopTID = price_check(
                              %TIDsMatched, LMSRPrice, Dict3c),
                              TIDsMatched, LimitPrice, Dict3c),
                Futarchyc2 = set_shares(
                               Decision, Futarchy, Q1, Q2),
                LMSRAmount = lmsr:change_in_market(
                               LMSRBeta, Q1, Q2, Q1b, Q2b),
                FMIDc = futarchy_matched:taker_id(
                          FID, Nonce0, Pubkey),
                PartMatchc = LMSRAmount + MatchedAmountc,
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
                empty = futarchy_matched:dict_get(FMIDc, Dict3c),
                io:fwrite("write futarchy_matched\n"),
                Dict3c_5 = futarchy_matched:dict_write(
                             FMc, Dict3c),
                NewUM0 = #futarchy_unmatched{
                  owner = Pubkey, decision=Decision, goal=Goal, 
                  %revert_amount = Amount - LMSRAmount - MatchedAmountc, 
                  revert_amount = Amount, 
                  limit_price = LimitPrice, ahead = <<0:256>>,
                  behind = OurOrders, futarchy_id = FID
                 },
                NewUM1 = futarchy_unmatched:make_id(
                          NewUM0, NewHeight),
                NewUM = NewUM1#futarchy_unmatched{
                  revert_amount = Amount - LMSRAmount - MatchedAmountc
                         },
                NewTID = NewUM#futarchy_unmatched.id,
                io:fwrite("futarchy_bet_tx new tid is \n"),
                io:fwrite(base64:encode(NewTID)),
                io:fwrite("\n"),
                empty = futarchy_unmatched:dict_get(
                          NewTID, Dict3c_5),
                io:fwrite("write futarchy_unmatched\n"),
                Dict4c = futarchy_unmatched:dict_write(
                           NewUM, Dict3c_5),
                Futarchyc3 = set_orders(
                               Decision, Goal, Futarchyc2, 
                               NewTID, NewTopTID),
                io:fwrite("write futarchy\n"),
                Dict5c = futarchy:dict_write(Futarchyc3, Dict4c),
                Dict5c
        end,
    %io:fwrite("futarchy bet tx finished\n"),
    io:fwrite("futarchy bet tx go finished\n"),
    Dict3.

almost_zero(Rest, Amount) ->
    abs(Rest * 1000) < Amount.

price_check(TIDsMatched, {rat, LA, LB}, Dict) ->
    LP = LA * ?e2_32 div LB,
    price_check(TIDsMatched, LP, Dict);
price_check(TIDsMatched, LimitPrice, Dict) ->
    LastTID = hd(lists:reverse(TIDsMatched)),
    false = (LastTID == <<0:256>>),
    LastFutarchyU = 
        futarchy_unmatched:dict_get(LastTID, Dict),
    LPc = LastFutarchyU#futarchy_unmatched.limit_price,
    %true = (LimitPrice * LPc) >= ?e2_32,
    if
        ((not is_integer(LimitPrice)) or
         (not is_integer(LPc))) ->
            io:fwrite({LimitPrice, LPc});
        true -> ok
    end,
    case prices_match(LimitPrice, LPc) of
        true -> ok;
        false -> io:fwrite({LimitPrice, LPc, LastTID})
                     %limit price is less than lpc
    end,
    LastFutarchyU#futarchy_unmatched.behind.%their top
    

almost_equal(A, {rat, N, D}) ->
    B = N * one_square() div D,
    almost_equal(A, B);
almost_equal(A = {rat, _, _}, B) ->
    almost_equal(B, A);
almost_equal(A, B) ->
    C = abs(A - B) * 1000 div one_square(),
    true = C < 30,%within 3%
    ok.

hash(#futarchy_bet_tx{
        pubkey = Pub, nonce = Nonce, limit_price = LP, 
        amount = A, decision = D, goal = G, 
        futarchy_hash = FH}) ->
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


%update_shares(1, Q1, Q2, Futarchy) ->
%    Futarchy#futarchy{
%      shares_true_yes = Q1,
%      shares_true_no = Q2
%     };
%update_shares(0, Q1, Q2, Futarchy) ->
%    Futarchy#futarchy{
%      shares_false_yes = Q1,
%      shares_false_no = Q2
%     }.

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
    io:fwrite("write futarchy unmatched\n"),
    Dict2 = futarchy_unmatched:dict_write(FU2, Dict),
%    futarchy_matched:maker_id(FID, FUNonce, TID, Owner),
    MID = futarchy_matched:maker_id(TID, Dict),
    %next create the matched trade.

    FM = #futarchy_matched
        {id = MID, owner = Owner, futarchy_id = FID,
         decision = D, goal = G, revert_amount = RA,
         win_amount = WinAmount
        },
    %io:fwrite("futarchy bet tx matched a trade. "),
    %io:fwrite(integer_to_list(RA)),
    %io:fwrite(" "),
    %io:fwrite(integer_to_list(WinAmount)),
    %io:fwrite("\n"),
    io:fwrite("write futarchy_matched\n"),
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
                    io:fwrite("write futarchy_unmatched\n"),
                    futarchy_unmatched:dict_write(BFU2, Dict3)
            end,
    {Behind, Dict4, RA, WinAmount}.
    
bought_one(Q1, Q2, Q1b, Q2b) ->
     %check that they bought one kind of shares.
    case {Q1, Q2} of
        {Q1b, _} -> ok;
        {_, Q2b} -> ok;
        _ -> io:fwrite({Q1, Q2, Q1b, Q2b})
    end,
    true = Q1 =< Q1b,
    true = Q2 =< Q2b.

