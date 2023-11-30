-module(futarchy_batch).
-export([go/4, make_dict/4]).

-include("../../records.hrl").

-define(e2_16, 65536). %this is "one" in a certain perspective. we can store rationals from 1/this to this/1.

-record(futarchy_batch_tx,
        {pubkey, nonce, fee,
        fid, %futarchy id
        decision,%looking at the market that does not get reverted in this case.
         matching_true,%how many shares of true the lmsr market is buying. This counts all matched trades that are buying true. (shares can't be sold, so it is always positive)
         matching_false
        }). 
%        price, %the price that this batch will match at.
%        true_matched, %ids of unmatched bets on true that will be matched or partially matched in this batch.
%        false_matched});

make_dict(Pubkey, FID, Decision, Fee) ->
    Account = trees:get(accounts, Pubkey),
    <<_:256>> = FID,
    case Decision of
        0 -> ok;
        1 -> ok
    end,
    #futarchy_batch_tx{
      pubkey = Pubkey,
      nonce = Account#acc.nonce+1,
      fee = Fee, fid = FID,
      decision = Decision}.

go({signed, Tx, _Sig, {Price, _Trues, _Falses}},
   Dict, NewHeight, NonceCheck) ->
   #futarchy_batch_tx{
    pubkey = Pubkey, fid = FID, nonce = Nonce0, 
    fee = Fee, decision = Decision, 
    matching_true = MT, matching_false = MF
   } = Tx, 
    true = NewHeight > (forks:get(54)),
    case Decision of
        1 -> ok;
        0 -> ok
    end,
    true = (0 =< MT),
    true = (0 =< MF),
    Nonce = nonce_check:doit(
              NonceCheck, 
              Nonce0),
    Acc = accounts:dict_update(
             Pubkey, Dict, 
            -Fee, 
            Nonce),
    Dict2 = accounts:dict_write(Acc, Dict),
    Futarchy = futarchy:dict_get(FID, Dict2),
    #futarchy{active = 1, last_batch_height = LBH,
             batch_period = BP, 
              true_yes_orders = PTY,
              true_no_orders = PTN,
              false_yes_orders = PFY,
              false_no_orders = PFN,
              shares_false_yes = SFY,
              shares_false_no = SFN,
              shares_true_yes = STY,
              shares_true_no = STN,
             liquidity_true = LT,
             liquidity_false = LF} =
        Futarchy,
    false = (LBH == 0),%futarchy isn't in a dormant state.
    true = NewHeight - LBH >= BP, %make sure enough time has passed since the last batch.

    %load up the lmsr market we care about.
    {Trues, Falses, LMSRBeta, Q1, Q2} = 
        case Decision of
            1 -> {PTY, PTN, LT, STY, STN};
            0 -> {PFY, PFN, LF, SFY, SFN}
        end,
  
    %how much liquidity the market provides in this case.
%    MML = lmsr:change_in_market(
%            LMSRBeta, Q1, Q2, 
%            Q1 + MT, Q2 + MF
%           ),

%check that the price written on the tx matches the new price in the lmsr after applying the changes.
%instantaneous price 
%`P = e^(q1/B) / (e^(q1/B) + e^(q2/B))`
%P = e^((q1-q1)/B) / (e^((q1-q1)/B) + e^((q2-q1)/B)
%P = 1 / (1 + e^((q2-q1)/B))
   
    %A bigger price means that (Q1 - Q2) is bigger.
 
    Price0 = lmsr:price(LMSRBeta, Q1 + MT, Q2 + MF),
    {rat, PT, PB} = Price0,
    Price = ?e2_16 * PT div PB,

    %total cost to move the market like this
    TotalCost = lmsr:change_in_market(
                  LMSRBeta, Q1, Q2, Q1+MT, Q2+MF),

    TotalCost = (MT * Price div ?e2_16) +
        %(MF * (?e2_16 * ?e2_16 div Price) div ?e2_16)
        (MF * ?e2_16 div Price),
  
    %Veo = shares * price = MT * Price div ?e2^16.
    %match(Trues, Price, MT),

    %walk down both sides of the order book, match everything you can at this price, the very last bet might only be partially matched. include the trade from the lmsr as well.
    %if there are more than 50 trades to match, then just match the first 50, but don't update the batch height counter, so that another batch can be made immediately.
    {NewTruesID, NewFalsesID, MT2, MF2, 
     FinishedBatch} = 
        match(Trues, Falses, Price, MT, MF),
    

    %update the futarchy to point to the new roots.
    %wipe clean futarchy_unmatched that are no longer in use.
    %delete the data from the old ones.

    ok.

match(Trues, Falses, Price, _, _) ->
    ok.
