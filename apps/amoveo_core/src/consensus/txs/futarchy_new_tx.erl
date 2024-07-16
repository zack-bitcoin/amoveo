-module(futarchy_new_tx).
-export([go/4, make_dict/8, times_l2/1, div_l2/1]).

-include("../../records.hrl").

%ln(2) estimated as a rational.
-define(l2top,    3342617437).
-define(l2bottom, 4822377600).


make_dict(Pubkey, DecisionOID, GoalOID, Period, 
          TrueLiquidity, FalseLiquidity, Fee, 
          Height) ->
    <<_:256>> = DecisionOID,
    <<_:256>> = GoalOID,
    <<_:520>> = Pubkey,
    true = is_integer(Period),
    true = Period > -1,
    Account = trees:get(accounts, Pubkey),
    Nonce = Account#acc.nonce+1,
    FID = futarchy:make_id(Pubkey, <<Nonce:256>>, Height),
    #futarchy_new_tx{pubkey = Pubkey,
                     nonce = Nonce,
                     decision_oid = DecisionOID,
                     goal_oid = GoalOID,
                     period = Period,
                     true_liquidity = TrueLiquidity,
                     false_liquidity = FalseLiquidity,
                     futarchy_id = FID, 
                     fee = Fee}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #futarchy_new_tx{
    pubkey = Pubkey, decision_oid = DecisionOID,
    goal_oid = GoalOID, period = Period, 
    true_liquidity = TrueLiquidity,
    false_liquidity = FalseLiquidity,
    nonce = Nonce0, 
    futarchy_id = FID, fee = Fee} = Tx,

    true = NewHeight > (forks:get(54)),
    true = is_integer(Period),
    true = Period > -1,
    true = TrueLiquidity > -1,
    true = FalseLiquidity > -1,
    false = (GoalOID == DecisionOID),
    Nonce = nonce_check:doit(
              NonceCheck, 
              Nonce0),
    Acc = accounts:dict_update(
             Pubkey, Dict, 
            -Fee - TrueLiquidity - FalseLiquidity, 
            Nonce),
    Dict2 = accounts:dict_write(Acc, Dict),
    FID = futarchy:make_id(Pubkey, <<Nonce0:256>>, NewHeight),
    empty = futarchy:dict_get(FID, Dict, NewHeight),
    #oracle{} = oracles:dict_get(DecisionOID, Dict2),
    #oracle{} = oracles:dict_get(GoalOID, Dict2),
  
    %calculating Beta, from LMSR's definition.
    %C_0 = B*ln(e^(q1/B) + e^(q2/B)) when q1 = q2 = 0
    %C_0 = B*ln(2).
    %C_0 is the money they provide for initial liquidity.
    % so B is C_0 / ln(2).

    %beta for the market that gets reverted if we decide on False.
    %BT = TrueLiquidity * ?l2bottom div ?l2top,
    BT = div_l2(TrueLiquidity),

    %beta for the market that gets reverted if we decide on True
    %BF = FalseLiquidity * ?l2bottom div ?l2top,
    BF = div_l2(FalseLiquidity),

    F = #futarchy{decision_oid = DecisionOID,
                  fid = FID,
                  creator = Pubkey,
                  goal_oid = GoalOID,
                  batch_period = Period,
                  liquidity_true = BT,
                  liquidity_false = BF},
    futarchy:dict_write(F, Dict2).

div_l2(X) ->
    X * ?l2bottom div ?l2top.
times_l2(X) ->
    X * ?l2top div ?l2bottom.
    
    
