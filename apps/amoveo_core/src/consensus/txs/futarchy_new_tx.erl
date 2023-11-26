-module(futarchy_new_tx).
-export([go/4, make_dict/8]).

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
    F0 = #futarchy{decision_oid = DecisionOID,
                   creator = Pubkey,
                   goal_oid = GoalOID,
                   batch_period = Period},
    Account = trees:get(accounts, Pubkey),
    F = futarchy:make_id(F0, Height),
    #futarchy_new_tx{pubkey = Pubkey,
                     nonce = Account#acc.nonce+1,
                     decision_oid = DecisionOID,
                     goal_oid = GoalOID,
                     period = Period,
                     true_liquidity = TrueLiquidity,
                     false_liquidity = FalseLiquidity,
                     futarchy_id = F#futarchy.fid, 
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
    Nonce = nonce_check:doit(
              NonceCheck, 
              Nonce0),
    Acc = accounts:dict_update(
             Pubkey, Dict, 
            -Fee - TrueLiquidity - FalseLiquidity, 
            Nonce),
    Dict2 = accounts:dict_write(Acc, Dict),
   
    %C_0 = B*ln(e^(q1/B) + e^(q2/B)) when q1 = q2 = 0
    %C_0 = B*ln(2).
    %C_0 is the money they provide for initial liquidity.
    % so B is C_0 / ln(2).
 
    TLpL2 = TrueLiquidity * ?l2bottom div ?l2top,
    FLpL2 = FalseLiquidity * ?l2bottom div ?l2top,
    F0 = #futarchy{decision_oid = DecisionOID,
                   creator = Pubkey,
                   goal_oid = GoalOID,
                   batch_period = Period,
                   liquidity_true = TLpL2,%Beta, from lmsr's definition.
                   liquidity_false = FLpL2},%
    F = futarchy:make_id(F0, NewHeight),
    FID = F#futarchy.fid,%check that the FID was calculated correctly.
    DOracle = oracles:dict_get(DecisionOID, Dict2),
    GOracle = oracles:dict_get(GoalOID, Dict2),
    #oracle{} = DOracle,
    #oracle{} = GOracle,
    futarchy:dict_write(F, Dict2).
    
    
