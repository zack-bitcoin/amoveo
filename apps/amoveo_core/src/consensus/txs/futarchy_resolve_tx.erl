-module(futarchy_resolve_tx).
-export([go/4, make_dict/5]).

-include("../../records.hrl").

-record(futarchy_resolve_tx,
        {pubkey, nonce, fee,
        fid, %id of the futarchy
         creator, %person who originally made this futarchy market.
        decision_oid %id of the decision oracle, which is now finalized
        }).

make_dict(Pubkey, FID, Creator, Decision, Fee) ->
    Account = trees:get(accounts, Pubkey),
    CreatorAccount = trees:get(accounts, Creator),
    #acc{} = CreatorAccount,
    <<_:256>> = FID,
    case Decision of
        0 -> ok;
        1 -> ok
    end,
    #futarchy_resolve_tx{
      pubkey = Pubkey, creator = Creator,
      nonce = Account#acc.nonce+1,
      fee = Fee, fid = FID,
      decision_oid = Decision
     }.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #futarchy_resolve_tx{
    pubkey = Pubkey, nonce = Nonce0, fee = Fee,
    fid = FID, decision_oid = Decision, creator = Creator
   } = Tx,
    Nonce = nonce_check:doit(
              NonceCheck, 
              Nonce0),
    Acc = accounts:dict_update(
            Pubkey, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Acc, Dict),
    Futarchy = futarchy:dict_get(FID, Dict, NewHeight),
    #futarchy{decision_oid = Decision,
              liquidity_true = LT,
              liquidity_false = LF,
              shares_true_yes = STY,
              shares_true_no = STN,
              shares_false_yes = SFY,
              shares_false_no = SFN,
              creator = Creator
               } = Futarchy,

    Oracle = oracles:dict_get(
               Decision, Dict2, NewHeight),
    #oracle{result = OracleResult
           } = Oracle,
   
 
    %set the futarchy market to unactive.
    Futarchy2 = Futarchy#futarchy{active = 0},
    %return any extra money to the creator.
    %from the canceled market they get the entire liquidity back.
    LiquidityRecover = 
        case OracleResult of
            1 -> LF;
            2 -> LT;
            3 -> LT + LF
        end,

    %the creator gets any extra shares sold in the market.
    {ExtraTrues, ExtraFalses} = 
        case OracleResult of
            1 ->
                C = lmsr:veo_in_market(LT, STY, STN),
                {C - STY, C - STN};
            2 ->
                C = lmsr:veo_in_market(LF, SFY, SFN),
                {C - SFY, C - SFN};
            3 ->
                {0, 0}
        end,
    %some of the extra shares can be combined back into veo.
    ExtraVeo = min(ExtraTrues, ExtraFalses),
    CreatorAcc = accounts:dict_update(
                   Creator, Dict2, 
                   LiquidityRecover + ExtraVeo, 
                   none),
    Dict3 = accounts:dict_write(CreatorAcc, Dict2),
    
    if
        (ExtraTrues > ExtraFalses) ->
            %todo,  send ExtraTrues - ExtraVeo of shares of true in the binary market to the creator.
            ok;
        true ->
            %todo,  send ExtraFalses - ExtraVeo of shares of true in the binary market to the creator.
            ok
    end,

    Dict3.
