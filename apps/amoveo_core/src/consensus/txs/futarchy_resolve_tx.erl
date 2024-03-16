-module(futarchy_resolve_tx).
-export([go/4, make_dict/4, cid_oid/1]).

-include("../../records.hrl").


%accounting
%=============
%pubkey
%  - Fee
% contract
%  + unreverted liquidity 
%  + unreverted shares of yes and no
% creator
%  + reverted liquiduty
%  + ExtraVeo 
% futarchy
%  - liquidity_true
%  - liquidity_false
%  - unreverted shares of yes and no.
%  leave the reverted shares unchanged, this gets reduced when there are futarchy_matched_tx that convert reverted shares back to veo.

% todo
% lower reverted to zero bit by bit as people revert their bets to veo with futarchy_matched_tx.
% move all the unreverted to the contract immediately.



make_contract(OID) ->
    %from amoveo/amoveo_core/priv/binary_2.fs compiled with chalang forth compiler. It has 2 currency types.
    Contract = 
        <<141,120,131,20,131,22,20,131,22,20,131,20,172,135,141,
          121,58,70,71,13,72,20,20,141,135,22,20,2,0,0,0,3,0,0,0,
          22,134,143,58,70,132,0,128,0,0,0,22,130,0,127,255,255,
          255,22,130,136,140,4,3,232,71,20,141,58,70,132,0,255,
          255,255,255,22,130,140,22,130,136,140,4,3,232,71,20,142,
          58,70,132,140,22,130,0,255,255,255,255,22,130,136,140,4,
          3,232,71,20,20,132,0,128,0,0,0,22,130,0,127,255,255,255,
          22,130,136,4,19,136,150,72,72,72>>,
    Input = <<3, 32, 2, (OID)/binary>>, % int 32 binary OID
    <<Input/binary, Contract/binary>>.

cid_oid(Futarchy) ->
    #futarchy
        {
          goal_oid = OID
        } = Futarchy,
    Contract = make_contract(OID),
    ContractHash = hash:doit(Contract),
    {contracts:make_v_id(
      ContractHash, 2, <<0:256>>, 0), OID,
     ContractHash}.

make_dict(Pubkey, FID, Decision, Fee) ->
%    Futarchy = futarchy:dict_get(FID, Dict, NewHeight),
    Futarchy = trees:get(futarchy, FID),
    Creator = Futarchy#futarchy.creator,
    Account = trees:get(accounts, Pubkey),
    CreatorAccount = trees:get(accounts, Creator),
%    #acc{} = CreatorAccount,
    <<_:256>> = FID,
    <<_:256>> = Decision,
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
    {ContractKey, OID, ContractHash} = cid_oid(Futarchy),

    
    Oracle = oracles:dict_get(
               Decision, Dict2, NewHeight),
    %not that Decision and OID are id for two different oracles.
    #oracle{result = OracleResult
           } = Oracle,


%    BinaryContract = make_contract(Decision),
%    ContractHash = hash:doit(BinaryContract),
   
%    ContractKey = contracts:make_v_id(
%                    ContractHash, 2, <<0:256>>, 0),
    Dict3 = 
        case contracts:dict_get(ContractKey, Dict2) of
            empty ->
                contracts:dict_write(
                  contracts:new(ContractHash, 2, <<0:256>>, 0),
                  Dict2);
            _ -> Dict2
        end,

    %todo, put unreverted liquidity and unreverted shares of yes and no into the contract.volume.
 
    %set the futarchy market to unactive.
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
    %give the extra money back to the creator.
    Dict4 = 
        case accounts:dict_get(Creator, Dict3) of
            #acc{} ->
                CreatorAcc = accounts:dict_update(
                               Creator, Dict3, 
                               LiquidityRecover + ExtraVeo, 
                               none),
                accounts:dict_write(CreatorAcc, Dict3);
            _ -> Dict3
        end,
  
 
    ExtraDiff = max(ExtraTrues, ExtraFalses) - ExtraVeo,
    Dict5 = if
                (ExtraDiff == 0) ->
                    %no extra liquidity.
                    Dict4;
                (ExtraTrues > ExtraFalses) ->
                    %send extra shares of True to the creator of the futarchy market. Because they originally provided the liquidity for the lmsr.
                    spend_or_create_sub(
                      Dict4, Creator, ContractKey, 
                      ExtraDiff, 1);
                true ->
                    %send extra shares of False...
                    spend_or_create_sub(
                      Dict4, Creator, ContractKey, 
                      ExtraDiff, 0)
            end,


    %todo. only remove the unreverted liquidity, not both.
    %todo. remove the unreverted shares of yes and no.
    Futarchy2 = Futarchy#futarchy
        {
          active = 0,
          liquidity_true = 0,
          liquidity_false = 0
        },
    io:fwrite("futarchy resolve tx quantities\n"),
    io:fwrite(integer_to_list(LT)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(LF)),
    io:fwrite("\n"),
    io:fwrite(integer_to_list(STY)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(STN)),
    io:fwrite("\n"),
    io:fwrite(integer_to_list(SFY)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(SFN)),
    io:fwrite("\n"),
    io:fwrite(integer_to_list(LiquidityRecover)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(ExtraTrues)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(ExtraFalses)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(ExtraVeo)),
    io:fwrite("\n"),
    Dict6 = futarchy:dict_write(Futarchy2, Dict5),
    Dict6.
    
    

spend_or_create_sub(Dict, To, CID, Amount, Direction) ->
    %for Direction, 1 is true and 0 is false. this is the result of the decision oracle.
    %but in the smart contract, the first outcome is the True outcome, and it is identified with digit 0. the second outcome is the false outcome, and it is identified with digit 1.
    D = case Direction of
            1 -> 0;
            0 -> 1
        end,
    ToKey = sub_accounts:make_v_key(To, CID, D),
    OA = sub_accounts:dict_get(ToKey, Dict),
    Tacc = 
        case OA of
            empty -> sub_accounts:new(To, Amount, CID, D);
            _ -> sub_accounts:dict_update(
                   ToKey, Dict, Amount, none)
        end,
    sub_accounts:dict_write(Tacc, Dict).
    
    
