-module(futarchy_resolve_tx).
-export([go/4, make_dict/4, cid_oid/1, spend_or_create_sub/5]).

-include("../../records.hrl").

%ln(2) estimated as a rational.
-define(l2top,    3342617437).
-define(l2bottom, 4822377600).

%problem: we are deleting an amount of money equal to shares_true_no
%decision result is true, so this is the bet that is not reverted.

%accounting
%=============
%pubkey
%  - Fee
% contract
%  + shares of yes and no from the unreverted market.
% creator
%  + profit from the lmsr in the the unreverted market. veo and shares.
%  + liquidity from the reverted market. 
% futarchy
%  - liquidity from the reverted market
%  - shares of yes and no from the unreverted market
%   record the amount of veo needed to revert the market. Also, update how we calculate the amount of veo in the futarchy after active=0.
% leave the unmatched shares unchanged. This is reduced when there are futarchy_matched_tx that convert it back to veo.


% move all the unreverted to the contract immediately.
% lower reverted to zero bit by bit as people revert their bets to veo with futarchy_matched_tx.



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
    %io:fwrite("futarchy_resolve_tx start \n\n"),
    Nonce = nonce_check:doit(
              NonceCheck, 
              Nonce0),
    Acc = accounts:dict_update(
            Pubkey, Dict, -Fee, Nonce),
    %io:fwrite("accounting account \n"),
    %io:fwrite(integer_to_list(-Fee)),
    %io:fwrite("\n"),
    Dict2 = accounts:dict_write(Acc, Dict),
    Futarchy = futarchy:dict_get(FID, Dict, NewHeight),
    #futarchy{decision_oid = Decision,
              liquidity_true = LT0,
              liquidity_false = LF0,
              shares_true_yes = STY,
              shares_true_no = STN,
              shares_false_yes = SFY,
              shares_false_no = SFN,
              creator = Creator
               } = Futarchy,
    {ContractKey, _GOID, ContractHash} = cid_oid(Futarchy),

    Oracle = oracles:dict_get(
               Decision, Dict2, NewHeight),
    %note that Decision and _GOID are id for two different oracles
    #oracle{result = OracleResult
           } = Oracle,

    Dict3 = 
        case contracts:dict_get(ContractKey, Dict2) of
            empty ->
                contracts:dict_write(
                  contracts:new(ContractHash, 2, <<0:256>>, 0),
                  Dict2);
            _ -> Dict2
        end,

    %in the oracle, 1="true", 2="false", 3="bad question"
    %put unreverted liquidity and unreverted shares of yes and no into the contract.volume.
    %io:fwrite("shares data\n"),
    %io:fwrite(integer_to_list(SFY)),
    %io:fwrite(", "),
    %io:fwrite(integer_to_list(SFN)),
    %io:fwrite(", "),
    %io:fwrite(integer_to_list(STY)),
    %io:fwrite(", "),
    %io:fwrite(integer_to_list(STN)), %TODO. same value as money deleted...
    %io:fwrite("\n"),

%    io:fwrite("oracle result is "),
%    io:fwrite(integer_to_list(OracleResult)),
%    io:fwrite("\n"),
    

    Contract = contracts:dict_get(ContractKey, Dict3),

    MoreShares = 
        case OracleResult of
            1 -> max(STY, STN);
            2 -> max(SFY, SFN);
            3 -> 0
        end,

    Contract2 = Contract#contract
        %{volume = Contract#contract.volume + VolumeBoost},
        {volume = Contract#contract.volume + MoreShares},

    %io:fwrite("accounting contract \n"),
    %io:fwrite(integer_to_list(MoreShares)),
    %io:fwrite("\n"),
    %unreverted liquidity
%  + unreverted shares of yes and no


    Dict4 = contracts:dict_write(Contract2, Dict3),
 
    %return any extra money to the creator.
    %from the canceled market they get the entire liquidity back.
    LiquidityRecover = 
        case OracleResult of
            1 -> LF0;
            2 -> LT0;
            3 -> LT0 + LF0
        end,

    %the creator gets any extra shares sold in the market.
    %imagine we turn all the veo in the market into shares of true and false.
    %then we use these shares to compensate everyone.
    %because of the nature of LMSR, there will be shares left over. These go to the creator.
    {ExtraTrues, ExtraFalses} = 
        case OracleResult of
            1 ->
                io:fwrite("futarchy resolve tx oracle result true\n"),
                %C = lmsr:veo_in_market(LT, STY, STN),
                C = lmsr:veo_in_market(LT0, STY, STN),
                {C - STY, C - STN};
            2 ->
                io:fwrite("futarchy resolve tx oracle result false\n"),
                %C = lmsr:veo_in_market(LF, SFY, SFN),
                C = lmsr:veo_in_market(LF0, SFY, SFN),
                {C - SFY, C - SFN};
            3 ->
                {0, 0}
        end,
    %some of the extra shares can be combined back into veo.
    ExtraVeo = min(ExtraTrues, ExtraFalses),
    %give the extra money back to the creator.
    Dict5 = 
        case accounts:dict_get(Creator, Dict4) of
            #acc{} ->
                ADiff = ExtraVeo + lmsr:veo_in_market(LiquidityRecover, 0, 0),
                CreatorAcc = accounts:dict_update(
                               Creator, Dict4, 
                               %LiquidityRecover + ExtraVeo, 
                               %ExtraVeo, 
                               ADiff,
                               none),
                %io:fwrite("accounting creator account \n"),
                %io:fwrite(integer_to_list(ADiff)),
                %io:fwrite("\n"),
                accounts:dict_write(CreatorAcc, Dict4);
            _ -> 
                %todo. if the creator account was deleted, the extra money should get deleted, so accounting works right.
                Dict4
        end,
  
    MaxExtra = max(ExtraTrues, ExtraFalses),
    ExtraDiff = MaxExtra - ExtraVeo,
    io:fwrite("futarchy_resolve_tx trues falses\n"),
    io:fwrite(integer_to_list(ExtraTrues)),
    io:fwrite("\n"),
    io:fwrite(integer_to_list(ExtraFalses)),
    io:fwrite("\n"),
    Dict6 = if
                (ExtraDiff == 0) ->
                    %no extra liquidity.
                    Dict5;
                (ExtraTrues > ExtraFalses) ->
                    %send extra shares of True to the creator of the futarchy market. Because they originally provided the liquidity for the lmsr.
                    io:fwrite("creator: "),
                    io:fwrite(packer:pack(Creator)),
                    io:fwrite("\ncontract_key: "),
                    io:fwrite(packer:pack(ContractKey)),
                    io:fwrite("\type: 1"),
                    spend_or_create_sub(
                      Dict5, Creator, ContractKey, 
                      ExtraDiff, 1);
                true ->
                    %send extra shares of False...
                    io:fwrite("creator: "),
                    io:fwrite(packer:pack(Creator)),
                    io:fwrite("\ncontract_key: "),
                    io:fwrite(packer:pack(ContractKey)),
                    io:fwrite("\type: 0"),
                    spend_or_create_sub(
                      Dict5, Creator, ContractKey, 
                      ExtraDiff, 0)
            end,
    {RevertedB, RevertedYes, RevertedNo} = 
        case OracleResult of
            2 -> {Futarchy#futarchy.liquidity_true,
                  Futarchy#futarchy.shares_true_yes,
                  Futarchy#futarchy.shares_true_no};
            1 -> {Futarchy#futarchy.liquidity_false,
                  Futarchy#futarchy.shares_false_yes,
                  Futarchy#futarchy.shares_false_no};
            3 -> {0, 0, 0}
        end,
    
    Reverteds = lmsr:veo_in_market(
                    RevertedB, RevertedYes, RevertedNo) - 
        lmsr:veo_in_market(RevertedB, 0, 0),%value of all reverted trades.
    Futarchy3 = Futarchy#futarchy
        {
          liquidity_true = Reverteds,
          liquidity_false = 0,
          shares_true_yes = 0,
          shares_true_no = 0,
          shares_false_yes = 0,
          shares_false_no = 0,
          active = 0
        },
    %io:fwrite("accounting futarchy \n"),
    %io:fwrite(integer_to_list(block:sum_amounts_helper(futarchy, Futarchy3, 0, 0, 0) - block:sum_amounts_helper(futarchy, Futarchy, 0, 0, 0))),
    %io:fwrite("\n"),
    Dict7 = futarchy:dict_write(Futarchy3, Dict6),
    Dict7.
    
    

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
            error -> 
                io:fwrite("futarchy resolve tx, creator cid D. \n"),
                io:fwrite(base64:encode(To)),
                io:fwrite("\n"),
                io:fwrite(base64:encode(CID)),
                io:fwrite("\n"),
                io:fwrite(integer_to_list(D)),
                io:fwrite("\n"),
                1=2;
            empty -> sub_accounts:new(To, Amount, CID, D);
            _ -> sub_accounts:dict_update(
                   ToKey, Dict, Amount, none)
        end,
    sub_accounts:dict_write(Tacc, Dict).
    
    
