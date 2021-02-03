-module(multi_tx).
-export([go/4, 
	 %make/2, 
	 make_dict/3, from/1, txs/1]).
-include("../../records.hrl").
-record(unmatched, {from, nonce, fee, oracle_id}).
-record(oracle_bet, {from, nonce, fee, id, type, amount}).
from(X) -> X#multi_tx.from.
txs(X) -> X#multi_tx.txs.

make_dict(From, Txs, Fee) ->
    %replace from and nonce in each sub-tx with a 0.
    Acc = trees:get(accounts, From),
    Txs2 = zero_accounts_nonces(Txs),
    #multi_tx{from = From, nonce = Acc#acc.nonce + 1, txs = Txs2, fee = Fee}.
zero_accounts_nonces([]) -> [];
zero_accounts_nonces([H|T]) 
  when (is_record(H, spend) or 
        is_record(H, create_acc_tx) or 
        is_record(H, oracle_new) or 
        is_record(H, oracle_bet) or 
        is_record(H, unmatched) or
        is_record(H, oracle_winnings) or
        is_record(H, oracle_close) or
        is_record(H, sub_spend_tx) or 
        is_record(H, contract_timeout_tx) or
        is_record(H, contract_simplify_tx) or
        is_record(H, contract_winnings_tx) or
        is_record(H, contract_use_tx) or
        is_record(H, market_new_tx) or
        is_record(H, market_liquidity_tx) or
        is_record(H, swap_tx2) or
        is_record(H, market_swap_tx))
       ->
    H2 = setelement(2, H, 0),
    H3 = setelement(3, H2, 0),
    H4 = setelement(4, H3, 0),
    [H4|zero_accounts_nonces(T)];
zero_accounts_nonces([H|T]) 
  when (is_record(H, swap_tx) or
        is_record(H, trade_cancel_tx) or
        is_record(H, contract_new_tx)) ->
    H2 = setelement(2, H, 0),
    H3 = setelement(4, H2, 0),
    [H3|zero_accounts_nonces(T)].

go(Tx, Dict, NewHeight, _) ->
    F = forks:get(4),
    true = NewHeight >= F,
    From = Tx#multi_tx.from,
    Txs = Tx#multi_tx.txs,
    true = length(Txs) > 0,
    F40 = forks:get(40),
    Fee = Tx#multi_tx.fee,
    Debt0 = 
        if
            NewHeight > F40 -> [{veo, Fee}];
            true -> []
        end,
    {Dict1, Debts} = flash_loan(From, Txs, Dict, Debt0, NewHeight),
    Dict2 = sub_txs(Txs, From, Dict1, NewHeight),
    AFee = 
        if
            NewHeight > F40 -> 0;
            true -> -Fee
        end,
    Facc = accounts:dict_update(From, Dict2, AFee, Tx#multi_tx.nonce),%TODO, maybe we need a way to set this nonce to "none".
    Dict3 = accounts:dict_write(Facc, Dict2),
    flash_payback(From, Debts, Dict3).
sub_txs([], From, Dict, _) -> Dict;
sub_txs([H|T], From, Dict, NewHeight) ->
    Type = element(1, H),
    Dict2 = case Type of
                swap_tx -> swap(H, From, Dict, NewHeight);
                %swap_tx2 -> swap2(H, From, Dict, NewHeight);
                trade_cancel_tx -> trade_cancel(H, From, Dict, NewHeight);
                contract_new_tx -> contract_new(H, From, Dict, NewHeight);
                _ -> 
                    create_spend(Type, H, From, Dict, NewHeight)
            end,
    sub_txs(T, From, Dict2, NewHeight).

contract_new(Tx, From, Dict, NewHeight) ->
    true = (NewHeight > forks:get(32)),
    %create_spend(contract_new_tx, Tx, From, Dict, NewHeight).
    Tx2 = Tx#contract_new_tx{
            from = From
           },
    contract_new_tx:go(Tx2, Dict, NewHeight, none).
    
create_spend(Type, H, From, Dict, NewHeight) ->
    case Type of
        spend -> ok;
        create_acc_tx -> ok;
        _ -> 
            true = (NewHeight > forks:get(32))
    end,
    0 = element(2, H),
    0 = element(3, H),
    0 = element(4, H),
    M = txs:key2module(Type),
    H2 = setelement(2, H, From),
    M:go(H2, Dict , NewHeight, none).
trade_cancel(Tx, From, Dict, NewHeight) -> 
    true = (NewHeight > forks:get(44)),
    #trade_cancel_tx{
      acc = 0,
      fee = 0
     } = Tx,
    Tx2 = Tx#trade_cancel_tx{
            acc = From
           },
    trade_cancel_tx:go(Tx2, Dict, NewHeight, none).
%swap2(Tx, From, Dict, NewHeight) ->    
%    true = (NewHeight > forks:get(44)),
%    #swap_tx2{
%      from = 0,
%      fee = 0
%     } = Tx,
%    Tx2 = Tx#swap_tx2{
%            from = From
%           },
%    swap_tx2:go(Tx2, Dict, NewHeight, none).
swap(Tx, From, Dict, NewHeight) ->    
    true = (NewHeight > forks:get(32)),
    #swap_tx{
      from = 0,
      fee = 0
     } = Tx,
    Tx2 = Tx#swap_tx{
            from = From
           },
    swap_tx:go(Tx2, Dict, NewHeight, none).

pay_kind(From, CID, Type, Amount, D) ->
    case CID of
        <<0:256>> ->
            {give_veo(From, Amount, D),
             {veo, Amount}};
        _ ->
            {give_sub(From, CID, Type, Amount, D),
             {sub, CID, Type, Amount}}
    end.
    

flash_loan(_, [], D, Debt, _) -> {D, Debt};
flash_loan(From, [Tx|T], D, Debt, Height)
  when is_record(Tx, market_new_tx) ->
    #market_new_tx{
                  cid1 = CID1,
                  type1 = Type1,
                  cid2 = CID2,
                  type2 = Type2,
                  amount1 = Amount1,
                  amount2 = Amount2
                 } = Tx,
    {D2, Debt1} = pay_kind(From, CID1, Type1, Amount1, D),
    {D3, Debt2} = pay_kind(From, CID2, Type2, Amount2, D2),
    flash_loan(From, T, D3, [Debt1|[Debt2|Debt]], Height);
flash_loan(From, [Tx|T], D, Debt, Height)
  when is_record(Tx, market_liquidity_tx) ->
    #market_liquidity_tx{
                  cid1 = CID1,
                  type1 = Type1,
                  cid2 = CID2,
                  type2 = Type2,
                  mid = MID,
                  amount = Amount0
                 } = Tx,
    F37 = forks:get(37),
    Amount = 
        if
            Height > F37 -> abs(Amount0);
            true -> Amount0
        end,
                 
    D2 = give_sub(From, MID, 0, Amount, D),
    M = markets:dict_get(MID, D2),
    #market{
             amount1 = MA1,
             amount2 = MA2,
             shares = Shares
           } = M,
    A1 = Amount * MA1 div Shares,
    A2 = Amount * MA2 div Shares,
    {D3, Debt1} = pay_kind(From, CID1, Type1, A1, D2),
    {D4, Debt2} = pay_kind(From, CID2, Type2, A2, D3),
    Debts2 = [{sub, MID, 0, Amount}, Debt1, Debt2] ++ Debt,
    flash_loan(From, T, D4, Debts2, Height);
flash_loan(From, [Tx|T], D, Debt, Height)
  when is_record(Tx, swap_tx2) ->
    #swap_tx2{
               match_parts = MatchParts,
               offer = SSO
             } = Tx,
    SO = signing:data(SSO),
    #swap_offer2{
                  cid2 = CID2,
                  type2 = Type2,
                  amount2 = Amount2,
                  parts = Parts
                } = SO,
    A2 = Amount2 * MatchParts div Parts,
    {D3, Debt2} = 
        pay_kind(From, CID2, Type2, A2, D),
    flash_loan(From, T, D3, [Debt2] ++ Debt, Height);
flash_loan(From, [Tx|T], D, Debt, Height)
  when is_record(Tx, market_swap_tx) ->
    #market_swap_tx{
                  cid1 = CID1,
                  type1 = Type1,
                  cid2 = CID2,
                  type2 = Type2,
                  give = Give,
                  take = Take,
                  mid = MID
                 } = Tx,
    Amount = max(Give, Take),
    {D2, Debt1} = pay_kind(From, CID1, Type1, Amount, D),
    {D3, Debt2} = pay_kind(From, CID2, Type2, Amount, D2),
    flash_loan(From, T, D3, [Debt1, Debt2] ++ Debt, Height);
flash_loan(From, [Tx|T], D, Debt, Height)
  when is_record(Tx, swap_tx) ->
    #swap_tx{
              offer = Offer
            } = Tx,
    #swap_offer{
                 amount2 = Amount,
                 cid2 = CID,
                 type2 = Type
               } = signing:data(Offer),
    case CID of
        <<0:256>> ->
            D2 = give_veo(From, Amount, D),
    %veothey are spending + debt
            flash_loan(From, T, D2, [{veo, Amount}|Debt], Height);
        _ -> 
            D2 = give_sub(From, CID, Type, Amount, D),
    %whatever currency they are spending + debt
            flash_loan(From, T, D2, [{sub, CID, Type, Amount}|Debt], Height)
    end;
flash_loan(From, [Tx|T], D, Debt, Height)
  when is_record(Tx, contract_use_tx) ->
    A = Tx#contract_use_tx.amount,
    if
        A > 0 -> 
            D2 = give_veo(From, A, D),
            flash_loan(From, T, D2, [{veo, A}|Debt], Height);%give this amount to acc + debts
        A < 0 -> 
            %V is a list of 1's the length of Contract.
            #contract_use_tx{
              contract_id = CID,
              many = M
             } = Tx,
            V = many_list(1, M),
            {D3, SubDebts} = give_subs(From, CID, V, -A, D),%give this many subcurrencies + debts
            flash_loan(From, T, D3, SubDebts ++ Debt, Height)
    end;
flash_loan(From, [Tx|T], D, Debt, Height) ->
    flash_loan(From, T, D, Debt, Height).

flash_payback(_, [], Dict) -> Dict;
flash_payback(From, [{veo, Amount}|T], Dict) ->
    F = accounts:dict_update(From, Dict, -Amount, none),
    Dict2 = accounts:dict_write(F, Dict),
    flash_payback(From, T, Dict2);
flash_payback(From, [{sub, CID, N, Amount}|T], Dict) ->
    Key = sub_accounts:make_key(From, CID, N),
    F = sub_accounts:dict_update(Key, Dict, -Amount, none),
    Dict2 = sub_accounts:dict_write(F, Dict),
    flash_payback(From, T, Dict2).


give_veo(From, Amount, Dict) ->
    F = accounts:dict_update(From, Dict, Amount, none),
    accounts:dict_write(F, Dict).
give_subs(From, CID, Subs, Amount, D) ->
    give_subs2(From, CID, 1, Subs, Amount, D, []).
give_subs2(From, CID, N, [], _, D, Debts) -> {D, Debts};
give_subs2(From, CID, N, [S|Subs], Amount, D, Debts) ->
    case S of
        0 -> give_subs2(From, CID, N+1, Subs, Amount, D, Debts);
        _ ->
            D2 = give_sub(From, CID, N, Amount, D),
            give_subs2(From, CID, N+1, Subs, Amount, D2, [{sub, CID, N, Amount}|Debts])
    end.

give_sub(From, CID, Type, Amount, Dict) ->
    Key = sub_accounts:make_key(From, CID, Type),
    SA = case sub_accounts:dict_get(Key, Dict) of
             empty -> sub_accounts:new(From, 0, CID, Type);
             X -> X
         end,
    SA2 = SA#sub_acc{balance = SA#sub_acc.balance + Amount},
    %F = sub_accounts:dict_update(Key, Dict, Amount, none),
    sub_accounts:dict_write(SA2, Dict).
    

many_list(_, N) when N<1 -> [];
many_list(X, N) -> 
    [X|many_list(X, N-1)].
