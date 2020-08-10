-module(multi_tx).
-export([go/4, 
	 %make/2, 
	 make_dict/3, from/1, txs/1]).
-include("../../records.hrl").
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
        is_record(H, contract_use_tx))
       ->
    H2 = setelement(2, H, 0),
    H3 = setelement(3, H2, 0),
    H4 = setelement(4, H3, 0),
    [H4|zero_accounts_nonces(T)];
zero_accounts_nonces([H|T]) 
  when (is_record(H, swap_tx)) ->
    H2 = setelement(2, H, 0),
    H3 = setelement(4, H2, 0),
    [H3|zero_accounts_nonces(T)].

go(Tx, Dict, NewHeight, _) ->
    F = forks:get(4),
    true = NewHeight >= F,
    From = Tx#multi_tx.from,
    Txs = Tx#multi_tx.txs,
    true = length(Txs) > 0,
    {Dict1, Debts} = flash_loan(From, Txs, Dict, []),
    Dict2 = sub_txs(Txs, From, Dict1, NewHeight),
    Fee = Tx#multi_tx.fee,
    Facc = accounts:dict_update(From, Dict2, -Fee, Tx#multi_tx.nonce),%TODO, we need a way to set this nonce to "none".
    Dict3 = accounts:dict_write(Facc, Dict2),
    flash_payback(From, Debts, Dict3).
sub_txs([], From, Dict, _) -> Dict;
sub_txs([H|T], From, Dict, NewHeight) ->
    Type = element(1, H),
    Dict2 = case Type of
                spend -> spend(H, From, Dict, NewHeight);
                create_acc_tx -> create_account(H, From, Dict, NewHeight);
                contract_use_tx -> contract_use(H, From, Dict, NewHeight);
                swap_tx -> swap(H, From, Dict, NewHeight)
                %pair_buy -> pair_buy(H, From, Dict, NewHeight)
            end,
    sub_txs(T, From, Dict2, NewHeight).

spend(H, From, Dict, NewHeight) ->
    create_spend(spend, H, From, Dict, NewHeight).
create_account(H, From, Dict, NewHeight) ->
    create_spend(create_acc_tx, H, From, Dict, NewHeight).
contract_use(Tx, From, Dict, NewHeight) ->    
%-record(contract_use_tx, {from, nonce, fee, contract_id, amount, many}).
    create_spend(contract_use_tx, Tx, From, Dict, NewHeight).
create_spend(Type, H, From, Dict, NewHeight) ->
    0 = element(2, H),
    0 = element(3, H),
    0 = element(4, H),
    M = txs:key2module(Type),
    H2 = setelement(2, H, From),
    M:go(H2, Dict , NewHeight, none).
swap(Tx, From, Dict, NewHeight) ->    
    #swap_tx{
      from = 0,
      fee = 0
     } = Tx,
    Tx2 = Tx#swap_tx{
            from = From
           },
    swap_tx:go(Tx2, Dict, NewHeight, false).
%pair_buy(Tx, From, Dict, NewHeight) ->    
%    #pair_buy_tx{
%      from = 0,
%      fee = 0
%     } = Tx,
%    Tx2 = Tx#pair_buy_tx{
%            from = From
%           },
%    pair_buy_tx:go(Tx2, Dict, NewHeight, none).
%-record(swap_tx, {from, offer, fee}).
%-record(pair_buy_tx, {from, offer, fee}).

flash_loan(_, [], D, Debt) -> {D, Debt};
flash_loan(From, [Tx|T], D, Debt)
  when is_record(Tx, swap_tx) ->
    #swap_tx{
              offer = Offer
            } = Tx,
    #swap_offer{
                 amount2 = Amount,
                 cid2 = CID,
                 type2 = Type
               } = testnet_sign:data(Offer),
    case CID of
        <<0:256>> ->
            D2 = give_veo(From, Amount, D),
    %veothey are spending + debt
            flash_loan(From, T, D2, [{veo, Amount}|Debt]);
        _ -> 
            D2 = give_sub(From, CID, Type, Amount, D),
    %whatever currency they are spending + debt
            flash_loan(From, T, D2, [{sub, CID, Type, Amount}|Debt])
    end;
%flash_loan(From, [Tx|T], D, Debt)
%  when is_record(Tx, pair_buy_tx) ->
%    Offer = Tx#pair_buy_tx.offer,
%    #pair_buy_offer{
%                     subs2 = Subs,
%                     contract_hash = CH,
%                     source_id = S,
%                     source_type = ST,
%                     amount1 = Amount1,
%                     amount2 = Amount2
%                   } = Offer,
%    MT = length(Subs),
%    CID = contracts:make_id(CH, MT, S, ST),
%    {D2, Debt2} = 
%        if
%            Amount2 > 0 -> 
%                D8 = give_veo(From, Amount2, D),
%                {D8, [{veo, Amount2}|Debt]};%give acc1 enough to cover this + debt
%            true -> {D, Debt}
%    end,
%    {D3, Debt3} =
%        if
%            Amount1 + Amount2 < 0 -> 
%                {D4, Subs} = give_subs(From, CID, Subs, -(Amount1 + Amount2), D), %give acc1 enough subcurrencies to cover this + debt
%                {D4, Subs ++ Debt};
%            true -> {D2, Debt2}
%        end,
%    flash_loan(from, T, D3, Debt3);
flash_loan(From, [Tx|T], D, Debt)
  when is_record(Tx, contract_use_tx) ->
    A = Tx#contract_use_tx.amount,
    if
        A > 0 -> 
            D2 = give_veo(From, A, D),
            flash_loan(From, T, D2, [{veo, A}|Debt]);%give this amount to acc + debts
        A < 0 -> 
            %V is a list of 1's the length of Contract.
            #contract_use_tx{
              contract_id = CID,
              many = M
             } = Tx,
            V = many_list(1, M),
            {D3, SubDebts} = give_subs(From, CID, V, -A, D),%give this many subcurrencies + debts
            flash_loan(From, T, D3, SubDebts ++ Debt)
    end;
flash_loan(From, [Tx|T], D, Debt) ->
    flash_loan(From, T, D, Debt).

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
