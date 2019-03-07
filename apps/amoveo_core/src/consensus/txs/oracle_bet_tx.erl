-module(oracle_bet_tx).
-export([go/4, go2/3, make/6, make_dict/5, id/1, from/1, to_prove/2]).
-include("../../records.hrl").
-record(oracle_bet, {from, %your account id.
		     nonce, 
		     fee, 
		     id, %id is the id of the oracle they want to participate in.
		     type, %either "true", "false" or "bad_question"
		     amount
                     }).%how many shares do you want to buy?
%This is how you can participate in an existing oracle.
%The market is an order book with 3 types of shares: "true", "false", "bad_question"
%All trades are matched into the order book in pairs at even odds.
%So the order book only stores 1 kind of order at a time.
%If you want your order to be held in the order book, it needs to be bigger than a minimum size.
%There is a maximum number of orders that can be stored in the order book at a time.
%If your order isn't big enough to be in the order book, you cannot buy shares of the type that are stored in the order book.
to_prove(OID, Trees) when (element(1, Trees) == trees) ->%
    Oracles = trees:oracles(Trees),%
    {_, Oracle, _} = oracles:get(OID, Oracles),%
    Orders = Oracle#oracle.orders,%
    orders:all(Orders);%
to_prove(OID, Trees) when (element(1, Trees) == trees2) -> 
    Root = trees:unmatched(Trees),
    X = 8 * 65,
    Head = unmatched:get({key, <<1:X>>, OID}, Root),
    unmatched:all(Root, OID).
    
from(X) -> X#oracle_bet.from.
id(X) -> X#oracle_bet.id.
make_dict(From, Fee, OID, Type, Amount) ->
    <<_:256>> = OID,
    Acc = trees:get(accounts, From),
    Tx = #oracle_bet{
       from = From, 
       nonce = Acc#acc.nonce + 1,
       fee = Fee,
       id = OID,
       type = Type,
       amount = Amount}.
make(From, Fee, OID, Type, Amount, Trees) ->
    <<_:256>> = OID,
    Accounts = trees:accounts(Trees),
    {_, Acc, _Proof} = accounts:get(From, Accounts),
    Tx = #oracle_bet{
       from = From, 
       nonce = Acc#acc.nonce + 1,
       fee = Fee,
       id = OID,
       type = Type,
       amount = Amount
	   },
    {Tx, []}.
merge_sort(L) ->
    M = to_singles(L),
    merge2(M).
to_singles([]) -> [];
to_singles([H|T]) -> [[H]|to_singles(T)].
merge2([]) -> [];
merge2([L]) -> L;
merge2(L) -> merge2(improve(L)).
improve([]) -> [];
improve([X]) -> [X];
improve([A|[B|T]]) -> [merge3(A, B)|improve(T)].
merge3([], X) -> X;
merge3(X, []) -> X;
merge3([A|Ta], [B|Tb]) when A > B -> [A|merge3(Ta, [B|Tb])];
merge3([A|Ta], [B|Tb]) -> [B|merge3([A|Ta], Tb)].
det_pow(X, 1) -> X;
det_pow(Base, Ex) ->
    B = Ex rem 2,
    case B of
	0 -> det_pow(Base*Base, Ex div 2);
	1 -> Base * det_pow(Base, Ex - 1)
    end.
dict_give_bets_main(Id, Orders, Type, Dict, OID, Height) ->
    %Id bought many orders of the same type. sum up all the amounts, and give him this many bets.
    F10 = Height > forks:get(10),
    Amount = sum_order_amounts(Orders, 0, F10),
    F = if %
	F10 -> matched;
	true -> oracle_bets%
    end,%
    F:dict_add_bet(Id, OID, Type, 2*Amount, Dict).
sum_order_amounts([], N, _) -> N;
sum_order_amounts([H|T], N, F10) -> 
    F = if%
	    F10 -> unmatched;
	    true -> orders%
	end,%
    A = F:amount(H),
    sum_order_amounts(T, A+N, F10).
dict_give_bets([], _Type, Dict, _, _) -> Dict;
dict_give_bets([Order|T], Type, Dict, OID, Height) ->
    F10 = Height > forks:get(10),
    {UF, F} = if %
	    F10 -> {unmatched, matched};
	    true -> {orders, oracle_bets}%
	end,%
    ID = UF:aid(Order),
    Dict2 = F:dict_add_bet(ID, OID, Type, 2*(UF:amount(Order)), Dict),
    dict_give_bets(T, Type, Dict2, OID, Height).
go(Tx, Dict, NewHeight, NonceCheck) ->
    From = Tx#oracle_bet.from,
    %txs:developer_lock(From, NewHeight, Dict),
    Nonce = if
		NonceCheck -> Tx#oracle_bet.nonce;
		true -> none
	    end,
    Facc = accounts:dict_update(From, Dict, -Tx#oracle_bet.fee - Tx#oracle_bet.amount, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),
    <<_:256>> = Tx#oracle_bet.id,
    Oracle = oracles:dict_get(Tx#oracle_bet.id, Dict2),
    0 = Oracle#oracle.result,%check that the oracle isn't already closed.
    go2(Tx, Dict2, NewHeight).
go2(Tx, Dict, NewHeight) -> %doit is split into two pieces because when we close the oracle we want to insert one last bet.
    From = Tx#oracle_bet.from,
    OID = Tx#oracle_bet.id,
    Oracle0 = oracles:dict_get(OID, Dict),
    OIL = governance:dict_get_value(oracle_initial_liquidity, Dict),
    F10 = NewHeight > forks:get(10),
    UMT = if%
	      F10  -> unmatched;
	      true -> orders%
	  end,%
    VolumeCheck = UMT:dict_significant_volume(Dict, OID, OIL),
    MOT = governance:dict_get_value(minimum_oracle_time, Dict),
    Oracle = if
    %if the volume of trades it too low, then reset the done_timer to another week in the future.
		 VolumeCheck -> Oracle0;
		 true -> 
                     Oracle0#oracle{done_timer = NewHeight + MOT}
	     end,
    true = NewHeight > Oracle#oracle.starts,
    %take some money from them. 
    OracleType = Oracle#oracle.type,%This shouldn't be 0 for the test we are doing.
    TxType = case Tx#oracle_bet.type of
		 1 -> 1;
		 2 -> 2;
		 3 -> 3
	     end,
    Amount = Tx#oracle_bet.amount,
    true = Amount > 0,
    NewOrder = UMT:new(Tx#oracle_bet.from, OID, Amount),
    Out = 
        if
	TxType == OracleType ->
                ManyOrders = dict_orders_many(OID, Dict, UMT),
                Minimum = OIL * det_pow(2, max(1, ManyOrders)), 
                true = Amount >= Minimum,
                Dict2 = UMT:dict_add(NewOrder, OID, Dict),
                oracles:dict_write(Oracle, Dict2);
	true ->
                {Matches1, Matches2, Next, Dict2} =
                    UMT:dict_match(NewOrder, OID, Dict),
    %Match1 is orders that are still open.
    %Match2 is orders that are already closed. We need to pay them their winnings.
		F8 = (NewHeight > forks:get(8)),
		M3 = case F8 of%
			 true -> Matches2;
			 false -> Matches1%
		     end,%
		Dict3 = dict_give_bets_main(From, M3, TxType, Dict2, Oracle#oracle.id, NewHeight),
                Dict4 = dict_give_bets(Matches2, OracleType, Dict3, Oracle#oracle.id, NewHeight),%gives oracle_bets to each account that got matched
                Oracle3 = case Next of
                              same -> 
                                  io:fwrite("oracle_bet_tx same type\n"),
                                  Oracle;
                              switch ->
                                  io:fwrite("oracle_bet_tx switch types\n"),
                                  Oracle#oracle{done_timer = NewHeight + MOT, type = TxType}
                          end,
                oracles:dict_write(Oracle3, Dict4)
        end,
    Out.
dict_orders_many(OID, Dict, UMT) ->
    {_, Many} = UMT:dict_head_get(Dict, OID),
    Many.
    
