-module(amoveo_utils).
-export([tuples2lists/1,
	 block_rewards/1,
	 block_rewards/2,
	 tx_history/1, tx_history/2, tx_history/3,
	 address_history/2,address_history/3,address_history/4,
	 push_txs/0
	]).
-include("records.hrl").

address_history(Mode, X) ->
    TB = block:top(),
    address_history(Mode, X, 200).
address_history(Mode, X, Many) ->
    address_history(Mode, X, Many, block:height()).
address_history(Mode, X, Many, End) when (is_list(X) orelse (size(X) > 65))->
    address_history(Mode, base64:decode(X), Many, End);
address_history(Mode, X, Many, End) when size(X) > 65 ->
    address_history(Mode, base64:decode(X), Many, End);
address_history(Mode, X, Many, End) ->
    TB = block:get_by_height(End),
    Finish = max(TB#block.height - Many, 0),
    address_history2(Mode, X, TB, Finish, []).
address_history2(_, X, Block, Finish, Out) 
  when Finish == Block#block.height -> Out;
address_history2(Mode, X, Block, Finish, Out) ->
    CB = hd(Block#block.txs),
    if
	Mode == verbose ->
	    io:fwrite("cb "),
	    io:fwrite(packer:pack(element(2, CB))),
	    io:fwrite(" height "),
	    io:fwrite(integer_to_list(Block#block.height)),
	    io:fwrite("\n");
	true -> ok
    end,
    Txs = tl(Block#block.txs),
    H = Block#block.height,
    K = address_txs(Mode, X, Txs, [], H),
    PB = block:get_by_hash(Block#block.prev_hash),
    address_history2(Mode, X, PB, Finish, Out ++ K).
address_txs(_, _, [], Out, _) -> Out;
address_txs(Mode, Key, [X|T], Out, Height) ->
    Tx = element(2, X),
    New = case element(1, Tx) of
	      spend -> 
		  From = Tx#spend.from,
		  To = Tx#spend.to,
		  Amount = Tx#spend.amount,
		  spend_common(Mode, Tx, Key, From, To, Amount, Height);
	      create_acc_tx ->
		  From = Tx#create_acc_tx.from,
		  To = Tx#create_acc_tx.pubkey,
		  Amount = Tx#create_acc_tx.amount,
		  spend_common(Mode, Tx, Key, From, To, Amount, Height);
	      _ -> ""
	  end,
    address_txs(Mode, Key, T, New ++ Out, Height).
spend_common(Mode, Tx, Key, From, To, Amount, Height) ->
    case Key of
	From -> 
	    if
		Mode == verbose ->
		    io:fwrite("gave "),
		    io:fwrite(integer_to_list(Amount)),
		    io:fwrite(" to "),
		    io:fwrite(base64:encode(To)),
		    io:fwrite(" at "),
		    io:fwrite(integer_to_list(Height)),
		    io:fwrite("\n");
		true -> ok
	    end,
	    [{Height, Tx}];
	To ->
	    if 
		Mode == verbose ->
		    io:fwrite("received "),
		    io:fwrite(integer_to_list(Amount)),
		    io:fwrite(" from "),
		    io:fwrite(base64:encode(From)),
		    io:fwrite(" at "),
		    io:fwrite(integer_to_list(Height)),
		    io:fwrite("\n");
		true -> ok
	    end,
	    [{Height, Tx}];
	_ -> []
    end.



%% convert tuples to lists so they can pretend json
tuples2lists(X) when is_tuple(X) ->
    tuples2lists(tuple_to_list(X));
tuples2lists([]) -> [];
tuples2lists([H|T]) ->
    [tuples2lists(H)|tuples2lists(T)];
tuples2lists(X) -> X.



block_rewards(A) ->
    T = block:top(),
    block_rewards(base64:decode(A), T).
block_rewards(A, T) ->
    block_rewards2(A, T, dict:new()).
block_rewards2(A, B, D) ->
    H = B#block.height,
    if
	H < 1 -> display(D);
	true ->
	    S = spends(tl(B#block.txs)),
	    S2 = from(A, S),
	    %io:fwrite(packer:pack(S2)),
	    %io:fwrite(" from \n"),
	    D2 = accumulate(S2, D),
	    PH = B#block.prev_hash,
	    PB = block:get_by_hash(PH),
	    block_rewards2(A, PB, D2)
    end.
display(D) ->
    L = dict:to_list(D),
    L2 = lists:reverse(lists:keysort(2, L)),
    display2(L2).
display2([]) -> ok;
display2([{K, V}|T]) ->
    <<X:80, _/binary>> = base64:encode(K),
    io:fwrite(integer_to_list(V div 100000000)),
    io:fwrite(" "),
    io:fwrite(<<X:80>>),
    io:fwrite("\n"),
    display2(T).
spends([]) -> [];
spends([Tx|T]) ->
    UT = element(2, Tx),
    case element(1, UT) of
	spend -> 
	    A = UT#spend.amount,
	    if
		A > 160000000 -> spends(T);
		true -> [UT|spends(T)]
	    end;
	_ -> spends(T)
    end.
from(_, []) -> [];
from(A, [H|T]) ->
    F = H#spend.from,
    if
	F == A -> [H|from(A, T)];
	true -> from(A, T)
    end.
accumulate([], D) -> D;
accumulate([H|T], D) ->
    A = H#spend.amount,
    To = H#spend.to,
    D2 = increase(To, A, D),
    accumulate(T, D2).
increase(To, A, D) ->
    case dict:find(A, D) of
	error -> dict:store(To, A, D);
	{ok, V} -> dict:store(To, A+V, D)
    end.
	    
	    
	    
	
tx_history(End) ->    
    tx_history(block:height(), End).
tx_history(Start, End) ->    
    Types = [oracle_new, oracle_bet, oracle_close, unmatched, oracle_winnings],
    tx_history(Start, End, Types).
tx_history(Start, End, Types) ->
    S = block:get_by_height(Start),
    tx_history2(S, End, Types, []).
tx_history2(S, End, Types, Out) when S#block.height < End ->
    Out;
tx_history2(B, End, Types, Out) ->
    Txs = tl(B#block.txs),
    Txs2 = get_types(Types, Txs),
    PH = B#block.prev_hash,
    PB = block:get_by_hash(PH),
    tx_history2(PB, End, Types, Out ++ Txs2).
get_types(Types, []) -> [];
get_types(Types, [H|T]) ->
    %io:fwrite(packer:pack(H)),
    Type = element(1, element(2, H)),
    B = is_in(Type, Types),
    if
	B -> [H|get_types(Types, T)];
	true -> get_types(Types, T)
    end.
is_in(_, []) -> false;
is_in(A, [A|_]) -> true;
is_in(A, [_|T]) -> is_in(A, T).
push_txs() ->	    
    lists:map(fun(P) -> 
		      spawn(fun() -> 
				    sync:trade_txs(P) 
			    end)
	      end,
	      peers:all()).
		      
