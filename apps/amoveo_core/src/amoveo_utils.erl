-module(amoveo_utils).
-export([tuples2lists/1,
	 block_rewards/1,
	 block_rewards/2,
	 tx_history/1, tx_history/2, tx_history/3,
	 address_history/2,address_history/3,address_history/4,
	 push_txs/0, key_full_to_light/1, iterator/2,
         recent_miners/1, all_keys/0, scan_db_top/0
	]).
-include("records.hrl").

scan_db_top() ->
    CFG = tree:cfg(amoveo),
    trees2:scan_verkle(element(10, block:top()), CFG).
    
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
bin_to_hex(<<>>) ->
    "";
bin_to_hex(<<X:4, R/bitstring>>) when (X<10) ->
    integer_to_list(X) ++
        bin_to_hex(R);
bin_to_hex(<<10:4, R/bitstring>>) ->
    "a" ++ bin_to_hex(R);
bin_to_hex(<<11:4, R/bitstring>>) ->
    "b" ++ bin_to_hex(R);
bin_to_hex(<<12:4, R/bitstring>>) ->
    "c" ++ bin_to_hex(R);
bin_to_hex(<<13:4, R/bitstring>>) ->
    "d" ++ bin_to_hex(R);
bin_to_hex(<<14:4, R/bitstring>>) ->
    "e" ++ bin_to_hex(R);
bin_to_hex(<<15:4, R/bitstring>>) ->
    "f" ++ bin_to_hex(R).



key_full_to_light(K) ->		      
    B = base64:decode(K),
    bin_to_hex(B).

iterator(_, 0) -> [];
iterator(X, N) when N > 0 -> 
    [X|iterator(X, N-1)].

recent_miners(L) ->
    recent_miners2(block:height(), L).
recent_miners2(0, _) -> ok;
recent_miners2(N, L) -> 
    B = block:get_by_height(N),
    Tx = hd(element(11, B)),
    A = element(2, Tx),
    Bool = is_in(A, L),
    if 
        Bool -> recent_miners2(N-1, L);
        true -> {A, N}
    end.

miner_pool_summary(Address) ->
    %scan the recent 200 blocks. record every time this address made a payment, or found a block.
    B = block:top(),
    miner_pool_summary2(200, Address, B, [], []).
miner_pool_summary2(0, _, _, Heights, Payments) ->
    {Heights, Payments};
miner_pool_summary2(N, Address, B, Heights, Payments) ->
    #block{txs = [CB|Txs], height = H, prev_hash = PH} = B,
    #coinbase{from = From} = CB,
    Heights2 = case From of
                  Address -> [H|Heights];
                  _ -> Heights
              end,
    Payments2 = merge_payments(Txs, Address, Payments),
    miner_pool_summary2(N-1, Address, block:get_by_hash(PH),
                        Heights2, Payments2).
merge_payments([Tx|Txs], Address, Payments) ->
    T = element(1, Tx),
    if
        ((T == spend) and (element(2, Tx) == Address)) ->
            merge_payments(Txs, Address, [element(5, Tx)|Payments]);
        true -> merge_payments(Txs, Address, Payments)
    end;
merge_payments([], _, P) -> P.

            
                                       
                      
    
            
    
all_keys() ->
    Height = block:height(),
    S = forks:get(52),
    potential_block:new(),
    {_, Keys} = (potential_block:check())#block.proofs,
    {Keys, all_keys2(S+1, Height)}.
    

all_keys2(H, HeightLimit) 
  when H > HeightLimit -> [];
all_keys2(H, HL) -> 
    B = block:get_by_height(H),
    #block{proofs = P} = B,
    {_, Keys} = P,
    merge_lists(Keys, all_keys2(H+1, HL)).

merge_lists([], K) -> K;
merge_lists([H|T], K) ->
    IS_IN = is_in_list(H, K),
    if
        IS_IN -> merge_lists(T, K);
        true -> merge_lists(T, [H|K])
    end.
is_in_list(_, []) -> false;
is_in_list(A, [A|_]) -> true;
is_in_list(A, [_|T]) -> 
    is_in_list(A, T).
            

           
    
