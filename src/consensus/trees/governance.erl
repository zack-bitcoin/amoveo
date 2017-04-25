-module(governance).
-export([det_power/3,tree_number_to_value/1, max/0,
	 is_locked/1, change/3, genesis_state/0,
	 get/2, write/2, lock/1, unlock/1,
	 test/0]).

-record(gov, {id, value, lock}).
-define(name, governance).

%try to fit everything into 32-bit values.
new(ID, Value, Lock) ->
    #gov{id = ID, value = Value, lock = Lock}.
genesis_state() ->
    G = [[block_reward, 1800],
	 [gas_limit, 1113],
	 [max_block_size, 940],
	 [create_channel_fee, 250],
	 [delete_channel_reward, 240],
	 [create_account_fee, 250],
	 [delete_account_reward, 240],
	 [channel_rent, 600],
	 [account_rent, 600],
	 [block_time, 297],
	 [oracle_future_limit, 335],
	 [shares_conversion, 575],
	 [fun_limit, 350],
	 [var_limit, 600],
	 [comment_limit, 137], 
	 [block_creation_maturity, 100],
	 [oracle_initial_liquidity, 1728],
	 [minimum_oracle_time, 352],
	 [maximum_oracle_time, 505],
	 [maximum_question_size, 352],
	 [block_time_after_median, 100],
	 [channel_closed_time, 352],
	 [retarget_period, 429],
	 [question_delay, 216],
	 [governance_delay, 72],
	 [governance_change_limit, 51]],
    genesis_state2(G, 0).
genesis_state2([], T) -> T;
genesis_state2([[Name, Value]|T], Tree) -> 
    Tree2 = write(Name, Value, 0, Tree),
    genesis_state2(T, Tree2).
change(Name, Amount, Tree) ->
    {_, V, _} = get(Name, Tree),
    V2 = V#gov{value = V#gov.value + Amount, lock = 0},
    write(V2, Tree).
unlock(X) -> 
    X#gov{lock = 0}.
lock(X) -> 
    X#gov{lock = 1}.
is_locked(V) ->
    X = V#gov.lock,
    case X of
	0 -> false;
	1 -> true
    end.
tree_number_to_value(T) when T<101 -> T;
tree_number_to_value(T) ->
    tree_number_to_value_exponential(T-100).
tree_number_to_value_exponential(T) ->
    Top = 101,
    Bottom = 100,
    det_power(Top, Bottom, T).
det_power(Top, Bottom, T) ->
    det_power(10000, Top, Bottom, T) div 100.
det_power(Base, Top, Bottom, 1) -> 
    (Base * Top) div Bottom;
det_power(Base, Top, Bottom, T) ->
    R = T rem 2,
    case R of
	1 ->
	    B2 = (Base * Top) div Bottom,
	    det_power(B2, Top, Bottom, T-1);
	0 ->
	    det_power(Base, (Top*Top) div Bottom, Bottom, T div 2)
    end.
serialize(G) ->
    <<(G#gov.id):8,
      (G#gov.value):16,
      (G#gov.lock):8>>.
deserialize(B) ->
    <<ID:8, Val:16, Lock:8>> = B,
    #gov{id = ID, value = Val, lock = Lock}.
write(Name, Value, Lock, Tree) ->
    ID = name2number(Name),
    E = new(ID, Value, Lock),
    write(E, Tree).
write(E, Tree) ->
    Key = E#gov.id,
    X = serialize(E),
    trie:put(Key, X, 0, Tree, ?name).
get(Name, Tree) when is_atom(Name) ->
    Key = name2number(Name),
    get(Key, Tree);
get(Key, Tree) when is_integer(Key) ->
    {X, Leaf, Proof} = trie:get(Key, Tree, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> Y = leaf:value(L),
		 deserialize(Y)
	end,
    {X, V, Proof}.
number2name(1) -> block_reward;
number2name(2) -> gas_limit;
number2name(3) -> max_block_size;
number2name(4) -> create_channel_fee;
number2name(5) -> delete_channel_reward;
number2name(6) -> create_account_fee;
number2name(7) -> delete_account_reward;
number2name(9) -> channel_rent;
number2name(10) -> account_rent;
number2name(11) -> block_time;
number2name(12) -> oracle_future_limit;
number2name(13) -> shares_conversion;
number2name(14) -> fun_limit;
number2name(15) -> var_limit;
number2name(16) -> comment_limit;
number2name(17) -> block_creation_maturity;
number2name(18) -> oracle_initial_liquidity;
number2name(19) -> minimum_oracle_time;
number2name(20) -> maximum_question_size;
number2name(21) -> block_time_after_median;
number2name(22) -> channel_closed_time;
number2name(23) -> retarget_period;
number2name(24) -> question_delay;
number2name(25) -> governance_delay;
number2name(_) -> bad.
name2number(block_reward) -> 1;
name2number(gas_limit) -> 2;
name2number(max_block_size) -> 3;
name2number(create_channel_fee) -> 4;
name2number(delete_channel_reward) -> 5;
name2number(create_account_fee) -> 6;
name2number(delete_account_reward) -> 7;
name2number(channel_rent) -> 9;
name2number(account_rent) -> 10;
name2number(block_time) -> 11;
name2number(oracle_future_limit) -> 12;
name2number(shares_conversion) -> 13;
name2number(fun_limit) -> 14;
name2number(var_limit) -> 15;
name2number(comment_limit) -> 16;
name2number(block_creation_maturity) -> 17;
name2number(oracle_initial_liquidity) -> 18;
name2number(minimum_oracle_time) -> 19;
name2number(maximum_oracle_time) -> 8;
name2number(maximum_question_size) -> 20;
name2number(block_time_after_median) -> 21;
name2number(channel_closed_time) -> 22;
name2number(retarget_period) -> 23;
name2number(question_delay) -> 24;
name2number(governance_delay) -> 25;
name2number(governance_change_limit) -> 26;
name2number(_) -> bad.
max() -> 27.

test() ->
    C = new(14, 1, 0),
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    {_, {gov, 14, 350, 0}, _} = get(fun_limit, Governance),
    G2 = write(C, Governance),
    {_, C, _} = get(fun_limit, G2),
    {_, {gov, 14, 350, 0}, _} = get(fun_limit, Governance),
    success.
    
