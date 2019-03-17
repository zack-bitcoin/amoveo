-module(governance).
-export([tree_number_to_value/1, max/1, is_locked/1, genesis_state/0, name2number/1, %custom for this tree
	 get_value/2, get/2, write/2,%update tree stuff
         dict_get/2,dict_write/2, dict_get_value/2, dict_lock/2, dict_unlock/2, dict_change/3, %update dict stuff
         verify_proof/4,make_leaf/3,key_to_int/1,
	 serialize/1,deserialize/1,
	 new/2, dict_write/2,
	 test/0]).%common tree stuff
-record(gov, {id, value, lock}).
-define(name, governance).
-define(fee, constants:encoded_fee()).
-include("../../records.hrl").
genesis_state() ->
    {MinimumOracleTime, MaximumOracleTime, BlockPeriod} =
        case application:get_env(amoveo_core, test_mode, false) of
            %true -> {1, 1, 100};
	    true -> {1, 1, 5};
            false -> {352, 505, 320}
        end,
    G = [[block_reward, 1620],
         [developer_reward, 429], 
         [time_gas, 1113],
         [space_gas, 1113],
         [max_block_size, 940],
         [block_period, BlockPeriod],
         [fun_limit, 350],
         [var_limit, 600],
         [oracle_initial_liquidity, 1500],
         [minimum_oracle_time, MinimumOracleTime],
         [maximum_oracle_time, MaximumOracleTime],
         [maximum_question_size, 352],
         [governance_change_limit, 51],
         [create_acc_tx, ?fee],
         [spend, ?fee],
         [delete_acc_tx, 0],
         [nc, ?fee],
         [ctc, ?fee],
         [csc, ?fee],
         [timeout, ?fee],
         [cs, ?fee],
         [ex, ?fee],
         [oracle_new, ?fee],
         [oracle_bet, ?fee],
         [oracle_close, ?fee],
         [unmatched, ?fee],
         [oracle_winnings, ?fee]],
    {ok, GenesisTree} = genesis_state(G, 1),
    GenesisTree.

genesis_state([], Tree) ->
    {ok, Tree};
genesis_state([[Name, Value] | Rest], Tree0) ->
    Id = name2number(Name),
    NewGovernance = new(Id, Value),
    Tree = write(NewGovernance, Tree0),
    genesis_state(Rest, Tree).
dict_change(Name, Amount, Dict) ->
    Gov0 = dict_get(Name, Dict),
    Value0 = Gov0#gov.value + Amount,
    Value = max(Value0, 1),
    Gov = Gov0#gov{value = Value, lock = 0},
    dict_write(Gov, Dict).
dict_lock(Name, Dict) ->
    Gov0 = dict_get(Name, Dict),
    Gov = Gov0#gov{lock = 1},
    dict_write(Gov, Dict).
dict_unlock(Name, Dict) ->
    Gov0 = dict_get(Name, Dict),
    Gov = Gov0#gov{lock = 0},
    dict_write(Gov, Dict).
    
is_locked(Gov) ->
    case Gov#gov.lock of
        0 -> false;
        1 -> true
    end.

tree_number_to_value(T) when T < 101 ->
    T;
tree_number_to_value(T) ->
    tree_number_to_value_exponential(T - 100).

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

serialize(Gov) ->
    <<(Gov#gov.id):8,
      (Gov#gov.value):16,
      (Gov#gov.lock):8>>.

get_value(coinbase, _) -> 0;
get_value(Name, Tree) ->
    {_, Gov, _} = get(Name, Tree),
    tree_number_to_value(Gov#gov.value).
key_to_int(X) when is_atom (X) ->
    name2number(X);
key_to_int(X) -> X.
get(Name, Tree) when is_atom(Name) ->
    case name2number(Name) of
        bad ->
            {error, unknown_name};
        Key ->
            get(Key, Tree)
    end;
get(Key, Tree) when is_integer(Key) ->
    {X, Leaf, Proof} = trie:get(Key, Tree, ?name),
    V = case Leaf of
            empty ->
                {error, empty_leaf};
            L ->
                LeafValue = leaf:value(L),
                deserialize(LeafValue)
        end,
    {X, V, Proof}.
%Blockchain variables
name2number(block_reward) -> 1;
name2number(developer_reward) -> 2;
name2number(max_block_size) -> 3;
name2number(block_period) -> 4;
%VM variables
name2number(time_gas) -> 5;
name2number(space_gas) -> 6;
name2number(fun_limit) -> 7;%how many functions can the vm make
name2number(var_limit) -> 8;%how many variables can the vm store
%Oracle variables
name2number(governance_change_limit) -> 9;
name2number(oracle_initial_liquidity) -> 10;
name2number(minimum_oracle_time) -> 11;
name2number(maximum_oracle_time) -> 12;
name2number(maximum_question_size) -> 13;
%Transaction fees
name2number(create_acc_tx) -> 14;%these store the minimum fee for each transaction type. "create_acc_tx" is the name of the record of the create_account_tx.
name2number(spend) -> 15;
name2number(delete_acc_tx) -> 16;
name2number(nc) -> 17;
name2number(ctc) -> 18;
name2number(csc) -> 19;
name2number(timeout) -> 20;
name2number(cs) -> 21;
name2number(ex) -> 22;
name2number(oracle_new) -> 23;
name2number(oracle_bet) -> 24;
name2number(oracle_close) -> 25;
name2number(unmatched) -> 26;
name2number(oracle_winnings) -> 27;
name2number(oracle_question_liquidity) -> 28;
name2number(nc_accept) -> 17;
name2number(X) -> 
    io:fwrite(X),
    1=2,
    throw(invalid_governance_atom).
max(Height) -> 
    B = Height > forks:get(5),
    if 
	B -> 29;
	true -> 28
    end.
make_leaf(Key, V, CFG) ->
    Key2 = if
               is_integer(Key) -> Key;
               true -> name2number(Key)
           end,
    leaf:new(Key2, V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).

%% Internals

%% Everything fits into 32-bit values
new(Id, Value) ->
    new(Id, Value, 0).
new(Id, Value, Lock) ->
    #gov{id = Id, value = Value, lock = Lock}.
dict_write(Gov, Dict) ->
    Key = Gov#gov.id,
    dict:store({governance, Key},
               serialize(Gov),
               Dict).
write(Gov, Tree) ->
    Key = Gov#gov.id,
    Serialized = serialize(Gov),
    trie:put(Key, Serialized, 0, Tree, ?name).

deserialize(SerializedGov) ->
    <<Id:8, Value:16, Lock:8>> = SerializedGov,
    #gov{id = Id, value = Value, lock = Lock}.

dict_get_value(Key, Dict) when ((Key == timeout) or (Key == delete_acc_tx)) ->
    case dict_get(Key, Dict) of
	empty -> empty;
	Gov ->
	    V = Gov#gov.value,
	    -tree_number_to_value(V)
    end;
dict_get_value(Key, Dict) ->
    case dict_get(Key, Dict) of
	empty -> empty;
	Gov ->
	    V = Gov#gov.value,
	    tree_number_to_value(V)
    end.
dict_get(Key, Dict) when is_integer(Key) ->
    case dict:find({governance, Key}, Dict) of
	error -> empty;
	{ok, X} -> deserialize(X)
    end;
dict_get(Key, Dict) ->
    dict_get(name2number(Key), Dict).


%% Tests

test() ->
    Num = name2number(fun_limit),
    C = new(Num, 1, 0),
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Governance = trees:governance(Trees),
    Leaf = {gov, Num, 350, 0},
    Leaf = deserialize(serialize(Leaf)),
    {_, Leaf, _} = get(fun_limit, Governance),
    G2 = write(C, Governance),
    {_, C2, _} = get(fun_limit, G2),
    io:fwrite(packer:pack([C, C2])),
    C = C2,
    {Root, Leaf, Proof} = get(fun_limit, Governance),
    true = verify_proof(Root, fun_limit, serialize(Leaf), Proof),
    success.
