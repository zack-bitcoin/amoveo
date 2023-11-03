-module(governance).
-export([tree_number_to_value/1, max/1, is_locked/1, genesis_state/0, name2number/1, number2name/1,%custom for this tree
	 get_value/2, get/2, write/2,%update tree stuff
         dict_get/2,dict_get/3,dict_write/2, dict_write_new/2, dict_get_value/3, dict_lock/2, dict_unlock/2, dict_change/3, %update dict stuff
         verify_proof/4,make_leaf/3,key_to_int/1,
	 serialize/1,deserialize/1,
	 new/2, hard_coded/2, value/1,
	 test/0]).%common tree stuff
-define(name, governance).
-define(fee, constants:encoded_fee()).
-include("../../records.hrl").

hard_coded(_, 1) -> 1370;%block reward
hard_coded(_, 2) -> 429;%developer reward
hard_coded(_, 3) -> 890;%max block size
hard_coded(_, 4) -> %block period
    case application:get_env(amoveo_core, test_mode, false) of
        true -> 5;
        false -> 550
    end;
hard_coded(_, 5) -> 1113;%time gas
hard_coded(_, 6) -> 1113;%space gas
hard_coded(_, 7) -> 350;%fun limit
hard_coded(_, 8) -> 600;%var limit
hard_coded(_, 9) -> 51;%gov change limit
hard_coded(_, 10) -> 1500;%oracle initial liquidity
hard_coded(_, 11) -> %minimum oracle time
    case application:get_env(amoveo_core, test_mode, false) of
        true -> 1;
        false -> 352
    end;
hard_coded(_, 12) -> %maximum oracle time
    case application:get_env(amoveo_core, test_mode, false) of
        true -> 1;
        false -> 505
    end;
hard_coded(_, 13) -> 352;%maximum question size
hard_coded(_, 14) -> 905;%create account tx
hard_coded(_, 15) -> 805;%spend tx
hard_coded(_, 16) -> 0;%delete tx
hard_coded(_, 17) -> 905;%nc
hard_coded(_, 18) -> 905;%ctc
hard_coded(_, 19) -> 905;%channel solo close
hard_coded(_, 20) -> 905;%channel timeout
hard_coded(_, 21) -> 905;%channel slash
hard_coded(_, 22) -> 905;%existence
hard_coded(_, 23) -> 905;%oracle new
hard_coded(_, 24) -> 905;%oracle bet
hard_coded(_, 25) -> 905;%oracle close
hard_coded(_, 26) -> 905;%unmatched
hard_coded(_, 27) -> 905;%oracle winnings
hard_coded(_, 28) -> 1100;%oracle question liquidity
hard_coded(_, 29) -> 905;%conctract_new_tx
hard_coded(_, 30) -> 905;%contract_use_tx
hard_coded(_, 31) -> 905;%sub_spend_tx
hard_coded(_, 32) -> 905;%contract_evidence_tx
hard_coded(_, 33) -> 905;%contract_timeout_tx
hard_coded(_, 34) -> 905;%contract_winnings_tx
hard_coded(_, 35) -> 905;%contract_simplify_tx
hard_coded(_, 36) -> 32;%max_contract_flavors
hard_coded(_, 37) -> 905;%swap_tx
hard_coded(_, 38) -> 905;%market_new_tx
hard_coded(_, 39) -> 905;%market_liquidity_tx
hard_coded(_, 40) -> 905;%market_swap_tx
hard_coded(_, 41) -> 936;%market_trading_fee
hard_coded(_, 42) -> 905;%swap_tx2
hard_coded(_, 43) -> 905;%trade_cancel_tx
hard_coded(_, 44) -> 905;%stablecoin_new_tx
hard_coded(_, 45) -> 905;%job_create_tx
hard_coded(_, 46) -> 905;%job_receive_salary_tx
hard_coded(_, 47) -> 905;%job_buy_tx
hard_coded(_, 48) -> 905;%job_adjust_tx
hard_coded(_, 49) -> 905.%job_team_adjust_tx



genesis_state() ->
    {MinimumOracleTime, MaximumOracleTime, BlockPeriod} =
        case application:get_env(amoveo_core, test_mode, false) of
            %true -> {1, 1, 100};
	    true -> {1, 1, 5};
            false -> {352, 505, 550}
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
    R = trees:empty_tree(governance),
    {ok, GenesisTree} = genesis_state(G, R),
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
name2number(nc_accept) -> 17;
name2number(ctc) -> 18;
name2number(ctc2) -> 18;
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
name2number(contract_new_tx) -> 29;
name2number(contract_use_tx) -> 30;
name2number(sub_spend_tx) -> 31;
name2number(contract_evidence_tx) -> 32;
name2number(contract_timeout_tx) -> 33;
name2number(contract_winnings_tx) -> 34;
name2number(contract_simplify_tx) -> 35;
name2number(max_contract_flavors) -> 36;
name2number(swap_tx) -> 37;
name2number(market_new_tx) -> 38;
name2number(market_liquidity_tx) -> 39;
name2number(market_swap_tx) -> 40;
name2number(market_trading_fee) -> 41;
name2number(swap_tx2) -> 42;
name2number(trade_cancel_tx) -> 43;
name2number(stablecoin_new_tx) -> 44;
name2number(job_create_tx) -> 45;
name2number(job_receive_salary_tx) -> 46;
name2number(job_buy_tx) -> 47;
name2number(job_adjust_tx) -> 48;
name2number(job_team_adjust_tx) -> 49;
name2number(X) -> 
    io:fwrite(X),
    1=2,
    throw(invalid_governance_atom).
number2name(1) -> block_reward;
number2name(2) -> developer_reward;
number2name(3) -> max_block_size;
number2name(4) -> block_period;
number2name(5) -> time_gas;
number2name(6) -> space_gas;
number2name(7) -> fun_limit;
number2name(8) -> var_limit;
number2name(9) -> governance_change_limit;
number2name(10) -> oracle_initial_liquidity;
number2name(11) -> minimum_oracle_time;
number2name(12) -> maximum_oracle_time;
number2name(13) -> maximum_question_size;
number2name(14) -> create_acc_tx;
number2name(15) -> spend;
number2name(16) -> delete_acc_tx;
number2name(17) -> nc;
number2name(18) -> ctc;
number2name(19) -> csc;
number2name(20) -> timeout;
number2name(21) -> cs;
number2name(22) -> ex;
number2name(23) -> oracle_new;
number2name(24) -> oracle_bet;
number2name(25) -> oracle_close;
number2name(26) -> unmatched;
number2name(27) -> oracle_winnings;
number2name(28) -> oracle_question_liquidity;
number2name(29) -> contract_new_tx;
number2name(30) -> contract_use_tx;
number2name(31) -> sub_spend_tx;
number2name(32) -> contract_evidence_tx;
number2name(33) -> contract_timeout_tx;
number2name(34) -> contract_winnings_tx;
number2name(35) -> contract_simplify_tx;
number2name(36) -> max_contract_flavors;
number2name(37) -> swap_tx;
number2name(38) -> market_new_tx;
number2name(39) -> market_liquidity_tx;
number2name(40) -> market_swap_tx;
number2name(41) -> market_trading_fee;
number2name(42) -> swap_tx2;
number2name(43) -> trade_cancel_tx;
number2name(44) -> stablecoin_new_tx;
number2name(45) -> job_create_tx;
number2name(46) -> job_receive_salary_tx;
number2name(47) -> job_buy_tx;
number2name(48) -> job_adjust_tx;
number2name(49) -> job_team_adjust_tx;
number2name(X) ->
    io:fwrite(X),
    1=2,
    error.
    
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
    HashKey = trees2:hash_key(governance, Key),
    csc:update({governance, Key}, Gov, Dict).

dict_write_new(Gov, Dict) ->
    Key = Gov#gov.id,
    HashKey = trees2:hash_key(governance, Key),
    csc:add(governance, HashKey, {governance, Key}, 
            Gov, Dict).



%    dict:store({governance, Key},
%               %serialize(Gov),
%               Gov,
%               Dict).
write(Gov, Tree) ->
    Key = Gov#gov.id,
    Serialized = serialize(Gov),
    trie:put(Key, Serialized, 0, Tree, ?name).

%deserialize(G = #gov{}) -> G;
deserialize(SerializedGov) ->
    <<Id:8, Value:16, Lock:8>> = SerializedGov,
    #gov{id = Id, value = Value, lock = Lock}.


dict_get_value(Key, Dict, Height) 
  when is_integer(Height) ->
    F52 = forks:get(52),
    if
        Height < F52 -> dict_get_value(Key, Dict);
        true ->
            N = name2number(Key), 
            HC = hard_coded(-1, N),
            HC2 = tree_number_to_value(HC),
            case Key of
                timeout -> -HC2;
                delete_acc_tx -> -HC2;
                _ -> HC2
            end
    end;
dict_get_value(Key, Dict, _Trees) ->
    dict_get_value(Key, Dict).

%value({_, G, _}) ->
%    value(G);
value(G = #gov{value = V, id = K}) ->
    K2 = number2name(K),
    X = case K2 of
            timeout -> -1;
            delete_acc_tx -> -1;
            _ -> 1
        end,
    X * tree_number_to_value(V).
            
dict_get_value(Key, Dict) ->
    case dict_get(Key, Dict) of
	error -> error;
	empty -> empty;
	Gov ->
            value(Gov)
    end.
dict_get(Key0, Dict) ->
    dict_get(Key0, Dict, 0).
dict_get(Key0, Dict, Height) ->
    Key = if
              is_integer(Key0) -> Key0;
              true -> name2number(Key0)
          end,
    HashKey = trees2:hash_key(governance, Key),
    %case dict:find({governance, Key}, Dict) of
    case csc:read({governance, Key}, Dict) of
    %case csc:read(Key, Dict) of
	error -> empty;%this works because governance has a finite list of things. 
	%{ok, X} -> deserialize(X)
	{ok, governance, X} -> X
    end.


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
