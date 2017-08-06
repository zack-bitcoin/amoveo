-module(governance).

-export([genesis_state/0, change/3,
         lock/2, unlock/2, is_locked/1,
         tree_number_to_value/1, det_power/3,
         serialize/1, write/2,
         get_value/2, get/2,
         name2number/1,
         max/0,
         test/0]).

-record(gov, {id, value, lock}).

-define(name, governance).


genesis_state() ->
    {BlockTime, MinimumOracleTime, MaximumOracleTime} =
        case application:get_env(ae_core, test_mode, false) of
            true -> {1, 1, 1};
            false -> {297, 352, 505}
        end,
    G = [[block_reward, 1800],
         [developer_reward, 1520], 
         [time_gas, 1113],
         [space_gas, 1113],
         [max_block_size, 940],
         [create_channel_fee, 250],
         [delete_channel_reward, 240],
         [create_account_fee, 250],
         [delete_account_reward, 240],
         %[channel_rent, 600],
         %[account_rent, 600],
         [block_time, BlockTime],
         [oracle_future_limit, 335],
         [shares_conversion, 575],
         [fun_limit, 350],
         [var_limit, 600],
         [comment_limit, 137], 
         [block_creation_maturity, 100],
         [oracle_initial_liquidity, 1728],
         [minimum_oracle_time, MinimumOracleTime],
         [maximum_oracle_time, MaximumOracleTime],
         [maximum_question_size, 352],
         [block_time_after_median, 100],
         [channel_closed_time, 352],
         [question_delay, 216],
         [governance_delay, 72],
         [governance_change_limit, 51],
         [ca, 10],
         [spend, 10],
         [da, 5],
         [repo, 5],
         [nc, 10],
         [gc, 10],
         [ctc, 10],
         [cr, 5],
         [csc, 10],
         [timeout, 10],
         [cs, 10],
         [ex, 10],
         [oracle_new, 10],
         [oracle_bet, 10],
         [oracle_close, 10],
         [unmatched, 10],
         [oracle_shares, 10]],
    {ok, GenesisTree} = genesis_state(G, 0),
    GenesisTree.

genesis_state([], Tree) ->
    {ok, Tree};
genesis_state([[Name, Value] | Rest], Tree0) ->
    Id = name2number(Name),
    NewGovernance = new(Id, Value),
    Tree = write(NewGovernance, Tree0),
    genesis_state(Rest, Tree).

change(Name, Amount, Tree) ->
    {_, Gov0, _} = get(Name, Tree),
    Value0 = Gov0#gov.value + Amount,
    Value = max(Value0, 1),
    Gov = Gov0#gov{value = Value, lock = 0},
    write(Gov, Tree).

lock(Name, Tree) ->
    {_, Gov0, _} = get(Name, Tree),
    Gov = Gov0#gov{lock = 1},
    write(Gov, Tree).

unlock(Name, Tree) ->
    {_, Gov0, _} = get(Name, Tree),
    Gov = Gov0#gov{lock = 0},
    write(Gov, Tree).

is_locked(Gov) ->
    case Gov#gov.lock of
        0 ->
            false;
        1 ->
            true
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

get_value(Name, Tree) ->
    {_, Gov, _} = get(Name, Tree),
    tree_number_to_value(Gov#gov.value).

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

name2number(block_reward) -> 1;
name2number(time_gas) -> 2;
name2number(space_gas) -> 27;
name2number(max_block_size) -> 3;
name2number(create_channel_fee) -> 4;
name2number(delete_channel_reward) -> 5;
name2number(create_account_fee) -> 6;
name2number(delete_account_reward) -> 7;
%name2number(channel_rent) -> 9;
%name2number(account_rent) -> 10;
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
name2number(question_delay) -> 24;
name2number(governance_delay) -> 25;
name2number(governance_change_limit) -> 26;
name2number(ca) -> 28;%these store the minimum fee for each transaction type. "ca" is the name of the record of the create_account_tx.
name2number(spend) -> 29;
name2number(da) -> 30;
name2number(repo) -> 31;
name2number(nc) -> 32;
name2number(gc) -> 33;
name2number(ctc) -> 34;
name2number(cr) -> 35;
name2number(csc) -> 36;
name2number(timeout) -> 37;
name2number(cs) -> 38;
name2number(ex) -> 39;
name2number(oracle_new) -> 40;
name2number(oracle_bet) -> 41;
name2number(oracle_close) -> 42;
name2number(unmatched) -> 43;
name2number(oracle_shares) -> 44;
name2number(developer_reward) -> 45;
name2number(_) -> bad.
max() -> 46.


%% Internals

%% Try to fit everything into 32-bit values
new(Id, Value) ->
    new(Id, Value, 0).
new(Id, Value, Lock) ->
    #gov{id = Id, value = Value, lock = Lock}.

write(Gov, Tree) ->
    Key = Gov#gov.id,
    Serialized = serialize(Gov),
    trie:put(Key, Serialized, 0, Tree, ?name).

deserialize(SerializedGov) ->
    <<Id:8, Value:16, Lock:8>> = SerializedGov,
    #gov{id = Id, value = Value, lock = Lock}.


%% Tests

test() ->
    NewGov = new(14, 1, 0),
    {Trees, _, _} = tx_pool:data(),
    Gov = trees:governance(Trees),
    {_, {gov, 14, 350, 0}, _} = get(fun_limit, Gov),
    G2 = write(NewGov, Gov),
    {_, NewGov, _} = get(fun_limit, G2),
    {_, {gov, 14, 350, 0}, _} = get(fun_limit, Gov),
    success.
