-module(testnet_sup).
-behaviour(supervisor).
-export([start_link/0,init/1, stop/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, infinity, Type, [I]}).
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
-define(keys, [keys, 
	       block_hashes, 
               headers, %top,
	       block_absorber,
	       tx_pool, 
	       peers, tx_pool_feeder, 
	       mine, channel_manager, channel_feeder,
	       request_frequency, sync, secrets,
	       arbitrage, order_book, oracle_questions]).
child_killer([]) ->
    [];
child_killer([H|T]) -> 
    supervisor:terminate_child(testnet_sup, H),
    child_killer(T).
stop() -> 
    child_killer(?keys).

child_maker([]) -> [];
child_maker([H|T]) -> [?CHILD(H, worker)|child_maker(T)].

tree_child(Id, KeySize, Size) ->
    tree_child(Id, KeySize, Size, 0).
tree_child(Id, KeySize, Size, Meta) ->
    {ok, Amount} = application:get_env(ae_core, trie_size),
    Sup = list_to_atom(atom_to_list(Id) ++ "_sup"),
    {Sup, {trie_sup, start_link, [KeySize, Size, Id, Amount, Meta, constants:hash_size(), hd]}, permanent, 5000, supervisor, [trie_sup]}.
init([]) ->
    KL = constants:key_length(), 
    AB = constants:address_bits(),
    HS = constants:hash_size(),
    PS = constants:pubkey_size(),
    FullLength = KL*2,
    BB = constants:balance_bits(),
    Children = child_maker(?keys),
    HB = constants:height_bits(),
    DB = constants:difficulty_bits(),
    Tries = [
	     tree_child(accounts, HS, constants:account_size(), KL div 8),
	     tree_child(channels, HS, constants:channel_size()),
	     tree_child(existence, HS, HS + (HB div 8)),
	     tree_child(oracles, HS, (((HB*2) div 8) + 4 + (3*HS)) + PS, (KL div 8)),
	     tree_child(orders, HS, ((BB div 8) + (PS * 2))),
	     tree_child(burn, HS, (BB div 8) + HS),
	     tree_child(oracle_bets, HS, (KL + (3 * BB div 8))),
	     tree_child(governance, 8, 4)
	    ],
    {ok, { {one_for_one, 50000, 1}, Tries ++ Children} }.

