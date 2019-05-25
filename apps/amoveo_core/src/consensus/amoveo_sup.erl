-module(amoveo_sup).
-behaviour(supervisor).
-export([start_link/0,init/1, stop/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, infinity, Type, [I]}).
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
-define(trees, [accounts, channels, existence, oracles, orders, governance, matched, unmatched
]).
-define(keys, [sync_kill, sync_mode, keys, recent_blocks, block_hashes, 
               block_db,
               headers, block_absorber, block_organizer, tx_pool, 
	       peers, blacklist_peer, tx_pool_feeder, 
	       mine, channel_manager, channel_feeder,
	       request_frequency, sync, secrets,
	       arbitrage, order_book, oracle_questions, 
	       tree_data, potential_block, my_ip, push_block,
	       found_block_timer, vanity,
	       %block_reader, 
               white_list, nc_sigs]).
child_killer([]) -> [];
child_killer([H|T]) -> 
    supervisor:terminate_child(amoveo_sup, H),
    child_killer(T).
tree_killer([]) -> [];
tree_killer([H|T]) ->
    trie_sup:stop(H),
    supervisor:terminate_child(amoveo_sup, H),
    tree_killer(T).
stop() -> 
    sync:stop(),
    timer:sleep(1000),
    child_killer(?keys),
    tree_killer(?trees),
    ok = application:stop(amoveo_core),
    ok = application:stop(amoveo_http).
    
child_maker([]) -> [];
child_maker([H|T]) -> [?CHILD(H, worker)|child_maker(T)].
%tree_child(Id, KeySize, Size) ->
%    tree_child(Id, KeySize, Size, 0, hd, 1000000).
tree_child(Id, KeySize, Size, Meta, Mode, TrieSize) ->
    true = ((Mode == ram) or (Mode == hd)),
    Location = constants:custom_root(),
    Sup = list_to_atom(atom_to_list(Id) ++ "_sup"),
    {Sup, {trie_sup, start_link, [KeySize, Size, Id, TrieSize, Meta, constants:hash_size(), Mode, Location]}, permanent, 5000, supervisor, [trie_sup]}.
init([]) ->
    KL = constants:key_length(), 
    HS = constants:hash_size(),
    PS = constants:pubkey_size(),
    %FullLength = KL*2,
    BB = constants:balance_bits(),
    Children = child_maker(?keys),
    HB = constants:height_bits(),
    %DB = constants:difficulty_bits(),
    {ok, TrieSize} = application:get_env(amoveo_core, trie_size),
    %Mode = hd,
    {ok, Mode} = application:get_env(amoveo_core, trie_mode),
    Tries = [
	     tree_child(accounts, HS, constants:account_size(), KL div 8, Mode, TrieSize),
	     tree_child(channels, HS, constants:channel_size(), 0, Mode, TrieSize),
	     tree_child(existence, HS, HS + (HB div 8), 0, Mode, 10000),
	     tree_child(oracles, HS, (((HB*2) div 8) + 4 + (3*HS)) + PS, (KL div 8), Mode, TrieSize),
	     tree_child(orders, HS, ((BB div 8) + (PS * 2)), 0, Mode, TrieSize),
	     tree_child(oracle_bets, HS, (HS + (3 * BB div 8)), 0, Mode, TrieSize),
	     tree_child(governance, 8, 4, 0, Mode, TrieSize),
	     tree_child(matched, HS, (HS + PS + (3 * BB div 8)), 0, Mode, TrieSize),
	     tree_child(unmatched, HS, (HS + PS + PS + (BB div 8)), 0, Mode, TrieSize)
	    ],
    {ok, { {one_for_one, 50000, 1}, Tries ++ Children} }.


