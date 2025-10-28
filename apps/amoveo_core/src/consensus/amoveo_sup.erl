-module(amoveo_sup).
-behaviour(supervisor).
-export([start_link/0,init/1, stop/0,
         trees/0, trie_data/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, infinity, Type, [I]}).
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%-define(trees, [accounts, channels, existence, oracles, orders, governance, matched, unmatched]).
-define(keys, [sync_kill, sync_mode, keys, recent_blocks, block_hashes, 
               block_db3,
               headers, tx_pool, 
	       peers, blacklist_peer, tx_pool_feeder, 
	       mine, %channel_manager, channel_feeder,
	       request_frequency, sync, secrets,
	       %arbitrage, order_book, 
               oracle_questions, 
	       tree_data, potential_block, push_block,
	       found_block_timer, vanity, peers_heights,
               white_list, nc_sigs, 
               mining_pool_refresher, 
               %checkpoint, 
               verkle_trees_sup, tx_reserve
              ]).
child_killer([]) -> [];
child_killer([H|T]) -> 
    supervisor:terminate_child(amoveo_sup, H),
    child_killer(T).
tree_killer([]) -> [];
tree_killer([H|T]) ->
    trie_sup:stop(H),
    supervisor:terminate_child(amoveo_sup, H),
    tree_killer(T).
trees() ->
    %oracle_bets ?
    [accounts, channels, existence, oracles, orders, governance, matched, unmatched, sub_accounts, contracts, trades, markets, stablecoins, receipts].
    
stop() -> 
    sync:stop(),
    timer:sleep(1000),
    child_killer(?keys ++ [checkpoint]),
    tree_killer(trees()),
    ok = application:stop(amoveo_core),
    ok = application:stop(amoveo_http),
    %ok = application:stop(verkle),
    ok.
    
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
    
    Children = child_maker(?keys) ++ [{checkpoint, {checkpoint, start_link, []}, permanent, 60000, worker, [checkpoint]}],
    Tries = lists:map(fun({Id, KeySize, Size, Meta, Mode, TS}) ->
                              tree_child(Id, KeySize, Size, Meta, Mode, TS)
                     end, trie_data()),
    {ok, { {one_for_one, 50000, 1}, Tries ++ Children %++ 
               %[{verkle_trees_sup, {verkle_trees_sup, start_link. []}, permanent, 5000, supervistor [verkle_trees_sup]}}
} }.

trie_data() ->
    %this seems unused? now we use verkle_trees_sup.
    KL = constants:key_length(), 
    HS = constants:hash_size(),
    PS = constants:pubkey_size(),
    BB = constants:balance_bits(),
    HB = constants:height_bits(),
    {ok, TrieSize} = application:get_env(amoveo_core, trie_size),
    {ok, Mode} = application:get_env(amoveo_core, trie_mode),
    [
     {accounts, HS, constants:account_size(), KL div 8, Mode, TrieSize},
     {channels, HS, constants:channel_size(), 0, Mode, TrieSize},
     {existence, HS, HS + (HB div 8), 0, Mode, 10000},
     {oracles, HS, (((HB*2) div 8) + 4 + (3*HS)) + PS, (KL div 8), Mode, TrieSize},
     {orders, HS, ((BB div 8) + (PS * 2)), 0, Mode, TrieSize},
     {oracle_bets, HS, (HS + (3 * BB div 8)), 0, Mode, TrieSize},
     {governance, 8, 4, 0, Mode, TrieSize},
     {matched, HS, (HS + PS + (3 * BB div 8)), 0, Mode, TrieSize},
     {unmatched, HS, (HS + PS + PS + (BB div 8)), 0, Mode, TrieSize},
     {sub_accounts, HS, constants:sub_account_size(), 0, Mode, TrieSize},
     {contracts, HS, constants:contract_size(), 0, Mode, TrieSize},
     {trades, HS, constants:trade_size(), 0, Mode, TrieSize},
     {markets, HS, constants:market_size(), 0, Mode, TrieSize},
     {stablecoins, HS, 202, 0, Mode, TrieSize},
     {receipts, HS, constants:receipt_size(), 0, Mode, TrieSize}
    ].
    


