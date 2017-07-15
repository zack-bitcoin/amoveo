-module(testnet_sup).
-behaviour(supervisor).
-export([start_link/0,init/1,stop/0]).%,start_http/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, infinity, Type, [I]}).
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
-define(keys, [keys, headers, %top,
	       block_hashes, %block_absorber,
	       %tx_pool, 
	       peers, tx_pool_feeder, 
	       mine, channel_manager, channel_feeder,
	       request_frequency, sync, secrets,
	       arbitrage, order_book]).

child_maker([]) -> [];
child_maker([H|T]) -> [?CHILD(H, worker)|child_maker(T)].
child_killer([]) -> [];
child_killer([H|T]) -> 
    supervisor:terminate_child(testnet_sup, H),
    child_killer(T).
stop() -> 
    child_killer(?keys),
    halt().
%exit(keys, kill).
%supervisor:terminate_child(testnet_sup, keys).
tree_child(Id, KeySize, Size) ->
    tree_child(Id, KeySize, Size, 0).
tree_child(Id, KeySize, Size, Meta) ->
    {ok, Amount} = application:get_env(ae_core, trie_size),
    Sup = list_to_atom(atom_to_list(Id) ++ "_sup"),
    {Sup, {trie_sup, start_link, [KeySize, Size, Id, Amount, Meta, constants:hash_size(), hd]}, permanent, 5000, supervisor, [trie_sup]}.
init([]) ->
    os:putenv("ERL_CRASH_DUMP_SECONDS", <<0>>),
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
	     tree_child(accounts, HS*8, constants:account_size(), KL*2 div 8),
	     tree_child(channels, HS*8, constants:channel_size(), KL div 8),
	     tree_child(existence, HS*8, HS),
	     tree_child(oracles, HS*8, ((((HB*2)+DB) div 8) + 4 + (3*HS)) + PS, (KL div 8)),
	     tree_child(orders, KL, (((constants:orders_bits()*2) + BB) div 8) + PS),
	     tree_child(burn, HS*8, (BB div 8) + HS),
	     tree_child(oracle_bets, KL, (KL + (3 * BB div 8))),
	     tree_child(shares, KL, (KL + 1 + ((BB + HB) div 8))),
	     tree_child(governance, 8, 4)
	    ],
    io:fwrite("testnet sup 00\n"),
    io:fwrite("testnet sup 01\n"),
    io:fwrite("testnet sup 02\n"),
    io:fwrite("testnet sup 03\n"),
    io:fwrite("testnet sup 04\n"),
    io:fwrite("testnet sup 05\n"),
    {ok, { {one_for_one, 50000, 1}, Tries ++ Children} }.

