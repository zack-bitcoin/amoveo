-module(testnet_sup).
-behaviour(supervisor).
-export([start_link/0,init/1,stop/0]).%,start_http/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, infinity, Type, [I]}).
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
-define(keys, [port, keys, 
	       block_hashes, top, block_absorber,
	       tx_pool, peers, tx_pool_feeder, 
	       mine, channel_manager, channel_feeder]).

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
init([]) ->
    Amount = constants:trie_size(),
    KeyLength = constants:key_length(), 
    Children = child_maker(?keys),
    HS = constants:hash_size(),
    Tries = [
		{accounts_sup, {trie_sup, start_link, [KeyLength, constants:account_size(), accounts, Amount, 0, HS, hd]}, permanent, 5000, supervisor, [trie_sup]},
		{channels_sup, {trie_sup, start_link, [KeyLength, constants:channel_size(), channels, Amount, 0, HS, hd]}, permanent, 5000, supervisor, [trie_sup]} 
	    ],
    {ok, { {one_for_one, 50000, 1}, Tries ++ Children} }.

