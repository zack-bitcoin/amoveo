-module(secrets).
-export([new/0]).
new() ->
    Secret = crypto:strong_rand_bytes(12),
    Commit = testnet_hasher:doit(Secret),
    {Commit, Secret}.
