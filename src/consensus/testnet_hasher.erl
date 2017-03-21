-module(testnet_hasher).
-export([doit/1]).

doit(X) -> hash:doit(X, constants:hash_size()).
