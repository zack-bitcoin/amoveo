-module(testnet_hasher).
-export([doit/1, bin_to_hex/1]).

doit(X) -> hash:doit(X, constants:hash_size()).
bin_to_hex(<<>>) -> "";
bin_to_hex(<<A, B/binary>>) ->
    byte_to_hex(<<A>>) ++ bin_to_hex(B).

byte_to_hex(<< N1:4, N2:4 >>) ->
    
    [erlang:integer_to_list(N1, 16), erlang:integer_to_list(N2, 16)].
