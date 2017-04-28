-module(oracle_questions).
-export([save/1, read/1, all/0]).
-define(LOC, constants:oracle_questions()).
all() ->
    {Trees, _, _} = tx_pool:data(),
    Oracles = trees:oracles(Trees),
    Oracles2 = trie:get_all(Oracles, oracles),
    all2(Oracles2).
all2([]) -> ok;
all2([Leaf|T]) -> 
    Oracle = leaf:value(Leaf),
    Q = oracles:question(Oracle),
    [Q|all2(T)].
binary_to_file(B) ->
    C = base58:binary_to_base58(B),
    "oracle_questions/"++C++".db".
read(Hash) ->
    BF = binary_to_file(Hash),
    Z = db:read(BF),
    case Z of
	[] -> empty;
	A -> zlib:uncompress(A)
    end.
save(Binary) ->
    Z = zlib:compress(Binary),
    Binary = zlib:uncompress(Z),%sanity check, not important for long-term.
    %Hash = testnet_hasher:doit(BlockPlus),
    Hash = block:hash(Binary),
    BF = binary_to_file(Hash),
    db:save(BF, Z).
    
