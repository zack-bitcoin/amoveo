-module(block_tree).
-export([test/0,absorb/1,top/0,read/1,save/1]).

absorb(Block) ->
    %check that it's parent exists.
    true = block:check(Block),
    save(Block).
binary_to_file(B) ->
    C = base64:encode(B),
    H = binary_to_list(C),
    "blocks/"++H++".db".
save(Block) ->
    Z = zlib:compress(term_to_binary(Block)),
    Hash = block:hash(Block),
    BF = binary_to_file(block:hash(Block)),
    db:save(BF, Z),
    top:add(Block),
    Hash.
read(Hash) ->
    BF = binary_to_file(Hash),
    Z = db:read(BF),
    binary_to_term(zlib:uncompress(Z)).
top() ->
    ok.
test() ->
    success.
