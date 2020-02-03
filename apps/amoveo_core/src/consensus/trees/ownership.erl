-module(ownership).
-export([new/5,
         max/0,
         cfg/0,
         make_root/1,

         pubkey/1,
         pstart/1,
         pend/1,
         contract/1,
         sid/1,
         
         verify/3,
         is_between/2,

         serialize/1,
         deserialize/1
        ]).

-record(x, {pubkey, %pubkey of who owns this probabilistic value space.
            pstart, %start of the probability space
            pend, %end of the probability space
            sortition_id,
            contract}).%32 byte hash of a smart contract. you only really own this value if the contract returns "true".

new(P, S, E, C, SID) ->
    #x{pubkey = P,
       pstart = S,
       pend = E,
       sortition_id = SID,
       contract = hash:doit(C)}.
pubkey(X) -> X#x.pubkey.
pstart(X) -> X#x.pstart.
pend(X) -> X#x.pend.
contract(X) -> X#x.contract.
sid(X) -> X#x.sortition_id.

max() -> 
    %<<X:256>> = <<-1:256>>, X.
    115792089237316195423570985008687907853269984665640564039457584007913129639935.

key_to_int(X) -> 
    <<Y:256>> = X,
    %<<Y:256>> = hash:doit(X),
    Y.

make_leaf(Key, V) ->
    leaf:new(key_to_int(Key), V, 0, cfg()).

is_between(X, <<RNGV:256>>) ->
    #x{pend = <<E:256>>,
       pstart = <<S:256>>} = X,
    (S =< RNGV) and
        (RNGV =< E).

verify(Ownership, Root, Proof) ->
    Key = Ownership#x.pstart,
    SO = serialize(Ownership),
    Leaf = make_leaf(Key, SO),
    verify:proof(Root, Leaf, Proof, cfg()).

cfg() ->
    S = (32*4) + 65,
    CFG = cfg:new(32, S, none, 0, 32, ram).

make_root(Owners) ->
    CFG = cfg(),
    Size = cfg:value(CFG),
    KeyLength = cfg:path(CFG),
    M = mtree:new_empty(KeyLength, Size, 0),
    L = merklize_make_leaves(Owners, CFG),
    Root0 = 1,
    {Root, M2} = mtree:store_batch(L, Root0, M),
    {mtree:root_hash(Root, M2), Root, M2}.
merklize_make_leaves([], _) -> [];
merklize_make_leaves([H|T], CFG) -> 
    N = key_to_int(H#x.pstart),
    Leaf = leaf:new(N, serialize(H), 0, CFG),
    [Leaf|merklize_make_leaves(T, CFG)].
    
    

serialize(X) ->
    PS = constants:pubkey_size(),
    HS = constants:hash_size(),
    #x{
        pubkey = P,
        pstart = S,
        pend = E,
        sortition_id = SID,
        contract = C
      } = X,
    PS = size(P),
    32 = size(S),
    32 = size(E),
    HS = size(C),
    HS = size(SID),
    <<P/binary,
      S/binary,
      E/binary,
      SID/binary,
      C/binary>>.
deserialize(B) ->
    HS = constants:hash_size()*8,
    PS = constants:pubkey_size()*8,
    X = 32*8,
    <<
      P:PS,
      S:X,
      E:X,
      SID:HS,
      C:HS
    >> = B,
    #x{
        pubkey = <<P:PS>>,
        pstart = <<S:X>>,
        pend = <<E:X>>,
        sortition_id = <<SID:HS>>,
        contract = <<C:HS>>
      }.
