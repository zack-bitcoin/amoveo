-module(ownership).
-export([new/4,
         pubkey/1,
         pstart/1,
         pend/1,
         contract/1,
         serialize/1,
         deserialize/1,
         is_between/2
        ]).

-record(x, {pubkey, %pubkey of who owns this probabilistic value space.
            pstart, %start of the probability space
            pend, %end of the probability space
            contract}).%32 byte hash of a smart contract. you only really own this value if the contract returns "true".

new(P, S, E, C) ->
    #x{pubkey = P,
       pstart = S,
       pend = E,
       contract = C}.
pubkey(X) -> X#x.pubkey.
pstart(X) -> X#x.pstart.
pend(X) -> X#x.pend.
contract(X) -> X#x.contract.

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
    S = (32*3) + 65,
    CFG = cfg:new(16, S, none, 0, 32, ram).

serialize(X) ->
    PS = constants:pubkey_size(),
    HS = constants:hash_size(),
    #x{
        pubkey = P,
        pstart = S,
        pend = E,
        contract = C
      } = X,
    PS = size(P),
    32 = size(S),
    32 = size(E),
    HS = size(C),
    <<P/binary,
      S/binary,
      E/binary,
      C/binary>>.
deserialize(B) ->
    HS = constants:hash_size()*8,
    PS = constants:pubkey_size()*8,
    X = 32*8,
    <<
      P:PS,
      S:X,
      E:X,
      C:HS
    >> = B,
    #x{
        pubkey = <<P:PS>>,
        pstart = <<S:X>>,
        pend = <<E:X>>,
        contract = <<C:HS>>
      }.
