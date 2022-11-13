-module(trees2).
-export([test/1, decompress_pub/1]).

-include("../../records.hrl").

range(N, N) ->
    [N];
range(N, M) when N < M ->
    [N|range(N+1, M)].

type2int(acc) -> 1.

cs2v([]) -> [];
cs2v([Acc|T]) when is_record(Acc, acc) ->
    %converts consensus state into the verkle data.
    %consensus state is like accounts and contracts and whatever.
    %verkle data is a list of leaves that can be fed into store_verkle:batch/3.
    CFG = tree:cfg(amoveo),
    K = key(Acc),
    V = serialize(Acc),
    H = hash:doit(V),

    M = dump:put(V, accounts_dump),
    M1 = type2int(acc),
    Meta = <<M1, M:(7*8)>>, %type 1 is for accounts.

    Leaf = leaf_verkle:new(K, H, Meta, CFG),
    [Leaf|cs2v(T)].
    
                       
    
                           
    

update_proof(L, ProofTree) ->
    %L is a list of accounts and contracts and whatever.
    CFG = tree:cfg(amoveo),
    Leaves = cs2v(L),
    verify_verkle:update(
      ProofTree, Leaves, CFG).

%recurse over the tree, and do cs2v on each leaf we find, to convert to the format we will write in the verkle tree.
%todo
cs2v_star([]) -> [];
cs2v_star([H|T]) -> 
    [cs2v_star(H)|cs2v_star(T)];
cs2v_star({N, {Key, Value}}) 
  when is_integer(N) and 
       is_binary(Key) and 
       is_binary(Value) -> 
    io:fwrite({size(Key), size(Value), Key, Value}),
    ok;
cs2v_star(X = {N, {mstem, A, B}}) -> X;
cs2v_star(B) when is_binary(B) -> B.

store_verified(Loc, ProofTree) ->
    CFG = tree:cfg(amoveo),
    %ProofTree2 = cs2v_star(ProofTree),
    store_verkle:verified(
      Loc, ProofTree, CFG).

store_things(Things, Loc) ->
    %return the pointer to the new version of the verkle tree.
    CFG = tree:cfg(amoveo),
    V = cs2v(Things),
    store_verkle:batch(V, Loc, CFG).

key(#acc{pubkey = Pub}) ->
    %hash of the pubkey.
    PubkeySize = constants:pubkey_size(),
    PubkeySize = size(Pub),
    hash:doit(Pub).
compress_pub(<<4, X:256, Y:256>>) ->
    Positive = Y rem 2,
    <<(6 + Positive), X:256>>.
det_pow(A, 1) -> A;
det_pow(A, B) when B rem 2 == 0 -> 
    %io:fwrite("det pow even\n"),
    det_pow(A*A, B div 2);
det_pow(A, B) -> 
    A * det_pow(A, B-1).
det_pow_mod(A, 1, _) -> A;
det_pow_mod(A, B, P) when B rem 2 == 0-> 
    det_pow_mod(A*A rem P, B div 2, P);
det_pow_mod(A, B, P) -> 
    (A*det_pow_mod(A, B-1, P)) rem P.
decompress_pub(<<A, X:256>>) ->
    %y^2 = x^3 + 7
    %P = 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1
    Positive = A - 6,
    P = det_pow(2, 256) - 
        det_pow(2, 32) -
        det_pow(2, 9) -
        det_pow(2, 8) -
        det_pow(2, 7) -
        det_pow(2, 6) -
        det_pow(2, 4) -
        1,
    X3 = ((X * X) rem P) * X rem P,
    Y2 = (X3 + 7) rem P,
    Y = det_pow_mod(Y2, (P + 1) div 4, P),
    if
        (Positive 
         bxor (Y rem 2)) == 0 -> 
            <<4, X:256, Y:256>>;
         true -> 
            NY = P -Y,
            <<4, X:256, NY:256>>
    end.
    
    
serialize(
  #acc{pubkey = Pub, nonce = Nonce, 
       balance = Balance}) ->
    %33 + 8 + 3 = 44 bytes.
    Pub2 = case size(Pub) of
               65 -> compress_pub(Pub);
               33 -> Pub
           end,
    <<Pub2/binary, Balance:64, Nonce:24>>.
    % balance was 6 bytes. nonce was 3 bytes.
    % lets up balance to 8 bytes.
account_deserialize(
  <<Pub:(33*8), Balance:64, Nonce:24>>) ->
    Pub2 = decompress_pub(<<Pub:(33*8)>>),
    #acc{pubkey = Pub2,
         nonce = Nonce, balance = Balance}.

to_keys([]) -> [];
to_keys([Acc|T]) ->
    [key(Acc)|to_keys(T)].

%to_values([]) -> [];
%to_values([Acc|T]) when is_record(Acc, acc) -> 
%    [account_serialized(Acc)|to_values(T)].

get_proof(Keys, Loc) ->
    get_proof(Keys, Loc, small).
get_proof(Keys, Loc, Type) ->
    CFG = tree:cfg(amoveo),
    case Type of
        fast -> ok;
        small -> ok
    end,
    get_verkle:batch(Keys, Loc, CFG, Type).

verify_proof(Proof, Things) ->
    CFG = tree:cfg(amoveo),
    {true, Leaves, ProofTree} = 
        verify_verkle:proof(Proof, CFG),
    Ks = to_keys(Things),
    Hs = lists:map(
           fun(A) -> 
                   hash:doit(
                     serialize(A))
           end, Things),
    KHs = lists:zipwith(fun(K, H) -> {K, H} end,
                        Ks, Hs),
    {lists:sort(KHs) == lists:sort(Leaves),
     ProofTree}.

prune(Trash, Keep) ->
    CFG = tree:cfg(amoveo),
    RemovedLeaves = 
        prune_verkle:doit_stem(Trash, Keep, CFG),
    lists:map(fun(L = {leaf, Key, Value, <<Type, Loc:(7*8)>>}) ->
                      case Type of
                          1 -> %acc
                              io:fwrite("prune account\n"),
                              dump:delete(Loc, accounts_dump)
                      end
              end, RemovedLeaves),
    ok.
    

test(0) ->
    CFG = tree:cfg(amoveo),
    Loc = 1,
    Many = 4,
    Pairs = 
        lists:map(
          fun(N) ->
                  Key = crypto:strong_rand_bytes(32),
                  Val = crypto:strong_rand_bytes(32),
                  Meta = crypto:strong_rand_bytes(8),
                  {Key, Val, Meta}
          end, range(1, 4)),
    Leaves = lists:map(
               fun({Key, Val, Meta}) ->
                       leaf_verkle:new(
                         Key, Val, Meta, CFG)
               end, Pairs), 
    AddKey = <<1:256>>,
    Keys0 = lists:map(
             fun({Key, _, _}) ->
                     Key
             end, Pairs),
    Keys = [AddKey| Keys0],
    
    {Loc2, stem, _} = store_verkle:batch(
                        Leaves, Loc, CFG),

    %normal proof has ~500 bytes overhead. Take ~1 second longer to make that fast proofs.
    {Proof, _} = get_verkle:batch(Keys, Loc2, CFG),
    %fast proof has ~8000 bytes overhead, but can be made faster.
    {FastProof, _} = 
        get_verkle:batch(Keys, Loc2, CFG, fast),

    %verifying proofs
    Root = stem_verkle:root(
             stem_verkle:get(Loc2, CFG)),
    {ProofTree1, _Commit1, _Opening1} = Proof,
    {ProofTree2, _, _} = FastProof,
    Root01 = stem_verkle:hash_point(hd(ed:decompress_points([hd(ProofTree1)]))),
    Root01 = stem_verkle:hash_point(hd(ed:decompress_points([hd(ProofTree2)]))),
    Root01 = stem_verkle:hash_point(Root),
    {true, Leaves2, ProofTree3} = 
        verify_verkle:proof(Proof, CFG),
    {true, Leaves2, ProofTree3} = 
        verify_verkle:proof(FastProof, CFG),

    %updating a proof
    AddLeaf = leaf_verkle:new(
                AddKey, <<27, 0:248>>, <<1:64>>, 
                CFG),
    UpdateLeaf = leaf_verkle:new(
                   element(1, hd(Pairs)),
                   <<28, 0:248>>, <<2:64>>, CFG),
    DeleteLeaf = {element(1, hd(tl(Pairs))),
                  0},
    ProofTree4 = 
        verify_verkle:update(
          ProofTree3, 
          [AddLeaf, UpdateLeaf, DeleteLeaf], 
          CFG),
    %new root of new tree:
    Root2 = stem_verkle:hash_point(hd(ProofTree4)),
    
    %efficiently update the hard drive with the new version. Faster that writing the leaves in a batch, because pedersen commitments are already computed.
    Loc3 = store_verkle:verified(
             Loc2, ProofTree4, CFG),

    Pruned = prune_verkle:doit_stem(Loc2, Loc3, CFG),
    %io:fwrite(Pruned),
    %{leaf, Key, Value, Meta}) ->

    success;
test(1) ->
    Range = 10,
    Keys = lists:map(fun(_) -> signing:new_key()
                     end, range(1, Range)),
    As = lists:map(
           fun({P, _}) ->
                   #acc{pubkey = P, 
                        balance = 100000000, 
                        nonce = 0} 
           end, Keys),
    {As0, _} = lists:split(Range div 2, As),

    As2 = lists:map(fun(A) ->
                            A#acc{balance = 27}
                    end, As0),
    
    Loc = 1,
    {Loc2, stem, _} = store_things(As, Loc),
    
    {Proof, _} = get_proof(to_keys(As2), Loc2),
   
    {true, ProofTree} = verify_proof(Proof, As0),
    
    ProofTree2 = update_proof(As2, ProofTree),
    
    Loc3 = store_verified(Loc2, ProofTree2),

    {Proof3, _} = get_proof(to_keys(As2), Loc3),
    
    {true, _} = verify_proof(Proof3, As2),

    prune(Loc2, Loc3),

    %io:fwrite({hd(Stuff), hd(Stuff2)}),
    %io:fwrite(As2),

    success;
test(2) ->
    {Pub, _} =  signing:new_key(),
    Cpub = compress_pub(Pub),
    Pub2 = decompress_pub(Cpub),
    <<_, _:256, Y1:256>> = Pub,
    <<_, _:256, Y2:256>> = Pub2,
    %io:fwrite({Y1, Y2}),
    Y1 = Y2,
    success.

