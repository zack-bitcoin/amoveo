-module(trees2).
-export([test/1]).

range(N, N) ->
    [N];
range(N, M) when N < M ->
    [N|range(N+1, M)].

type2int(account) ->
    <<1>>.


make_leaf(Type, Data) when Type == account ->
    Meta0 = type2int(Type),
    Meta = <<Meta0/binary, 0:56>>,
    CFG = tree:cfg(amoveo),
    %todo. get key and value from data. value is the hash of data.
    Key = ok,
    Value = ok,
    leaf_verkle:new(Key, Value, Meta, CFG).

update_proof(L, ProofTree) ->
    CFG = tree:cfg(amoveo),
    Leaves = lists:map(
               fun({Type, Data}) ->
                       make_leaf(Type, Data)
               end, L),
    verify_verkle:update(
      ProofTree, Leaves, CFG).

store(Loc, Data, ProofTree) ->
    CFG = tree:cfg(amoveo),
    %for each leaf in the proof tree, we need to store that data in the dump, in order to know the meta for the leaf.

    %todo, store data so that depth first search is in order. (order keys from lowest to highest).

    %do a depth first scan of the prooftree, storing data in the dump and inserting the correct meta for the leaf.

    %Location = dump:put(Data, DumpID). (dump id is like, accounts vs contracts.)
    Loc2 = ok,
    ProofTree2 = ok,
    store_verkle:verified(
      Loc2, ProofTree2, CFG).
    

test(0) ->
    CFG = tree:cfg(amoveo),
    Loc = 1,
    Many = 6,
    Pairs = 
        lists:map(
          fun(N) ->
                  Key = crypto:strong_rand_bytes(32),
                  Val = crypto:strong_rand_bytes(32),
                  %Meta = crypto:strong_rand_bytes(8),
                  Meta = <<N:64>>,
                  {Key, Val, Meta}
          end, range(1, Many)),
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
    Keys2 = [AddKey, hd(Keys0), hd(tl(Keys0))],
    
    {Loc2, stem, _} = store_verkle:batch(
                        Leaves, Loc, CFG),

    %normal proof has ~500 bytes overhead. Take ~1 second longer to make that fast proofs.
    {Proof, _} = get_verkle:batch(Keys2, Loc2, CFG),
    %fast proof has ~8000 bytes overhead, but can be made faster.
    {FastProof, _} = 
        get_verkle:batch(Keys2, Loc2, CFG, fast),

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

    %pruning an old version of the tree, and getting meta data from the deleted leaves.

    Deleted = 
        prune_verkle:doit_stem(Loc2, Loc3, CFG),
    2 = length(Deleted),
    


    success.
