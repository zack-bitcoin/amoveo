-module(trees2).
-export([test/1, decompress_pub/1]).

-include("../../records.hrl").
-record(exist, {hash, height}).
-record(unmatched, {account, %pubkey of the account
		    oracle, %oracle id
		    amount,
		    pointer}).
-record(receipt, {id, tid, pubkey, nonce}).

range(N, N) ->
    [N];
range(N, M) when N < M ->
    [N|range(N+1, M)].

%places we need to support every type
% type2int
% cs2v
% key
% delete_thing

%kinds of trees to support
% {acc, balance, nonce, pubkey}
% {exist, hash, height}
% {oracle, id, result, question, starts, type, orders, creator, done_timer}
% {matched, amount, oracle, true, false, bad}
% {unmatched, account, oracle, amount, pointer}
% {sub_acc, balance, nonce, pubkey, contract_id, type}
% {contract, code, many_types, nonce, last_modified, delay, closed, result, source, source_type, sink, volume}
% {trade, height, value}
% {market, id, cid1, type1, amount1, cid2, type2, amount2, shares}
% {receipt, id, tid, pubkey, nonce}

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
store_verified(Loc, ProofTree) ->
    CFG = tree:cfg(amoveo),
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
    case size(Pub) of
        PubkeySize ->
            hash:doit(compress_pub(Pub));
        33 -> hash:doit(Pub)
    end.
compress_pub(<<4, X:256, Y:256>>) ->
    Positive = Y rem 2,
    <<(6 + Positive), X:256>>;
compress_pub(<<4, X:256>>) ->
    <<4, X:256>>.

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
    %33 + 8 + 4 = 45 bytes.
    Pub2 = compress_pub(Pub),
    <<Pub2/binary, Balance:64, Nonce:32>>;
serialize(#exist{hash = A, height = E}) ->
    <<E:32, A/binary>>;%8 + 32 = 40
serialize(
  #oracle{id = ID, result = Result, question = Q,
          starts = S, type = T, creator = C, 
          done_timer = D
         }) ->
    32 = size(ID),
    32 = size(Q),
    C2 = compress_pub(C),
    <<ID/binary, Result, T, %32 + 1 + 1
      S:256, D:256, C2/binary,  %32 + 32 + 33
      Q/binary>>; %32
%128 + 35 = 163
serialize(
 #matched{account = A, oracle = O, true = T, 
          false = F, bad = B}) ->
    A2 = compress_pub(A),
    32 = size(O),
    <<A2/binary, O/binary, T:64, F:64, B:64>>;
%33 + 32 + 8 + 8 + 8 = 56+33 = 89
serialize(
 #unmatched{account = A, oracle = O, amount = A, 
            pointer = P}) ->
    A2 = compress_pub(A),
    32 = size(O),
    32 = size(P),
    <<A2/binary, O/binary, A:64, P/binary>>; 
%33 + 32 +8 + 32 = 41 + 64 = 105
serialize(
 #sub_acc{balance = B, nonce = N, pubkey = P, 
          contract_id = CID, type = T}) ->
    P2 = compress_pub(P),
    32 = size(CID),
    <<B:64, N:32, T:32, P2/binary, CID/binary>>;
%8 + 4 + 4 + 33 + 32 = 65 + 16 = 81
serialize(
  #contract{code = C, many_types = MT, nonce = Nonce, 
            last_modified = LM, delay = D, closed = C,
            result = R, source = S, source_type = ST,
            sink = Sink, volume = V}) ->
    32 = size(C),
    32 = size(R),
    32 = size(S),
    32 = size(Sink),
    <<C/binary, R/binary, S/binary, Sink/binary,
    ST:16, MT:16, Nonce:32, LM:32, D:32, C, V:64>>;
%32*4 + 2 + 2 + 4 + 4 + 4 + 1 + 8
%128 + 16 + 9
%128 + 25 = 153
serialize(
  #trade{height = H, value = V}) ->
    32 = size(V),
    <<V/binary, H:32>>; %32 + 4 = 36
serialize(
  #market{id = I, cid1 = C1, type1 = T1, amount1 = A1,
          cid2 = C2, type2 = T2, amount2 = A2, 
          shares = S}) ->
    32 = size(I),
    32 = size(C1),
    32 = size(C2),
    <<I/binary, C1/binary, C2/binary, T1:16, T2:16,
    A1:64, A2:64, S:64>>;
%32 + 32 + 32 + 2 + 2+ 8 + 8 + 8
%96 + 28 = 124
serialize(
  #receipt{tid = T, pubkey = P, 
            nonce = N}) ->
    32 = size(T),
    P2 = compress_pub(P),
    <<T/binary, P2/binary, N:32>>.
%32 + 33 + 8 = 73







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
    lists:map(fun(L = {leaf, _Key, _Value, Meta}) ->
                      delete_thing(Meta)
              end, RemovedLeaves),
    ok.
delete_thing(<<1, Loc:56>>) ->
    dump:delete(Loc, accounts_dump).
    

test(0) ->
    %testing the raw verkle tree interface. only stores keys and values of 32 bytes.
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
    %testing the amoveo verkle tree, which stores accounts and contracts and other things.
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
    %testing pubkey compression.
    {Pub, _} =  signing:new_key(),
    Cpub = compress_pub(Pub),
    Pub2 = decompress_pub(Cpub),
    <<_, _:256, Y1:256>> = Pub,
    <<_, _:256, Y2:256>> = Pub2,
    %io:fwrite({Y1, Y2}),
    Y1 = Y2,
    success.

