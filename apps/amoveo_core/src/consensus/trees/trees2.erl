-module(trees2).
-export([test/1, decompress_pub/1, merkle2verkle/2, root_hash/1, get_proof/3, hash_key/2, key/1, serialize/1, store_things/2, verify_proof/2, deserialize/2, store_verified/2, update_proof/2, compress_pub/1, get/2,
         one_root_clean/2, one_root_maker/2, recover_from_clean_version/1,
         copy_bits/4, scan_verkle/2, scan_verkle/0, prune/2,
         recover/1,
         val2int/1]).

-include("../../records.hrl").
%-record(exist, {hash, height}).
-record(unmatched, {account, %pubkey of the account
		    oracle, %oracle id
		    amount,
		    pointer}).
-record(oracle_bet, {id, true, false, bad}).%true, false, and bad are the 3 types of shares that can be purchased from an oracle%
-record(receipt, {id, tid, pubkey, nonce}).

-define(sanity, false).
    

root_hash(Loc) ->
    CFG = tree:cfg(amoveo),
    stem_verkle:hash_point(
      stem_verkle:root(
        stem_verkle:get(Loc, CFG))).

range(N, N) ->
    [N];
range(N, M) when N < M ->
    [N|range(N+1, M)].

%kinds of trees
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

type2int(acc) -> 1;
type2int(oracle) -> 3;
type2int(matched) -> 4;
type2int(unmatched) -> 5;
type2int(unmatched_head) -> 5;
type2int(sub_acc) -> 6;
type2int(contract) -> 7;
type2int(trade) -> 8;
type2int(market) -> 9;
type2int(receipt) -> 10;
type2int(job) -> 11;
type2int(B) when is_binary(B) -> 5.

int2type(1) -> acc;
int2type(3) -> oracle;
int2type(4) -> matched;
int2type(5) -> unmatched;
int2type(6) -> sub_acc;
int2type(7) -> contract;
int2type(8) -> trade;
int2type(9) -> market;
int2type(10) -> receipt;
int2type(11) -> job.
    

int2dump_name(1) -> accounts_dump;
%int2dump_name(2) -> exists_dump;
int2dump_name(3) -> oracles_dump;
int2dump_name(4) -> matched_dump;
int2dump_name(5) -> unmatched_dump;
int2dump_name(6) -> sub_accs_dump;
int2dump_name(7) -> contracts_dump;
int2dump_name(8) -> trades_dump;
int2dump_name(9) -> markets_dump;
int2dump_name(10) -> receipts_dump;
int2dump_name(11) -> jobs_dump.

int2cleaner_name(1) -> accounts_cleaner;
%int2cleaner_name(2) -> exists_cleaner;
int2cleaner_name(3) -> oracles_cleaner;
int2cleaner_name(4) -> matched_cleaner;
int2cleaner_name(5) -> unmatched_cleaner;
int2cleaner_name(6) -> sub_accs_cleaner;
int2cleaner_name(7) -> contracts_cleaner;
int2cleaner_name(8) -> trades_cleaner;
int2cleaner_name(9) -> markets_cleaner;
int2cleaner_name(10) -> receipts_cleaner;
int2cleaner_name(11) -> jobs_cleaner.



cs2v([]) -> [];
cs2v([A|T]) ->
    %converts consensus state into the verkle data.
    %consensus state is like accounts and contracts and whatever.
    %verkle data is a list of leaves that can be fed into store_verkle:batch/3.
    CFG = tree:cfg(amoveo),
    %#consensus_state{val = A} = A0,
    K = key(A), %A is {<<4,164,...>>, 1}
    case K of
        bad -> cs2v(T);
        _ ->
            V = serialize(A),
            H = hash:doit(V),
            M1 = type2int(element(1, A)),
            DBName = int2dump_name(M1),
            M = dump:put(V, DBName),
            Meta = <<M1, M:(7*8)>>, 
            Leaf = leaf_verkle:new(K, H, Meta, CFG),
            [Leaf|cs2v(T)]
    end.
    

update_proof(L, ProofTree) ->
    %io:fwrite("trees2 update proof\n"),
    %io:fwrite({L, ProofTree}),
    %L is a list of accounts and contracts and whatever.
    CFG = tree:cfg(amoveo),
    Leaves = cs2v(L),

    verify_verkle:update(
      ProofTree, Leaves, CFG).

%recurse over the tree, and do cs2v on each leaf we find, to convert to the format we will write in the verkle tree.
store_verified(Loc, ProofTree) ->
    CFG = tree:cfg(amoveo),
    %io:fwrite(size(element(2, element(2, hd(hd(tl(ProofTree))))))), %32 bytes

    store_verkle:verified(
      Loc, ProofTree, CFG).

remove_repeat([L1 = {leaf, A, X, <<Type, P1:56>>}, 
               L2 = {leaf, A, X, <<_, P2:56>>}|T]) ->
    if
        not(P1 == P2) ->
            DBName = int2dump_name(Type),
            dump:delete(P1, DBName);
        true -> ok
    end,
    remove_repeat([L2|T]);
remove_repeat([L1 = {leaf, A, _, <<Type1, P1:56>>}, 
               L2 = {leaf, A, _, <<Type2, P2:56>>}|T]) ->
    io:fwrite("trees2 store things failure because we are trying to store 2 different things in the same slot."),
    DBName1 = int2dump_name(Type1),
    X = dump:get(P1, DBName1),
    DBName2 = int2dump_name(Type2),
    Y = dump:get(P2, DBName2),
    Acc = deserialize(1, X),
    WeirdAddr = base64:decode(<<"BCdW548Bb9YppmBiYDbGJM9ApGIEkum1muoYrg+saWHjOABlAFjlx/IMFKUwZXTw0+PK7X0Nw9CXGNq+9L4pb2E=">>),
    WeirdAddr2 = base64:decode(<<"BCt0x829BD824B016326A401d083B33D092293333A83IlCVaAvL29TuGDEJyZZ5BbPJNlTO08Dba5a5CD+eFxs=">>),
    ThisAddr = case element(1, Acc) of
                   acc -> element(4, Acc);
                   _ -> 0
               end,
    if
        (WeirdAddr == ThisAddr) ->
            io:fwrite("handling one old address that is double stored\n"),
            remove_repeat(T);
        (WeirdAddr2 == ThisAddr) ->
            io:fwrite("handling a second old address that is double stored\n"),
            remove_repeat(T);
        true -> 
            {Addra, Addrb} = case element(1, Acc) of
                                 acc -> 
                                     Addr = element(4, Acc),
                                     <<X1:264, Y1:256>> = Addr,
                                     {<<X1:264>>, <<Y1:256>>};
                                 _ -> {0,0}
                             end,
            io:fwrite({DBName1, deserialize(1, X), deserialize(1, Y), X == Y, base64:encode(Addra), base64:encode(Addrb)}),
            1=2
    end;

remove_repeat([H|T]) ->
    [H|remove_repeat(T)];
remove_repeat([]) -> [].

store_things(Things, Loc) ->
    %return the pointer to the new version of the verkle tree.
    CFG = tree:cfg(amoveo),
    %io:fwrite({Things}),
    false = is_record(hd(Things), consensus_state),
    V = cs2v(Things),
    V2 = lists:sort(fun({leaf, <<A:256>>, _, _}, 
                        {leaf, <<B:256>>, _, _}) -> 
                            A =< B
                    end, V),
    V3 = remove_repeat(V2),
    
    %io:fwrite("store batch\n"),
    %io:fwrite({Things, V}),
    {P, _, _} = store_verkle:batch(V3, Loc, CFG),
    %io:fwrite("stored batch\n"),
    P.
val2int({X, _}) ->
    val2int(X);
val2int(#acc{pubkey = Pub}) ->
    accounts:key_to_int(Pub);
val2int(#channel{id = ID}) ->
    channels:key_to_int(ID);
val2int(X = #contract{}) ->
    X2 = contracts:make_id(X),
    contracts:key_to_int(X2);
val2int(#exist{hash = H}) ->
    existence:key_to_int(H);
val2int(#gov{id = ID}) ->
    ID;
val2int(#market{id = ID}) ->
    markets:key_to_int(ID);
val2int(#matched{account = A, oracle = O}) ->
    matched:key_to_int({key, A, O});
val2int(#oracle_bet{id = ID}) ->
    oracle_bets:key_to_int(ID);
val2int(#oracle{id = ID}) ->
    oracles:key_to_int(ID);
val2int(X = #orders{}) ->
    Pub = orders:aid(X),
    orders:key_to_int(Pub);
val2int(#receipt{id = ID}) ->
    receipts:key_to_int(ID);
val2int(S = #sub_acc{}) ->
    Key = sub_accounts:make_key(S),
    sub_accounts:key_to_int(Key);
val2int(#trade{value = V}) ->
    trades:key_to_int(V);
val2int(#unmatched{account = A, oracle = O}) ->
    K = {key, A, O},
    unmatched:key_to_int(K);
val2int({unmatched_head, Pointer, Many, OID}) ->
    PS = constants:pubkey_size() * 8,
    K = {key, <<1:PS>>, OID},
    unmatched:key_to_int(K);
val2int(#job{id = ID}) ->
    jobs:key_to_int(ID).

    



hash_key(accounts, Pub) ->
    key(#acc{pubkey = Pub});
hash_key(oracles, X) ->
    key(#oracle{id = X});
hash_key(unmatched, {key, Account, OID}) ->
    %false = (Account == <<1:264>>),
    %false = (Account == <<1:520>>),
    key(#unmatched{
           account = Account, oracle = OID});
hash_key(matched, {key, Account, OID}) ->
    key(#matched{
           account = Account, oracle = OID});
hash_key(oracle_bets, Key = {key, _Account, OID}) ->
    %stored in a smaller merkle tree with a root in this account.
    hash:doit(OID);
hash_key(orders, Key = {key, Account, OID}) ->
    accounts:ensure_decoded_hashed(Account);
hash_key(contracts, CID) when is_binary(CID) ->
    CID;
%hash_key(contracts, {key, C, M, S, T}) ->
hash_key(contracts, {key, CID}) ->
    CID;
%key(#contract{code = C, many_types = M, source = S, source_type = T});
hash_key(sub_accounts, ID) 
  when is_binary(ID) and (size(ID) == 32) ->
    ID;
hash_key(sub_accounts, {key, Pub, CID, T}) ->
    key(#sub_acc{pubkey = Pub, type = T, contract_id = CID});
hash_key(governance, N) ->
    hash:doit(<<N, 27>>);
hash_key(trades, {key, Val}) ->
    key(#trade{value = Val});
hash_key(trades, X)
  when is_binary(X) and (size(X) == 32) ->
    key(#trade{value = X});
hash_key(receipts, ID) 
  when is_binary(ID) and (size(ID) == 32) ->
    %ID;
    key(#receipt{id = ID});
hash_key(receipts, {key, ID}) ->
    key(#receipt{id = ID});
hash_key(markets, X)
  when is_binary(X) and (size(X) == 32) ->
    key(#market{id = X});
hash_key(channels, ID) 
  when is_binary(ID) and (size(ID) == 32) ->
    ID;
hash_key(jobs, ID) 
  when is_binary(ID) and (size(ID) == 32) ->
    ID;
%hash_key(trades, X) 
%  when is_binary(X) and (size(X) == 32) ->
%    X;
%hash_key(receipts, X) 
%  when is_binary(X) and (size(X) == 32) ->
%    X;
hash_key(N, X) -> 
    io:fwrite("hash key type "),
    io:fwrite({N, X}),
    io:fwrite("\n"),
    1=2,
    X.

key({Tree, Key}) when is_atom(Tree) -> 
    hash_key(Tree, Key);

key({Head, Many}) when is_binary(Head) and is_integer(Many) ->
    %unmatched head
    %io:fwrite({Tree, K}),%{<<4,164,...>>, 1}
    %key = {key, <<1:PS>>, OID}
    Key = {key, <<1:520>>, Head}, 
    hash_key(unmatched, Key);
    %hash_key(Tree, K);
    
key(#acc{pubkey = Pub}) ->
    %hash of the pubkey.
    ZeroPub = base64:decode("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEFAx4lA9qJP3/x4hz1EkNIQAAAAAAAAA="),
    ZeroPub2 = base64:decode("v8X+pcYXf+kLyaTW0F6W228vqIQPKrXegIJU0SoVn4Wr5EIYUOeHGwo7AhNuCjfIz7+fUpgqNSTPQsmCfxpieeQ="),
    ZeroPub3 = base64:decode("DtLQX8Wlh3VHkTBPd77/SS8vZTxtvDCRBumkUnYVSVOoVgVtd//+WRBh3CrpLEYoHTtNpMyuiPDzUhxwcYHLdCM="),
    if
        (Pub == ZeroPub) -> bad;
        (Pub == ZeroPub2) -> bad;
        (Pub == ZeroPub3) -> bad;
        true ->
    
            Pub2 = compress_pub(Pub),
            hash:doit(Pub2)
    end;
%key(#exist{hash = X}) ->
%    hash:doit(X);
key(#oracle{id = X}) ->
    hash:doit(<<X/binary, 0>>);
key(#matched{account = A, oracle = B}) ->
    A2 = compress_pub(A),
    hash:doit(<<A2/binary, B/binary, 1>>);%33 + 32 + 1 = 66 bytes
%key(#unmatched{account = <<1:520>>, oracle = B}) ->
    %it is the header of the linked list.
    %it remembers a pointer to the start of the list, and it knows how long the list is.
%    hash:doit(<<B/binary, 2>>);
key({unmatched_head, Head, Many, OID}) ->
    %A2 = compress_pub(Head),
    %<<X:264>> = A2,
    %io:fwrite(integer_to_list(X)),
    %io:fwrite("\n"),
    %A2 = <<1:264>>,
    hash:doit(<<OID/binary, 2>>);%33 bytes
%key({unmatched_head, Head, Many, OID}) -> 
%    hash:doit(<<OID/binary, 9>>);
key(K = #unmatched{oracle = accounts}) ->
    io:fwrite(K),
    1=2;
key(K = #unmatched{account = <<1:520>>, 
                   oracle = B}) ->
    if
        not(is_binary(B)) -> 
            io:fwrite(K),%{unmatched, <<1:520>>, accounts, undefined, undefined 
            % unmatched {account, oracle, amount, pointer}
            1=2;
        true -> ok
    end,
    hash:doit(<<B/binary, 2>>);%33 bytes
key(K = #unmatched{account = A, oracle = B}) ->
    A2 = compress_pub(A),%error here when we are storing the head. see unmatched:serialize_head
    %false = (A2 == <<0:264>>),
    %false = (A2 == <<1:264>>),
    hash:doit(<<A2/binary, B/binary, 3>>);%66 bytes
key(K = {oracle_bets, T, F, B, ID}) ->
    hash:doit(ID);
key(K = {orders, AID, Amount, Pointer}) ->
    accounts:ensure_decoded_hashed(AID);
key(#sub_acc{pubkey = P, type = T, 
             contract_id = CID}) ->
    sub_accounts:make_key(P, CID, T);%65+32+32 = 129 bytes
%    P2 = compress_pub(P),
%    hash:doit(<<P2/binary, CID/binary, T:16, 4>>);%69 bytes
key(#contract{code = C, many_types = MT, 
              source = S, source_type = ST}) ->
    contracts:make_id(C, MT, S, ST);%32+32+2+2 = 68 bytes.
    %hash:doit(<<C/binary, S/binary, MT:16, ST:16, 5>>);
key(#trade{value = V}) -> 
    hash:doit(<<V/binary, 6>>);%33 bytes
key(#market{id = X}) -> 
    hash:doit(<<X/binary, 7>>);%33 bytes
key(#receipt{id = X}) -> 
    hash:doit(<<X/binary, 8>>);%33 bytes
key(#job{id = X}) -> 
    hash:doit(<<X/binary, 9>>).%33 bytes

compress_pub(<<1:264>>) ->
    <<1:264>>;
compress_pub(<<1:520>>) ->
    <<1:264>>;
compress_pub(<<0:520>>) ->
    <<0:264>>;
compress_pub(<<0:264>>) ->
    <<0:264>>;
compress_pub(<<4, X:256, Y:256>>) ->
    Positive = Y rem 2,
    <<(6 + Positive), X:256>>;
compress_pub(<<4, X:256>>) ->
    1=2,
    <<4, X:256>>;
compress_pub(X) ->
    <<E:8, A:128, B:128, C:128, D:128>> = X,
    <<X1:264, X2:256>> = X,
    io:fwrite({<<E:8, A:128>>, <<B:128>>, <<C:128>>, <<D:128>>, base64:encode(X), base64:encode(<<X1:264>>), base64:encode(<<X2:256>>)}),
    %ok.
    bad_pub.


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
decompress_pub(<<0:264>>) ->
    <<0:520>>;
decompress_pub(<<1:264>>) ->
    <<1:520>>;
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
    
    
serialize({Pub = <<_:520>>, Many}) ->
%unmatched head
    Pub2 = compress_pub(Pub),
    <<Pub2/binary, Many:64, 0:520>>;
%1=2,
    %serialize({unmatched_head, Head, Many, OID});
%    ok;
serialize(
  #acc{pubkey = Pub, nonce = Nonce, 
       balance = Balance}) ->
    %33 + 8 + 4 = 45 bytes.
    Pub2 = compress_pub(Pub),
    <<Pub2/binary, Balance:64, Nonce:32>>;
%serialize(#exist{hash = A, height = E}) ->
%    32 = size(A),
%    <<E:32, A/binary>>;%4 + 32 = 36
serialize(
  #oracle{id = ID, result = Result, question = Q,
          starts = S, type = T, creator = C, 
          done_timer = D, orders_hash = OH
         }) ->
    % orders, orders_hash, f
    %io:fwrite("serialize oracle\n"),
    32 = size(ID),
    32 = size(Q),
    32 = size(OH),
    C2 = compress_pub(C),
    <<ID/binary, Result, T, %32 + 1 + 1
      S:32, D:32, C2/binary,  %4 + 4 + 33
      Q/binary, OH/binary>>; %64
%64 + 10 + 33 + 32 = 139
serialize(
 #matched{account = A, oracle = O, true = T, 
          false = F, bad = B}) ->
    A2 = compress_pub(A),
    32 = size(O),
    <<A2/binary, O/binary, T:64, F:64, B:64>>;
%33 + 32 + 8 + 8 + 8 = 56+33 = 89
serialize(
 #unmatched{account = A, oracle = O, amount = M, 
            pointer = P}) ->
    A2 = compress_pub(A),
    P2 = compress_pub(P),
    32 = size(O),
    false = (oracle == <<0:256>>),
    65 = size(P),
    <<A2/binary, O/binary, M:64, P2/binary>>; 
%33 + 32 +8 + 33 = 66 + 40 = 106
serialize({unmatched_head, Head, Many, OID}) ->
%   incorrect because serialization doesn't include the oid.
    Head2 = compress_pub(Head),
    <<0:264, OID/binary, Many:64, Head2/binary>>;
%33 + 8 + 32 + 33 = 106
serialize(
 #sub_acc{balance = B, nonce = N, pubkey = P, 
          contract_id = CID, type = T}) ->
    P2 = compress_pub(P),
    32 = size(CID),
    <<B:64, N:32, T:32, P2/binary, CID/binary>>;
%8 + 4 + 4 + 33 + 32 = 65 + 16 = 81
serialize(
  #contract{code = C, many_types = MT, nonce = Nonce, 
            last_modified = LM, delay = D, 
            closed = Closed, result = R, source = S, 
            source_type = ST, sink = Sink, volume = V
           }) ->
    32 = size(C),
    32 = size(R),
    32 = size(S),
    32 = size(Sink),
    <<C/binary, R/binary, S/binary, Sink/binary,
    ST:16, MT:16, Nonce:32, LM:32, D:32, Closed, 
      V:64>>;
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
    <<T/binary, P2/binary, N:32>>;
%32 + 33 + 4 = 69
serialize(#job{id = ID, worker = W0, boss = Boss0, value = V,
               salary = S, balance = Balance, time = T}) ->
    W = compress_pub(W0),
    Boss  = compress_pub(Boss0),
    32 = size(ID),
    33 = size(W),
    33 = size(Boss),
    true = is_integer(V),
    true = is_integer(S),
    true = is_integer(Balance),
    true = is_integer(T),
    <<ID/binary, W/binary, Boss/binary, V:64, S:64, Balance:64, T:32>>.
%32 + 33 + 33 + 8 + 8 + 8 + 4 = 126


deserialize(1, 
  <<Pub:(33*8), Balance:64, Nonce:32>>) ->
    Pub2 = decompress_pub(<<Pub:(33*8)>>),
    #acc{pubkey = Pub2,
         nonce = Nonce, balance = Balance};
%deserialize(2, <<E:32, A:256>>) ->
%    #exist{hash = <<A:256>>, height = E};
deserialize(3, <<ID:256, Result, T, S:32, D:32,
                 C2:264, Q:256, OH:256>>) ->
    C = decompress_pub(<<C2:264>>),
    #oracle{id = <<ID:256>>, result = Result,
            question = <<Q:256>>, starts = S,
            type = T, creator = C, done_timer = D,
            orders_hash = <<OH:256>>, orders = 1};
deserialize(4, <<A:264, O:256, T:64, F:64, B:64>>) ->
    A2 = decompress_pub(<<A:264>>),
    #matched{account = A2, oracle = <<O:256>>, 
             true = T, false = F, bad = B};
deserialize(5, <<0:264, OID:256, Many:64, Head:264>>) ->
    %this was commented to sync. make sure it isn't broken.
    A2 = decompress_pub(<<Head:264>>),
    {unmatched_head, A2, Many, <<OID:256>>};
deserialize(5, <<A:264, Many:64, 0:520>>) ->
    A2 = decompress_pub(<<A:264>>),
    {A2, Many};
deserialize(5, <<A:264, O:256, Am:64, P:264>>) ->
    A2 = decompress_pub(<<A:264>>),
    P2 = decompress_pub(<<P:264>>),
    #unmatched{account = A2, oracle = <<O:256>>,
               amount = Am, pointer = P2};
deserialize(6, <<B:64, N:32, T:32, P:264, CID:256>>) 
->
    P2 = decompress_pub(<<P:264>>),
    #sub_acc{balance = B, nonce = N, pubkey = P2,
             contract_id = <<CID:256>>, type = T};
deserialize(7, <<C:256, R:256, S:256, Sink:256,
                 ST:16, MT:16, Nonce:32, LM:32, D:32,
               Closed, V:64>>) ->
    #contract{code = <<C:256>>, result = <<R:256>>,
              source = <<S:256>>, sink = <<Sink:256>>,
              source_type = ST, many_types = MT,
              nonce = Nonce, last_modified = LM,
              delay = D, closed = Closed, volume = V};
deserialize(8, <<V:256, H:32>>) ->
    #trade{height = H, value = <<V:256>>};
deserialize(9, <<I:256, C1:256, C2:256, T1:16, T2:16,
                 A1:64, A2:64, S:64>>) ->
    #market{id = <<I:256>>, cid1 = <<C1:256>>,
            cid2 = <<C2:256>>, type1 = T1, type2 = T2,
            amount1 = A1, amount2 = A2, shares = S};
deserialize(10, <<T:256, P:264, N:32>>) ->
    P2 = decompress_pub(<<P:264>>),
    R = #receipt{tid = <<T:256>>, pubkey = P2, nonce = N},
    ID = receipts:id_maker(R),
    R#receipt{id = ID};
%    #receipt{id = ID, tid = <<T:256>>, pubkey = P2, nonce = N};
%deserialize(_, T) when is_tuple(T) -> T;
deserialize(11, <<ID:256, W:264, Boss:264, V:64, S:64, Balance:64, T:32>>) ->
    #job{id = <<ID:256>>, worker = decompress_pub(<<W:264>>),
         boss = decompress_pub(<<Boss:264>>),
         value = V, salary = S, balance = Balance, time = T};
deserialize(N, B) ->
    io:fwrite({N, B, size(B)}),
    1=2,
    ok.

    
%to_keys(L) -> lists:map(fun(X) -> key(X) end, L).
to_keys([]) -> [];
to_keys([Acc|T]) ->
    [key(Acc)|to_keys(T)].

strip_tree_info([], R, D) -> {lists:reverse(R), D};
strip_tree_info([{Tree, X}|T], R, D) -> 
    %io:fwrite("strip tree info 2\n"),
    K = hash_key(Tree, X),
    %D2 = dict:store(K, Tree, D),
    D2 = dict:store(K, {Tree, X}, D),
    strip_tree_info(T, [K|R], D2);
strip_tree_info([H|T], R, D) -> 
    io:fwrite("strip tree info 3\n"),
    1=2,
    strip_tree_info(T, [H|R], D).


ordered_remove_repeats([]) -> [];
ordered_remove_repeats([X]) -> [X];
ordered_remove_repeats([A,A|T]) -> 
    ordered_remove_repeats([A|T]);
ordered_remove_repeats([A|T]) -> 
    [A|ordered_remove_repeats(T)].

remove_repeats([]) ->
    [];
remove_repeats([H|T]) ->
    B = is_in(H, T),
    if
        B -> remove_repeats(T);
        true -> [H|remove_repeats(T)]
    end.
is_in(X, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) -> 
    is_in(X, T).
   
get(Keys, Loc) when is_integer(Loc) -> 
    CFG = tree:cfg(amoveo),
    {Keys3, TreesDict} = 
        strip_tree_info(Keys, [], dict:new()),%this is where we lose the tree info. it also hashes the keys.
    Keys4 = depth_order(Keys3),
    Keys5 = ordered_remove_repeats(Keys4),
    L = get_verkle:unverified(
          Keys5, Loc, CFG), 
    if
        Keys5 == [<<0:256>>] -> 
            io:fwrite({Keys}),
            ok;
        true -> ok
    end,
    lists:map(fun({Key, Leaf}) ->
                      case Leaf of
                          0 -> 
                              UnhashedKey = 
                                  dict:fetch(Key, TreesDict),
                              {UnhashedKey, empty};
                          _ ->
                              {leaf, Key2, _, <<T,DL:56>>} = Leaf,
                              if
                                  not(Key == Key2) ->
                          %it is weird that Key isn't the same as Key2.
                                      Acc = dump_get(T, DL),
                                      io:fwrite({Key, Key2, Acc, dict:fetch(Key, TreesDict), Keys
                                                 %dict:fetch(Key2, TreesDict)
                                                }),
                                      1=2;
                                  true -> ok
                              end,
                              UnhashedKey = 
                                  dict:fetch(Key, TreesDict),
                              {UnhashedKey, dump_get(T, DL)}
                      end
              end, L).
            
    
    
get_proof(Keys, Loc) ->
    get_proof(Keys, Loc, small).
get_proof(Keys0, Loc, Type) ->
    {Keys3, TreesDict} = 
        strip_tree_info(Keys0, [], dict:new()),%this is where we lose the tree info. it also hashes the keys.
    Keys = remove_repeats(Keys3),%this is a N^2 algorithm, it might end up being the bottleneck eventually.
    CFG = tree:cfg(amoveo),
    case Type of
        fast -> ok;
        small -> ok
    end,
    %io:fwrite({Keys0, Type, Loc}),
    %io:fwrite("get proof many keys: "),
    %io:fwrite(integer_to_list(length(Keys0))),
    %io:fwrite("\n"),
    {Proof, MetasDict} =
        get_verkle:batch(Keys, Loc, CFG, Type),
    %io:fwrite({Proof}),
    %order keys based on depth first scan of the tree from low to high.
    Keys30 = depth_order(Keys),
    %print_now(),
    %io:fwrite("key tree order \n"),
    if
        ?sanity ->
            Keys2 = key_tree_order(element(1, Proof)),
            KeyLengthBool = length(Keys) == length(Keys2),
            if
                KeyLengthBool -> ok;
                true ->
                    io:fwrite({length(Keys), length(Keys30),
                               Keys2, 
                               Keys30,
                               element(1, Proof), 
                               MetasDict}),
                    ok
            end;
        true -> ok
    end,
    Leaves = 
        lists:map(fun(K) ->
                          case dict:find(K, MetasDict) of
                              {ok, <<T, V:56>>} ->
                                  dump_get(T, V);
                              error ->
                                  {EmptyTree, UK} = 
                                      dict:fetch(K, TreesDict),
                                  %UK is the unhashed version of the key.
                                  {EmptyTree, UK}
                          end
                  end, Keys30),

    Proof2 = remove_leaves_proof(Proof),

    if
        not(?sanity) -> ok;
        true ->

            {Proof3, _} = 
                restore_leaves_proof(Proof2, Leaves),
            Proof4 = get_verkle:deserialize_proof(
                       get_verkle:serialize_proof(Proof3)),
    %checking that serializing doesn't beak anything.
            if
                not(Proof == Proof3) -> 
                    io:fwrite({element(1, Proof) == element(1, Proof3), element(1, Proof), element(1, Proof3)}),
                    1=2;
                not(Proof3 == Proof4) ->
                    io:fwrite("serialization issue\n"),
                    1=2;
                true -> ok
            end
    end,
    case Type of
        small -> 
            if
                not(?sanity) -> ok;
                true ->
                    io:fwrite("sanity check verkle proof\n"),
                    {true, _, _} = 
                        verify_verkle:proof(Proof, CFG)
            end,
            {get_verkle:serialize_proof(
                   Proof2), Leaves};
        fast -> 
            {Proof2, Leaves}
    end.
depth_order(Keys) ->
    K2 = lists:map(fun(K) ->
                           <<A:256/little>> = K,
                           <<B:256>> = 
                               <<A:256/big>>,
                           {K, B}
                   end, Keys),
    K3 = lists:sort(fun({K, B}, {K2, B2}) ->
                            B < B2
                    end, K2),
    lists:map(fun({K, _}) ->
                      K end, K3).
    
                      

remove_leaves_proof([]) -> [];
remove_leaves_proof({I, 0}) -> {I, 0};
remove_leaves_proof({I, {<<K:256>>, <<V:256>>}}) -> 
    {I, 1};
remove_leaves_proof(T) when is_tuple(T) -> 
    list_to_tuple(
      remove_leaves_proof(
        tuple_to_list(T)));
remove_leaves_proof([H|T]) -> 
    [remove_leaves_proof(H)|
     remove_leaves_proof(T)];
remove_leaves_proof(<<X:256>>) ->
    <<X:256>>;
remove_leaves_proof(N) when is_integer(N) -> N.


restore_leaves_proof([], Leaves) -> {[], Leaves};
restore_leaves_proof([{I, 0}], T) -> 
    {[{I, 0}], T};
restore_leaves_proof(Proofs, [{_Tree, _K}|T]) -> 
    %skip empty slot.
    restore_leaves_proof(Proofs, T);
restore_leaves_proof([{I, 1}], [L|T]) -> 
    case L of
        {_Tree, _Key} -> 
            {[{I, 0}], T};
        _ -> 
            V = hash:doit(serialize(L)),
            K = key(L),
            {[{I, {K, V}}], T}
    end;
restore_leaves_proof(Proofs, Leaves) 
  when is_tuple(Proofs) -> 
    {Proofs2, Leaves2} = 
        restore_leaves_proof(
          tuple_to_list(Proofs), Leaves),
    {list_to_tuple(Proofs2), Leaves2};
restore_leaves_proof([H|T], L) -> 
    {H2, L2} = restore_leaves_proof(H, L),
    {T2, L3} = restore_leaves_proof(T, L2),
    {[H2|T2], L3};
restore_leaves_proof(<<X:256>>, L) ->
    {<<X:256>>, L};
restore_leaves_proof(X, L) when is_integer(X) ->
    {X, L}.

    


%only used in a sanity check, maybe we should delete this.
key_tree_order([]) -> [];
%key_tree_order({I, 0}) ->
%empty slot
%    [<<0:256>>];
key_tree_order({I, {<<K:256>>, <<V:256>>}}) 
  when is_integer(I) -> [<<K:256>>];
key_tree_order(T) when is_tuple(T) -> 
      key_tree_order(
        tuple_to_list(T));
key_tree_order([H|T]) -> 
    key_tree_order(H) ++ key_tree_order(T);
key_tree_order(<<X:256>>) -> [];
key_tree_order(I) when is_integer(I) -> [];
key_tree_order(X) -> 
    io:fwrite({X}),
    1=2.
    


dump_get(T, V) ->
    S = dump:get(V, int2dump_name(T)),
    deserialize(T, S).
    

verify_proof(Proof0, Things) ->
    CFG = tree:cfg(amoveo),
    
    Proof1 = 
        if
            is_binary(Proof0) ->
                get_verkle:deserialize_proof(Proof0);
            true -> Proof0
        end,
    {Proof, []} = 
        restore_leaves_proof(Proof1, Things),

    CFG = tree:cfg(amoveo),
    {true, Leaves, ProofTree} = 
        verify_verkle:proof(Proof, CFG),
    %todo. in verify_verkle:proof, if there are 2 things stored in the same branch, and you try to make a proof of both of them, when you verify the proof, only one of the 2 things is included.
    %or maybe it is just missing leaves.
    lists:map(fun(X) ->
                      case X of
                          {unmatched, _, accounts, _, _} ->
                              io:fwrite(Things),
                              1=2;
                          #unmatched{oracle = accounts} ->
                              io:fwrite(Things),
                              1=2;
                          _ -> ok
                      end
              end, Things),

    Ks = to_keys(Things),
    
    Hs = lists:map(
           fun(A) -> 
                   case A of
                       {Foo, _Bar} -> 
                           if
                               (is_atom(Foo)) ->
                                   0;
                               true ->
                                   hash:doit(serialize(A))
                           end;
                       _ ->
                           hash:doit(
                             serialize(A))
                   end
           end, Things),
    KHs = lists:zipwith(fun(K, H) -> {K, H} end,
                        Ks, Hs),
    %io:fwrite({Leaves, Ks}),
    %io:fwrite("starting what you need: \n"),
    %print_pairs(KHs),%this is missing an element if there are 2 things that start with the same first step in their paths. todo
    %io:fwrite("starting what you got: \n"),
    %print_pairs(Leaves),
    %io:fwrite("\n"),
    
    Bool = merge_same(KHs, Leaves),
    %io:fwrite(
    %{lists:sort(KHs), lists:sort(Leaves)}),
    %{lists:sort(KHs) == lists:sort(Leaves),
    {Bool, ProofTree}.
%verify_proof(Proof) ->
%    CFG = tree:cfg(amoveo),
%    Proof1 = get_verkle:deserialize_proof(Proof),
%    verify_verkle:proof(Proof1, CFG).

merge_same([], []) -> true;
merge_same([X|T1], %what we need
           T2 = [{D, X}|_] %what we got
          ) 
  when is_integer(D) ->
    %io:fwrite("merged same pair\n"),
    %io:fwrite(integer_to_list(size(term_to_binary([X|T1])))),
    %io:fwrite(" - "),
    %io:fwrite(integer_to_list(size(term_to_binary(T2)))),
    %io:fwrite("\n"),
    merge_same(T1, T2);%we leave the X in the got pile, because it is possible we still need to match more things with this leaf. This leaf is evidence that certain locations are empty.

%merge_same([X|T1], [X|T2]) ->
%    io:fwrite("merged same pair\n"),
%    merge_same(T1, T2);
merge_same([{Key, 0}|T1], %what you need.
           [{D, {LKey, Val}}|T2]) %what you got. 
  when is_integer(D) ->
    %io:fwrite("merged empty \n"),
    %io:fwrite(integer_to_list(size(term_to_binary(T1)))),
    %io:fwrite(" - "),
    %io:fwrite(integer_to_list(size(term_to_binary(T2)))),
    %io:fwrite("\n"),
    CFG = tree:cfg(amoveo),
    <<Key0:256>> = Key,
    Key2 = leaf_verkle:path_maker(Key0, CFG),

    <<LKey0:256>> = LKey,
    LKey2 = leaf_verkle:path_maker(LKey0, CFG),

    false = Key == LKey,
    SSD = starts_same_depth(Key2, LKey2, D),
    case SSD of
        true -> 
            merge_same(T1, [{D, {LKey, Val}}|T2]);
        skip -> 
            %maybe this leaf was already used, or we aren't using it. lets continue.
            merge_same([{Key, 0}|T1], T2);
        false -> 
            <<_:240, SecondKey, FirstKey>> = Key,
            <<_:240, SecondLKey, FirstLKey>> = LKey,
            %<<_:240, SecondT2, FirstT2>> = element(1, element(2, hd(T2))),
            <<_:240, SecondT1, FirstT1>> = 
                case T1 of
                    [] -> <<0:256>>;
                    _ -> element(1, hd(T1)) 
                end,
            io:fwrite(
              {D, {need, {FirstKey, SecondKey}}, 
               {got, {FirstLKey, SecondLKey}},
               {next_need, {FirstT1, SecondT1}},
               %{next_got, {FirstT2, SecondT2}}
               {next_got, T2}}),
            1=2
    end;
merge_same([{Key, 0}|T1], [{Branch, 0}|T2]) ->
    %if doesn't match branch, recurse to see if it matches the next branch.
    %if it does match, keep the branch to see if more match.
    %io:fwrite("empty empty\n"),
    %io:fwrite(integer_to_list(length(T1))),
    %io:fwrite(" - "),
    %io:fwrite(integer_to_list(length(T2))),
    %io:fwrite("\n"),
    CFG = tree:cfg(amoveo),
    <<Key0:256>> = Key,
    Key2 = leaf_verkle:path_maker(Key0, CFG),
    Bool = starts_same(Key2, lists:reverse(Branch)),
    if
        Bool -> 
            %io:fwrite("merged empty 2\n"),
            merge_same(T1, [{Branch, 0}|T2]);
        true -> 
            %io:fwrite("nothing left to match with this branch 2 --  "),
            %print_empty_branch(Branch),
            <<K1>> = hd(Key2),
            <<K2>> = hd(tl(Key2)),
            %io:fwrite(" %% "),
            %io:fwrite(integer_to_list(K1)),
            %io:fwrite(" "),
            %io:fwrite(integer_to_list(K2)),
            %io:fwrite("\n"),
            %io:fwrite("branch length "),
            %io:fwrite(integer_to_list(length(Branch))),
            %io:fwrite("\n"),
            merge_same([{Key, 0}|T1], T2)
    end;
%merge_same([{_, 0}], []) -> true;
merge_same(X, [{Branch, 0}|T2]) ->
    %nothing left to match with this branch.
    %io:fwrite("nothing left to match with this branch\n"),
    %io:fwrite(integer_to_list(length(X))),
    %io:fwrite(" - "),
    %io:fwrite(integer_to_list(length(T2))),
    %io:fwrite("\n"),
    merge_same(X, T2);
merge_same(X, [{D, {K, V}}|T2]) 
  when is_integer(D) and 
       is_binary(K) and 
       is_binary(V) and 
       (32 == size(K)) and 
       (32 == size(V)) ->
    %io:fwrite("nothing left to match with this leaf\n"),%maybe was used to show that a branch is empty.
    %io:fwrite(integer_to_list(length(X))),
    %io:fwrite(" - "),
    %io:fwrite(integer_to_list(length(T2))),
    %io:fwrite("\n"),
    merge_same(X, T2);
merge_same(A, B) -> 
    io:fwrite("what you need: \n"),
    print_pairs(A),
    io:fwrite("what you got: \n"),
    print_pairs(B),
    B2 = case B of
             [] -> [[]];
             _ -> B
         end,
    io:fwrite({hd(A), hd(B2), length(A), length(B)}),
    1=2,
    false.

print_pairs([]) -> 
    io:fwrite("finished print pairs"),
    io:fwrite("\n"),
    ok;
print_pairs([{<<_:232, Z, Y, X>>, _}|T]) -> 
    io:fwrite("key starts with: "),
    io:fwrite(integer_to_list(X)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(Y)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(Z)),
    io:fwrite("\n"),
    print_pairs(T);
print_pairs([{D, {<<_:232, Z, Y, X>>, _}}|T]) -> 
    io:fwrite("key starts with: "),
    io:fwrite(integer_to_list(X)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(Y)),
    io:fwrite(" "),
    io:fwrite(integer_to_list(Z)),
    io:fwrite(" depth: "),
    io:fwrite(integer_to_list(D)),
    io:fwrite("\n"),
    print_pairs(T);
print_pairs([{L = [_|_], _}|T]) -> 
    io:fwrite("empty branch: "),
    print_empty_branch(L),
    io:fwrite("\n"),
    print_pairs(T).
print_empty_branch([]) -> ok;
print_empty_branch([H|T]) ->
    print_empty_branch(T),
    io:fwrite(integer_to_list(H)),
    io:fwrite(" ").
    
                   
    


starts_same(_, []) -> true;
starts_same([<<X>>|T], [X|T2]) ->
    starts_same(T, T2);
starts_same(_, _) -> false.

starts_same_depth(_, _, 0) -> true;
starts_same_depth([<<X>>|T1], [<<X>>|T2], D) ->
    %io:fwrite("starts same depth "),
    %io:fwrite(integer_to_list(X)),
    %io:fwrite("\n"),
    starts_same_depth(T1, T2, D-1);
starts_same_depth([<<A>>|_], [<<B>>|_], C) -> 
    %io:fwrite({hd(A), hd(B), length(A), length(B), C}),
    if
        (B < A) -> skip;
        true -> false
    end.





prune(Trash, Keep) when (is_integer(Trash) and (is_integer(Keep))) ->
    if
        (Trash == Keep) -> 
            io:fwrite("trees2:prune. trash==keep.\n"),
            ok;
        true ->
    %io:fwrite("trees2 prune\n"),
    %io:fwrite("keep: "),
    %io:fwrite(integer_to_list(Keep)),
    %io:fwrite("\n"),
    %io:fwrite("trash: "),
    %io:fwrite(integer_to_list(Trash)),
    %io:fwrite("\n"),
            tree:quick_save(amoveo),
    RemovedLeaves = tree:garbage(Keep, Trash, amoveo),
%    io:fwrite("many to remove: "),
%    io:fwrite(integer_to_list(length(RemovedLeaves))),
%    io:fwrite("\n"),
    lists:map(fun(L = {leaf, _Key, _Value, Meta}) ->
                      delete_thing(Meta)
              end, RemovedLeaves),
    ok
    end.
delete_thing(<<X, Loc:56>>) ->
    %io:fwrite("trees2 delete_thing\n"),
    DBname = int2dump_name(X),
    dump:delete(Loc, DBname).

merkle2verkle(
  Tree = #trees5{
     accounts = A, channels = _C, existence = _E, 
     oracles = O, governance = _G, matched = M,
     unmatched = U, sub_accounts = SA, contracts = CO,
     trades = T, markets = M2, receipts = R}, 
  Loc) ->
    Types = [accounts, oracles, matched, unmatched, sub_accounts, contracts, trades, markets, receipts],
    TypePairs = lists:zipwith(
                  fun(A, B) -> {A, B} end,
                  Types,
                  [A, O, M, U, SA, CO, T, M2, R]),
    AllLeaves = lists:foldl(
      fun({Type, X}, A) ->
              Leaves0 = trie:get_all(X, Type),
              Leaves = 
                  lists:map(
                    fun(F) -> 
                            Des = (Type):deserialize(leaf:value(F)),
%                            case Des of
%                                {_, 1} -> io:fwrite({Type, F, leaf:value(F)}),
%                                          1 = 2;
%                                _ -> ok
%                            end,
                            Des
                    end,
                    Leaves0),
              if
                  true -> ok;
                  (length(Leaves) == 2) ->
                      io:fwrite({Leaves0});
                  true -> ok
              end,
              A ++ Leaves
      end, [], TypePairs),
    %io:fwrite(AllLeaves),
    %io:fwrite("trees2 store things \n"),
    %io:fwrite(AllLeaves),
    store_things(AllLeaves, Loc).
-record(cfg, {path, value, id, meta, hash_size, mode, empty_root, parameters}).
one_root_clean(Pointer, CFG) ->
    bits:top(jobs_cleaner),
    Hash = scan_verkle(Pointer, CFG),
    io:fwrite("one root clean 0\n"),
    bits:top(jobs_cleaner),
    io:fwrite("one root clean 1\n"),
    NewPointer = one_root_maker(Pointer, CFG),
    io:fwrite("one root clean 2\n"),
    CFG2 = CFG#cfg{id = cleaner},
    bits:top(jobs_cleaner),
    io:fwrite("one root clean 3\n"),
    recover_from_clean_version(NewPointer),
    bits:top(jobs_cleaner),
    io:fwrite("one root clean 4\n"),
    Hash = scan_verkle(NewPointer, CFG),
    NewPointer.

one_root_maker(Pointer, CFG) ->
    %delete the contents of the files in the cleaner folder.
    %os:cmd("truncate -s 0 cleaner/data/*"),
    %os:cmd("rm cleaner/data/*"),
    os:cmd("rm -r cleaner/data"),
    %os:cmd("cp -r cleaner/empty_version cleaner/data"),
    os:cmd("tar -xf ../../../../empty_version.tar.gz"),
    os:cmd("cp -r empty_version cleaner/data"),
    timer:sleep(500),
    %reload the cleaner verkle tree, it should be empty.
    %io:fwrite("one_root_clean: reload the now empty cleaner db\n"),
    bits:reset(accounts_cleaner),
    bits:reset(cleaner_v_leaf),
    bits:reset(cleaner_v_stem),
    bits:reset(contracts_cleaner),
    bits:reset(markets_cleaner),
    bits:reset(matched_cleaner),
    bits:reset(oracles_cleaner),
    bits:reset(receipts_cleaner),
    bits:reset(sub_accs_cleaner),
    bits:reset(trades_cleaner),
    bits:reset(unmatched_cleaner),
    bits:reset(jobs_cleaner),

    io:fwrite("one root maker 0\n"),
    bits:top(jobs_cleaner),
    io:fwrite("one root maker 1\n"),

    dump:reload(accounts_cleaner),
    dump:reload(contracts_cleaner),
    dump:reload(markets_cleaner),
    dump:reload(matched_cleaner),
    dump:reload(oracles_cleaner),
    dump:reload(receipts_cleaner),
    dump:reload(sub_accs_cleaner),
    dump:reload(trades_cleaner),
    dump:reload(unmatched_cleaner),
    dump:reload(jobs_cleaner),

    io:fwrite("one root maker 2\n"),
    bits:top(jobs_cleaner),
    io:fwrite("one root maker 3\n"),

    tree:reload_ets(cleaner),
    timer:sleep(500),
    %build the clean version
    io:fwrite("one_root_clean: copy the data for that one root to the cleaner db\n"),
    CFG2 = CFG#cfg{id = cleaner},
    NewPointer = one_root_clean_stem(Pointer, CFG, CFG2),
    %copy the clean version over the main version.
    io:fwrite("one_root_clean: back up the cleaner db to the hard disk\n"),

    io:fwrite("one root maker 4\n"),
    bits:top(jobs_cleaner),
    io:fwrite("one root maker 5\n"),

    tree:quick_save(cleaner),%this is not backing up the consensus state to any files. Where are we writing and reading to???

    io:fwrite("one root maker 6\n"),
    bits:top(jobs_cleaner),
    io:fwrite("one root maker 7\n"),

    NewPointer.

recover_from_clean_version(Pointer) ->
    io:fwrite("one_root_clean: copying everything from the cleaner db back to the main db\n"),
    io:fwrite("clean pointer: "),
    io:fwrite(integer_to_list(Pointer)),
    io:fwrite("\n"),
    

    bits:top(jobs_cleaner),
    io:fwrite("trees2:recover_from_clean_version -1\n"),

    %TODO. we need to make sure it is on the hard disk in the cleaner folder before we start copying things.

    IDs = [1,3,4,5,6,7,8,9,10,11],

    lists:map(fun(ID) -> dump:quick_save(
                           int2cleaner_name(ID)) end, 
              IDs),

    bits:top(jobs_cleaner),
    io:fwrite("trees2:recover_from_clean_version -2\n"),

    dump:quick_save(cleaner_v_leaf),
    dump:quick_save(cleaner_v_stem),
    timer:sleep(3000),

    bits:top(jobs_cleaner),
    io:fwrite("trees2:recover_from_clean_version 0\n"),

    os:cmd("cp -r cleaner/data/accounts_cleaner.db ../../../../db/data/accounts_dump.db"),
    os:cmd("cp -r cleaner/data/contracts_cleaner.db ../../../../db/data/contracts_dump.db"),
    os:cmd("cp -r cleaner/data/existence_cleaner.db ../../../../db/data/existence_dump.db"),
    os:cmd("cp -r cleaner/data/markets_cleaner.db ../../../../db/data/markets_dump.db"),
    os:cmd("cp -r cleaner/data/matched_cleaner.db ../../../../db/data/matched_dump.db"),
    os:cmd("cp -r cleaner/data/oracles_cleaner.db ../../../../db/data/oracles_dump.db"),
    os:cmd("cp -r cleaner/data/receipts_cleaner.db ../../../../db/data/receipts_dump.db"),
    os:cmd("cp -r cleaner/data/sub_acc_cleaner.db ../../../../db/data/sub_acc_dump.db"),
    os:cmd("cp -r cleaner/data/trades_cleaner.db ../../../../db/data/trades_dump.db"),
    os:cmd("cp -r cleaner/data/unmatched_cleaner.db ../../../../db/data/unmatched_dump.db"),
    os:cmd("cp -r cleaner/data/jobs_cleaner.db ../../../../db/data/jobs_dump.db"),

    bits:top(jobs_cleaner),
    io:fwrite("trees2:recover_from_clean_version 1\n"),

    os:cmd("cp -r cleaner/data/accounts_cleaner_rest.db ../../../../db/data/accounts_dump_rest.db"),
    os:cmd("cp -r cleaner/data/contracts_cleaner_rest.db ../../../../db/data/contracts_dump_rest.db"),
    os:cmd("cp -r cleaner/data/existence_cleaner_rest.db ../../../../db/data/existence_dump_rest.db"),
    os:cmd("cp -r cleaner/data/markets_cleaner_rest.db ../../../../db/data/markets_dump_rest.db"),
    os:cmd("cp -r cleaner/data/matched_cleaner_rest.db ../../../../db/data/matched_dump_rest.db"),
    os:cmd("cp -r cleaner/data/oracles_cleaner_rest.db ../../../../db/data/oracles_dump_rest.db"),
    os:cmd("cp -r cleaner/data/receipts_cleaner_rest.db ../../../../db/data/receipts_dump_rest.db"),
    os:cmd("cp -r cleaner/data/sub_accs_cleaner_rest.db ../../../../db/data/sub_accs_dump_rest.db"),
    os:cmd("cp -r cleaner/data/trades_cleaner_rest.db ../../../../db/data/trades_dump_rest.db"),
    os:cmd("cp -r cleaner/data/unmatched_cleaner_rest.db ../../../../db/data/unmatched_dump_rest.db"),
    os:cmd("cp -r cleaner/data/jobs_cleaner_rest.db ../../../../db/data/jobs_dump_rest.db"),

    bits:top(jobs_cleaner),
    io:fwrite("trees2:recover_from_clean_version 2\n"),

    lists:map(fun(ID_num) ->
                      Name = int2dump_name(ID_num),
                      CleanName = int2cleaner_name(ID_num),
                      io:fwrite("try copying bits "),
                      io:fwrite(Name),
                      io:fwrite("\n"),
                      Top = bits:top(CleanName),%dies here on the jobs iteration.
                      io:fwrite("reset bits\n"),
                      bits:reset(Name),
                      io:fwrite("reset bits 2\n"),
                      bits:top(jobs_cleaner),
                      io:fwrite("clean top\n"),
                      copy_bits(1, Top, CleanName, Name),
                      io:fwrite("clean_bits/4\n"),
                      Top = bits:top(Name),
                      io:fwrite("name top\n"),
                      bits:quick_save(Name),
                      io:fwrite("copying bits "),
                      io:fwrite(Name),
                      io:fwrite(" "),
                      io:fwrite(integer_to_list(Top)),
                      io:fwrite("\n")
              end, IDs),

    bits:top(jobs_cleaner),
    io:fwrite("trees2:recover_from_clean_version 3\n"),

    bits:reset(amoveo_v_leaf),
    bits:reset(amoveo_v_stem),
    copy_bits(1, bits:top(cleaner_v_leaf), cleaner_v_leaf, amoveo_v_leaf),
    copy_bits(1, bits:top(cleaner_v_stem), cleaner_v_stem, amoveo_v_stem),
                      bits:quick_save(amoveo_v_leaf),
                      bits:quick_save(amoveo_v_stem),


%    os:cmd("cp -r cleaner/data/cleaner_v_leaf_bits.db ../../../../db/data/amoveo_v_leaf_bits.db"),
    os:cmd("cp -r cleaner/data/cleaner_v_leaf.db ../../../../db/data/amoveo_v_leaf.db"),
    os:cmd("cp -r cleaner/data/cleaner_v_leaf_rest.db ../../../../db/data/amoveo_v_leaf_rest.db"),
%    os:cmd("cp -r cleaner/data/cleaner_v_stem_bits.db ../../../../db/data/amoveo_v_stem_bits.db"),
    os:cmd("cp -r cleaner/data/cleaner_v_stem.db ../../../../db/data/amoveo_v_stem.db"),
    os:cmd("cp -r cleaner/data/cleaner_v_stem_rest.db ../../../../db/data/amoveo_v_stem_rest.db"),

    timer:sleep(1000),



%accounts_cleaner.db     cleaner_v_leaf_rest.db  cleaner_v_stem_rest.db  matched_cleaner.db   sub_accs_cleaner.db
%cleaner_v_leaf_bits.db  cleaner_v_stem_bits.db  contracts_cleaner.db    oracles_cleaner.db   trades_cleaner.db
%cleaner_v_leaf.db       cleaner_v_stem.db       markets_cleaner.db      receipts_cleaner.db  unmatched_cleaner.db

    %reload the verkle tree.
    io:fwrite("one_root_clean: reloading the main db \n"),
    tree:reload_ets(amoveo),
%    dump:reload(amoveo_v_leaf),%reload the bits part....
%    dump:reload(amoveo_v_stem),
    %tree:clean_ets(amoveo, Pointer),
    dump:reload(accounts_dump),
    dump:reload(contracts_dump),
    dump:reload(markets_dump),
    dump:reload(matched_dump),
    dump:reload(oracles_dump),
    dump:reload(receipts_dump),
    dump:reload(sub_accs_dump),
    dump:reload(trades_dump),
    dump:reload(unmatched_dump),
    dump:reload(jobs_dump),

    %delete the contents of the cleaner folder to save space.
    %os:cmd("rm -rf cleaner/*.db"),
    ok.

copy_bits(X, Top, _, _) when X > Top ->
    ok;
copy_bits(I, Top, CleanName, Name) ->
    B = bits:get(CleanName, I),
    if
        B ->
            bits:set(Name, I);
        true -> ok
    end,
    copy_bits(I+1, Top, CleanName, Name).
    

one_root_clean_stem(Pointer, 
               CFG, %the old database we are reading from.
               CFG2) -> %the new database we are inserting to.
    
    %make a new verkle database. copy over everything that we want to keep. It is a depth first scan of the old tree.

    %make the new database.
    %io:fwrite("one root clean stem "),
    %io:fwrite(integer_to_list(Pointer)),
    %io:fwrite("\n"),
    S = stem_verkle:get(Pointer, CFG),
    SanityHash = stem_verkle:hash(S),
    P = tuple_to_list(stem_verkle:pointers(S)),
    T = tuple_to_list(stem_verkle:types(S)),
    H = tuple_to_list(stem_verkle:hashes(S)),
    P2 = one_root_clean2(P, T, H, CFG, CFG2),
    S2 = setelement(4, S, list_to_tuple(P2)),
    SanityHash = stem_verkle:hash(S2),
    %S2 = S#stem_verkle{pointers = list_to_tuple(P2)},
    stem_verkle:put(S2, CFG2).
-record(leaf, {key, value, meta}).
one_root_clean2([], [], _, _, _) -> [];
one_root_clean2(
  [Pointer|PT], [Type|TT], [Hash|HT], 
  CFG, CFG2 ) -> 
    P2 = case Type of
             0 -> %empty
                 Hash = <<0:256>>,
                 0;
             1 -> %another stem
                 P3 = one_root_clean_stem(Pointer, CFG, CFG2),
                 Stem = stem_verkle:get(P3, CFG2),
                 Hash2 = stem_verkle:hash(Stem),%different.
                 if
                     not(Hash == Hash2) -> 
                         Stem0 = stem_verkle:get(Pointer, CFG),
                         Hash3 = stem_verkle:hash(Stem0),
                         io:fwrite({Hash2, Hash, Hash3, Stem, Stem0});
                     true -> ok
                 end,
                 P3;
             2 -> %a leaf
                 Leaf = leaf_verkle:get(Pointer, CFG),
                 #leaf{key = Key, value = LeafHash, meta = Meta} = Leaf,
                 Hash = store_verkle:leaf_hash(Leaf, CFG),
                 %Hash = leaf_verkle:hash(Leaf, CFG),
                 %<<N:256>> = store_verkle:leaf_hash(Leaf, CFG),
                 <<M1, Pointer2:(7*8)>> = Meta,
                 %Type = int2type(M1),
                 CS0 = dump:get(Pointer2, int2dump_name(M1)),
                 %CSHash = hash:doit(CS0),
                 %CS = deserialize(M1, CS0),

                 Pointer4 = dump:put(CS0, int2cleaner_name(M1)),
                 Meta2 = <<M1, Pointer4:(7*8)>>,
                 Leaf2 = Leaf#leaf{meta = Meta2},

                 %io:fwrite({Hash, LeafHash, CSHash}),
                 %io:fwrite({Key, M1, CS}),
                 %Hash = fr:encode(N),
                 %SL = leaf_verkle:serialize(Leaf, CFG),
                 %ets:insert(LID, {Pointer, SL})

                 %todo, we need to store the actual consensus state data to it's file as well. decode the leaf, and store the data in the cleaner db.
                 %from cs2v
                 %meta = <<M1, M:(7*8)>>
            %V = serialize(A),
            %H = hash:doit(V),
            %M1 = type2int(element(1, A)),
            %DBName = int2dump_name(M1),


                 Pointer3 = leaf_verkle:put(Leaf2, CFG2),
                 %io:fwrite("put a leaf. stem1: "),
                 %io:fwrite(integer_to_list(Pointer)),
                 %io:fwrite(",  stem2: "),
                 %io:fwrite(integer_to_list(Pointer3)),
                 %io:fwrite(",  leaf1: "),
                 %io:fwrite(integer_to_list(Pointer2)),
                 %io:fwrite(",  leaf2: "),
                 %io:fwrite(integer_to_list(Pointer4)),
                 %io:fwrite("\n"),
                 Pointer3
         end,
    [P2|one_root_clean2(PT, TT, HT, CFG, CFG2)].

scan_verkle() ->
    Pointer = (block:top())#block.trees,
    CFG = tree:cfg(amoveo),
    scan_verkle(Pointer, CFG).
scan_verkle(Pointer, CFG) ->
    S = stem_verkle:get(Pointer, CFG),
    P = tuple_to_list(stem_verkle:pointers(S)),
    T = tuple_to_list(stem_verkle:types(S)),
    H = tuple_to_list(stem_verkle:hashes(S)),
    success = scan_verkle2(P, T, H, CFG),
    stem_verkle:hash(S).
scan_verkle2([],[],[],_) -> success;
scan_verkle2([0|PT], [0|TT], [<<0:256>>|HT], CFG) -> 
    %empty slot
    success = scan_verkle2(PT, TT, HT, CFG);
scan_verkle2([Pointer|PT], [2|TT], [Hash|HT], CFG) -> 
    %a leaf.
    %io:fwrite("scanned a leaf\n"),
    L = leaf_verkle:get(Pointer, CFG),
    #leaf{key = Key, value = LeafHash, meta = Meta} = L,
    <<M1, Pointer2:(7*8)>> = Meta,
    CS0 = dump:get(Pointer2, int2dump_name(M1)),
    Hash3 = hash:doit(CS0),
    CS = deserialize(M1, CS0),
    Hash2 = store_verkle:leaf_hash(L, CFG),
    if
        not(Hash == Hash2) -> 
            io:fwrite("trees2:scan_verkle2: bad leaf verkle data\n"),
            1=2;
        not(LeafHash == Hash3) ->
            io:fwrite("trees2:scan_verkle2 bad cs data\n"),
            io:fwrite(integer_to_list(Pointer2)),
            io:fwrite("\n"),
            io:fwrite({M1, Pointer2, CS});
            %1=2;
            %ok;
        true -> 
            ok
    end,
    success = scan_verkle2(PT, TT, HT, CFG);
scan_verkle2([Pointer|PT], [1|TT], [Hash|HT], CFG) -> 
    %another stem.
    Hash2 = scan_verkle(Pointer, CFG),
    if
        not(Hash == Hash2) -> 
            io:fwrite("bad stem hash\n"),
            1=2;
        true -> ok
    end,
    success = scan_verkle2(PT, TT, HT, CFG);
scan_verkle2(_, _, _, _) -> 
    io:fwrite("scan verkle 2 impossible error\n"),
    1=2.
   

recover(0) ->
    %we lost almost all the consensus state, so we are trying to recover it from some data from an old node's database.

    os:cmd("cp -r ../../../../../german_backup/accounts_dump.db cleaner/data/accounts_cleaner.db"),
    os:cmd("cp -r ../../../../../german_backup/contracts_dump.db cleaner/data/contracts_cleaner.db"),
    os:cmd("cp -r ../../../../../german_backup/existence_dump.db cleaner/data/existence_cleaner.db"),
    os:cmd("cp -r ../../../../../german_backup/markets_dump.db cleaner/data/markets_cleaner.db"),
    os:cmd("cp -r ../../../../../german_backup/matched_dump.db cleaner/data/matched_cleaner.db"),
    os:cmd("cp -r ../../../../../german_backup/oracles_dump.db cleaner/data/oracles_cleaner.db"),
    os:cmd("cp -r ../../../../../german_backup/receipts_dump.db cleaner/data/receipts_cleaner.db"),
    os:cmd("cp -r ../../../../../german_backup/sub_accs_dump.db cleaner/data/sub_accs_cleaner.db"),
    os:cmd("cp -r ../../../../../german_backup/trades_dump.db cleaner/data/trades_cleaner.db"),
    os:cmd("cp -r ../../../../../german_backup/unmatched_dump.db cleaner/data/unmatched_cleaner.db"),
    
    os:cmd("cp -r ../../../../../german_backup/accounts_dump_rest.db cleaner/data/accounts_cleaner_rest.db"),
    os:cmd("cp -r ../../../../../german_backup/contracts_dump_rest.db cleaner/data/contracts_cleaner_rest.db"),
    os:cmd("cp -r ../../../../../german_backup/existence_dump_rest.db cleaner/data/existence_cleaner_rest.db"),
    os:cmd("cp -r ../../../../../german_backup/markets_dump_rest.db cleaner/data/markets_cleaner_rest.db"),
    os:cmd("cp -r ../../../../../german_backup/matched_dump_rest.db cleaner/data/matched_cleaner_rest.db"),
    os:cmd("cp -r ../../../../../german_backup/oracles_dump_rest.db cleaner/data/oracles_cleaner_rest.db"),
    os:cmd("cp -r ../../../../../german_backup/receipts_dump_rest.db cleaner/data/receipts_cleaner_rest.db"),
    os:cmd("cp -r ../../../../../german_backup/sub_accs_dump_rest.db cleaner/data/sub_accs_cleaner_rest.db"),
    os:cmd("cp -r ../../../../../german_backup/trades_dump_rest.db cleaner/data/trades_cleaner_rest.db"),
    os:cmd("cp -r ../../../../../german_backup/unmatched_dump_rest.db cleaner/data/unmatched_cleaner_rest.db"),


    IDs = [1,3,4,5,6,7,8,9,10],
    lists:map(fun(ID_num) ->
                      
                      CleanName = int2cleaner_name(ID_num),
                      Name = int2dump_name(ID_num),
                      {Top} = binary_to_term(db:read("cleaner/data/" ++ atom_to_list(CleanName) ++ "_rest.db")),
                      io:fwrite("recovering "),
                      io:fwrite(atom_to_list(Name)),
                      io:fwrite(" it has top: "),
                      io:fwrite(integer_to_list(Top)),
                      io:fwrite("\n"),
                      %recover_range(1, bits:top(CleanName),
                      Top2 = case Top of
                                 1 -> 2000;
                                 Top -> Top
                             end,
                      recover_range(1, Top2,
                                    Name, CleanName, ID_num)
              end, IDs),

    ok.
                                    
recover_range(I, End, Name, _, _) when I > End -> 
    io:fwrite("recover range finished "),
    io:fwrite(atom_to_list(Name)),
    io:fwrite("\n"),
    ok;
recover_range(I, End, Name, CleanName, ID_num) ->

    CS = dump:get(I, CleanName),
    D = deserialize(ID_num, CS),
    K = trees2:key(D),
    Loc = (block:top())#block.trees,
    Leaf = get_leaf(K, Loc, tree:cfg(amoveo)),
    if
        (none == Leaf) -> ok;
        true ->
            K2 = leaf_verkle:raw_key(Leaf),
            if
                not(K2 == K) -> ok;
                true ->

    <<ID_num, V:56>> = leaf_verkle:meta(Leaf),
    S = dump:get(V, int2dump_name(ID_num)),
    %V1 = dump_get(ID_num, V),
    %S = serialize(V1),
    CFG = tree:cfg(amoveo),
    
    LeafHashShouldBe = leaf_verkle:value(Leaf),
    ExistingLeafHash = hash:doit(S),
    ReplacementLeafHash = hash:doit(CS),
    if
        ExistingLeafHash == LeafHashShouldBe ->
            %io:fwrite("data is already good, so change nothing\n");
            ok;
        ReplacementLeafHash == LeafHashShouldBe ->
            %store at pointer V in the matched dump.
            %store CS. 
            Word = size(CS),
            file_manager:write(Name, V*Word, CS),
            bits:set(Name, V),
            io:fwrite("replace this bad data\n");
        true ->
            io:fwrite("this is bad data, and the potential replacement is also bad\n")
    end
    end
    end,
    recover_range(I+1, End, Name, CleanName, ID_num).
    

get_leaf(<<Key:256>>, Pointer, CFG) ->
    Stem = stem_verkle:get(Pointer, CFG),
    %io:fwrite({Key}),
    Path = leaf_verkle:path_maker(Key, CFG),
    get_leaf2(Stem, Path, CFG).
get_leaf2(Stem, [<<P>>|Path], CFG) ->
    Type = element(P+1, stem_verkle:types(Stem)),
    Pointer = element(P+1, stem_verkle:pointers(Stem)),
    case Type of
        0 -> %io:fwrite("get leaf 2 impossible error\n"),
             %io:fwrite({[<<P>>|Path], Stem}),
             %1=2;
            none;
        2 -> %found the leaf
            leaf_verkle:get(Pointer, CFG);
        1 ->
            NextStem = stem_verkle:get(Pointer, CFG),
            get_leaf2(NextStem, Path, CFG)
    end.
            
            
            
    

all_zeros(<<>>) -> true;
all_zeros(<<0, R/binary>>) -> 
    all_zeros(R);
all_zeros(_) -> false.

 

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
    %testing making and verifying the verkle proof.
    
    Range = 2,
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
    Loc2 = store_things(As, Loc),
    
    {Proof, As0b} = get_proof(to_keys(As2), Loc2),
%make sure in and out are same length!! todo

    {true, ProofTree} = verify_proof(Proof, As0b),
    
    ProofTree2 = 
        update_proof(As2, ProofTree),
   % io:fwrite(ProofTree2),
    
    Loc3 = store_verified(Loc2, ProofTree2),

    {Proof3, As2b} = get_proof(to_keys(As2), Loc3),
    
    {true, V2} = verify_proof(Proof3, As2b),

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
    success;
test(3) ->
    %testing converting the merkle stuff to verkle stuff.
    {Pub0, _Priv} = signing:new_key(),
    Pub0c = compress_pub(Pub0),
    Acc0 = accounts:new(Pub0, 1000027),
    Empty = 1,
    A = accounts:write(Acc0, Empty),

    Start = 5,
    QuestionText = <<"question text">>,
    ID = oracle_new_tx:id_generator2(Start, 0, 0, QuestionText),
    Oracle0 = 
        oracles:new(
          ID, hash:doit(QuestionText), Start, Pub0, 
          0, 0, dict:new(), true, forks:get(52) + 10),
    O = oracles:write(Oracle0, Empty),

    Matched0 = matched:new(Pub0, ID, 1, 1000),
    M = matched:write(Matched0, Empty),

    Unmatched0 = unmatched:new(Pub0, ID, 2000),
    U = unmatched:write(Unmatched0, Empty),

    C0 = contracts:new(hash:doit(<<>>), 2),
    CID = contracts:make_id(C0),
    C = contracts:write(C0, Empty),

    SA0 = sub_accounts:new(Pub0, 1000029, CID, 1),
    SA = sub_accounts:write(SA0, Empty),

    Trade0 = trades:new(104, hash:doit(<<>>)),
    Tr = trades:write(Trade0, Empty),

    Market0 = markets:new(CID, 1, 10005, CID, 2, 10006),
    M = markets:write(Market0, Empty),

    Receipt0 = receipts:new(hash:doit(<<>>), Pub0, 1),
    R = receipts:write(Receipt0, Empty),

    T = #trees5{
      accounts = A, 
      oracles = O, 
      matched = M,
      unmatched = U, sub_accounts = SA,
      contracts = C, trades = Tr, 
      markets = M, receipts = R},
    V = merkle2verkle(T, 1),
    success;
test(4) ->
    %testing proofs of the non-existence of things.
    Many = 8000,
    Keys = lists:map(fun(_) -> signing:new_key()
                     end, range(1, Many)),
    As = lists:map(
           fun({P, _}) ->
                   #acc{pubkey = P, 
                        balance = 100000000, 
                        nonce = 0} 
           end, Keys),
    Loc = 1,
    {As0, As1} = lists:split(Many div 2, As),
    print_now(),
    io:fwrite("store things \n"),
    Loc2 = store_things(As1, Loc),
    %As0_1 = [hd(As0)] ++ As1,
    As0_1 = As,
    Keys2 = to_keys(As0_1),
    print_now(),
    io:fwrite("get proof\n"),
    {Proof, As2} = 
        get_proof(Keys2, Loc2),
    true = length(As2) == length(As0_1),
    print_now(),
    io:fwrite("things proved "),
    io:fwrite(integer_to_list(length(As2))),
    io:fwrite("\n"),
    print_now(),
    io:fwrite("verify proof\n"),
    {true, ProofTree} = 
        verify_proof(Proof, As2),
    As3 = lists:map(fun(A) ->
                            A#acc{balance = 28}
                    end, As0_1),
    print_now(),
    io:fwrite("update proof\n"),
    ProofTree2 = update_proof(As3, ProofTree),
    print_now(),
    io:fwrite("store new\n"),
    Loc3 = store_verified(Loc2, ProofTree2),
    print_now(),
    io:fwrite("get proof 2\n"),
    %io:fwrite({to_keys(As2)}),
    {Proof3, As2b} = get_proof(to_keys(As2), Loc3),
    print_now(),
    io:fwrite("verify proof 2\n"),
    %{true, V2} = verify_proof(Proof3, As3),
    {true, V2} = verify_proof(Proof3, As2b),
    prune(Loc2, Loc3),
    success;
test(5) ->
    %testing the tool for deleting everything besides the history connected to a single root.
    Many = 20,
    Keys = lists:map(fun(_) -> signing:new_key()
                     end, range(1, Many)),
    As = lists:map(
           fun({P, _}) ->
                   #acc{pubkey = P, 
                        balance = 100000000, 
                        nonce = 0} 
           end, Keys),
    AsB = lists:map(
           fun({P, _}) ->
                   #acc{pubkey = P, 
                        balance = 100000002, 
                        nonce = 0} 
           end, Keys),
    Loc = 1,
    {As0, As1} = lists:split(Many div 2, As),
    Loc2 = store_things(As1, Loc),
    CFG = tree:cfg(amoveo),
    Loc2V1 = stem_verkle:get(Loc2, CFG),
    Loc3 = store_things(AsB, Loc2),
    Loc4 = store_things(As0, Loc3),
    
    one_root_clean(Loc4, CFG),
    ok.
    %timer:sleep(1000),
    %loc2 should be empty. loc3 and loc4 should not be.
    %Loc2V2 = stem_verkle:get(Loc2, CFG),
    %{Loc2V1, Loc2V2}.

    
print_now() ->    
    {_, A, B} = erlang:timestamp(),
    B2 = B div 100000,
    io:fwrite(integer_to_list(A)),
    io:fwrite("."),
    io:fwrite(integer_to_list(B2)),
    io:fwrite(" "),
    ok.
    



