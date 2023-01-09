-module(markets).
-export([new/6, 
	 write/2, get/2, delete/2,%update tree stuff
         %dict_update/9, 
         dict_delete/2, dict_write/3, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
         make_id/1,make_id/4,
         det_sqrt/1,
	 all/0,
	 test/0]).
-include("../../records.hrl").

%https://en.wikipedia.org/wiki/Integer_square_root
det_sqrt(N) when N>0->
    det_sqrt(-1, 0, 1, N).
det_sqrt(Guess, Guess, _, _) -> Guess;
det_sqrt(Guess, Guess1, Guess, _) -> 
    min(Guess1, Guess);
det_sqrt(_, Guess0, Guess, N) ->
    Guess2 = (Guess + (N div Guess)) div 2,
    det_sqrt(Guess0, Guess, Guess2, N).
            

new(CID1, Type1, Amount1, CID2, Type2, Amount2) ->
    Shares = det_sqrt(Amount1*Amount2),
    #market{
     id = make_id(CID1, Type1, CID2, Type2),
     cid1 = CID1,
     type1 = Type1,
     amount1 = Amount1,
     cid2 = CID2,
     type2 = Type2,
     amount2 = Amount2,
     shares = Shares}.
serialize(M) ->
    #market{
      id = ID,
      cid1 = CID1,
      type1 = Type1,
      amount1 = Amount1,
      cid2 = CID2,
      type2 = Type2,
      amount2 = Amount2,
      shares = Shares
     } = M,
    BAL = constants:balance_bits(),
    32 = size(ID),
    32 = size(CID1),
    32 = size(CID2),
    <<ID/binary,
      CID1/binary,
      CID2/binary,
      Type1:16,
      Type2:16,
      Amount1:BAL,
      Amount2:BAL,
      Shares:BAL
    >>.

deserialize(B) ->
    BAL = constants:balance_bits(),
    HS = constants:hash_size()*8,
    <<ID:HS,
      CID1:HS,
      CID2:HS,
      Type1:16,
      Type2:16,
      Amount1:BAL,
      Amount2:BAL,
      Shares:BAL>> = B,
    #market{
             id = <<ID:HS>>,
             cid1 = <<CID1:HS>>,
             type1 = Type1,
             amount1 = Amount1,
             cid2 = <<CID2:HS>>,
             type2 = Type2,
             amount2 = Amount2,
             shares = Shares}.
make_id(X) ->
    X#market.id.
make_id(CID1, Type1, CID2, Type2) ->
    %io:fwrite("markets make id \n"),
    %io:fwrite(packer:pack([Type1, Type2, CID1, CID2])),
    %io:fwrite("\n"),
    <<N1:256>> = CID1,
    <<N2:256>> = CID2,
    if
        ((N1+Type1) =< (N2+Type2)) ->
            X = <<CID1/binary,
                  CID2/binary,
                  Type1:16,
                  Type2:16>>,
            %io:fwrite(base64:encode(X)),
            hash:doit(X);
        true ->
            make_id(CID2, Type2, CID1, Type1)
    end.

dict_write(M, Dict, Height) ->
    F42 = forks:get(42),
    if
        Height > F42 ->
            true = M#market.amount1 > 0,
            true = M#market.amount2 > 0;
        true -> ok
    end,
   dict:store({markets, M#market.id},
              %serialize(M),
              M,
              Dict).
write(X, Root) ->
    ID = X#market.id,
    M = serialize(X),
    trie:put(key_to_int(ID), M, 0, Root, markets).
key_to_int(<<X:256>>) -> X.

dict_get(Key, Dict) ->
    <<_:256>> = Key,
    X = dict:find({markets, Key}, Dict),
    case X of
        error -> error;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, Y} -> Y
%            SY = size(Y),
%            case SY of
%                124 -> trees2:deserialize(9, Y);
%                _ ->
%                    deserialize(Y)
%            end
    end.
get(ID, Markets) ->
    <<_:256>> = ID,
    {RH, Leaf, Proof} = trie:get(key_to_int(ID), Markets, markets),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
dict_delete(Key, Dict) ->      
    dict:store({markets, Key}, 0, Dict).
delete(ID,Channels) ->
    trie:delete(ID, Channels, markets).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
    
all() ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Channels = trees:markets(Trees),
    All = trie:get_all(Channels, markets),
    lists:map(
      fun(Leaf) ->
	      deserialize(leaf:value(Leaf))
      end, All).

test() ->
    A = new(<<0:256>>, 0, 1, hash:doit(1), 1, 1),
    A = deserialize(serialize(A)),
    R = trees:empty_tree(markets),
    NewLoc = write(A, R),
    ID = A#market.id,
    {Root, A, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(A), Proof),
    success.
    
