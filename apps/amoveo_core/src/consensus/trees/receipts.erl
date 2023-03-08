-module(receipts).
-export([new/3,
         write/2, get/2, delete/2,
         dict_delete/2, dict_write/2, dict_get/2,dict_get/3,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
         tid/1, pubkey/1, id/1,
	 all/0,
	 test/0]).
-include("../../records.hrl").

-record(receipt, {id, tid, pubkey, nonce}).

tid(R) -> R#receipt.tid.
pubkey(R) -> R#receipt.pubkey.
id(R) -> R#receipt.id.

new(T, P, Nonce) ->
    <<_:256>> = T,
    <<_:520>> = P,
    true = is_integer(Nonce),
    HEI = constants:height_bits(),
    Nonce < math:pow(2, HEI),
    R1 = #receipt{tid = T, 
                  pubkey = P,
                  nonce = Nonce},
    ID = id_maker(R1),
    R1#receipt{id = ID}.
id_maker(R) ->
    #receipt{
           tid = T,
           pubkey = P,
          nonce = N} = R,
    HEI = constants:height_bits(),
    hash:doit(<<T/binary, P/binary, N:HEI>>).
serialize(R) ->
    #receipt{
           tid = <<T:256>>,
           pubkey = <<P:520>>,
           nonce = N} = R,
    HEI = constants:height_bits(),
    <<T:256,
      P:520,
      N:HEI
    >>.
deserialize(<<T:256, P:520, N0/binary>>) ->
    HEI = constants:height_bits(),
    <<N:HEI>> = N0,
    new(<<T:256>>, <<P:520>>,  N).

dict_write(R, Dict) ->
    csc:update({receipts, {key, R#receipt.id}}, R, Dict).

dict_write_old(R, Dict) ->
    dict:store({receipts, R#receipt.id},
               %serialize(R),
               R,
               Dict).
write(R, Root) ->
    ID = R#receipt.id,
    S = serialize(R),
    trie:put(key_to_int(ID), S, 0, Root, receipts).

key_to_int({key, <<X:256>>}) ->
    key_to_int(<<X:256>>);
key_to_int(<<X:256>>) ->
    X.
dict_get(Key, Dict) ->
    dict_get(Key, Dict, 0).
dict_get(Key = {key, _}, Dict, _Height) ->
    case csc:read({receipts, Key}, Dict) of
        error -> error;
        {empty, _, _} -> empty;
        {ok, receipts, Val} -> Val
    end.
            
dict_get_old(Key, Dict) ->
    <<_:256>> = Key,
    X = dict:find({receipts, Key}, Dict),
    case X of
	error -> error;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, {receipts, Key}} -> empty;
        {ok, Y} -> Y
%            SY = size(Y),
%            case SY of
%                73 -> trees2:deserialize(10, Y);
%                _ -> deserialize(Y)
%            end
    end.
%deserialize 10
get({key, ID}, Receipts) ->
    <<_:256>> = ID,
    {RH, Leaf, Proof} = trie:get(key_to_int(ID), Receipts, receipts),
    V = case Leaf of
            empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
dict_delete(Key, Dict) ->      
    csc:remove({receipts, Key}, Dict).
    %dict:store({receipts, Key}, 0, Dict).
delete(ID,Tree) ->
    trie:delete(ID, Tree, receipts).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
all() ->
    trees:all(?MODULE).

test() ->
    A = new(hash:doit(1), keys:pubkey(), 0),
    A = deserialize(serialize(A)),
    R = trees:empty_tree(receipts),
    NewLoc = write(A, R),
    ID = id(A),
    {Root, A, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(A), Proof),
    success.
    


                 
    

