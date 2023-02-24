-module(trades).
-export([new/2,
         height/1, value/1,
         write/2, get/2, delete/2,
         dict_delete/2, dict_write/2, dict_get/2, dict_get/3, %update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
	 all/0,
	 test/0]).

-include("../../records.hrl").

height(T) -> T#trade.height.
value(T) -> T#trade.value.

new(H, V) ->
    <<_:256>> = V,
    true = is_integer(H),
    true = H>0,
    #trade{height = H, value = V}.
serialize(T) ->
    #trade{
           value = V,
           height = H
          } = T,
    HEI = constants:height_bits(),
    HS = constants:hash_size(),
    32 = size(V),
    true = H < math:pow(2, HEI),
    <<
      V/binary,
      H:HEI
    >>.
deserialize(<<V:256, R/binary>>) ->    
    HEI = constants:height_bits(),
    <<H:HEI>> = R,
    #trade{
            value = <<V:256>>,
            height = H}.

dict_write(T, Dict) ->
    csc:update({trades, {key, T#trade.value}}, T, Dict).

dict_write_old(T, Dict) ->
    dict:store({trades, T#trade.value},
               %serialize(T),
               T,
               Dict).
write(T, Root) ->
    ID = T#trade.value,
    M = serialize(T),
    trie:put(key_to_int(ID), M, 0, Root, trades).
%returns a pointer to the new root.

key_to_int(<<X:256>>) ->
    X.

dict_get(Key, Dict, _Height) ->
    dict_get(Key, Dict).
dict_get(Key = {key, _}, Dict) ->
    case csc:read({trades, Key}, Dict) of
        error -> error;
        {empty, _} -> empty;
        {ok, trades, Val} -> Val
    end.
            

dict_get_old(Key, Dict) ->
    <<_:256>> = Key,
    X = dict:find({trades, Key}, Dict),
    case X of
	error -> error;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, {trades, Key}} -> empty;
        {ok, Y} -> Y
%            SY = size(Y),
%            case SY of
%                36 -> trees2:deserialize(8, Y);
%                _ ->
%                    deserialize(Y)
%            end
    end.
get({key, ID}, Channels) ->
    <<_:256>> = ID,
    {RH, Leaf, Proof} = trie:get(key_to_int(ID), Channels, trades),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
dict_delete(Key, Dict) ->      
    dict:store({trades, Key}, 0, Dict).
delete(ID,Tree) ->
    trie:delete(ID, Tree, trade).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
    
all() ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Channels = trees:trads(Trees),
    All = trie:get_all(Channels, trades),
    lists:map(
      fun(Leaf) ->
	      deserialize(leaf:value(Leaf))
      end, All).

test() ->
    A = new(5, hash:doit(1)),
    A = deserialize(serialize(A)),
    R = trees:empty_tree(contracts),
    NewLoc = write(A, R),
    ID = value(A),
    {Root, A, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(A), Proof),
    success.
    
    
