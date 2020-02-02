-module(sortition_blocks).
-export([new/5,
	 write/2, get/2, delete/2,%update tree stuff
         %dict_update/1, 
         dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
	 all/0,
         test/0
]).
-define(id, ?MODULE).
-include("../../records.hrl").
%-record(sortition_block, {id, validators, sid, height, state_root}).

new(ID, V, H, SH, SR) ->
    ID = hash:doit([SH, V]),
    #sortition_block{
     id = ID,
     validators = V,
     height = H,
     side_height = SH,
     state_root = SR
    }.

id(X) -> X#sortition_block.id.
validators(X) -> X#sortition_block.validators.
height(X) -> X#sortition_block.height.
side_height(X) -> X#sortition_block.side_height.
state_root(X) -> X#sortition_block.state_root.
    
key_to_int(X) -> 
    <<_:256>> = X,
    <<Y:256>> = hash:doit(X),
    Y.
get(Key, Sortition) ->
    ID = key_to_int(Key),
    {RH, Leaf, Proof} = trie:get(ID, Sortition, ?id),
    S = case Leaf of
            empty -> empty;
            Leaf ->
                deserialize(leaf:value(Leaf))
        end,
    {RH, S, Proof}.

delete(Key, Sortition) ->
    ID = key_to_int(Key),
    trie:delete(ID, Sortition, ?id).

dict_delete(Key, Dict) ->
    dict:store({?MODULE, Key}, 0, Dict).
dict_write(S, Dict) ->
    K = id(S),
    dict:store({?MODULE, K},
               serialize(S), 
               Dict).
write(S, Root) ->
    Key = id(S),
    SS = serialize(S),
    ID = key_to_int(Key),
    trie:put(ID, SS, 0, Root, ?id).
dict_get(Key, Dict) ->
    case dict:find({?MODULE, Key}, Dict) of
	error -> empty;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, Y} -> deserialize(Y)
    end.

verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).

deserialize(B) ->
    HS = constants:hash_size()*8,
    PS = constants:pubkey_size()*8,
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    <<
      ID:256,
      Validators:256,
      StateRoot:256,
      SideHeight:HEI,
      Height:HEI
    >> = B,
    #sortition_block{
           id = <<ID:256>>,
           validators = <<Validators:256>>,
           state_root = <<StateRoot:256>>,
           side_height = SideHeight,
           height = Height
          }.
serialize(S) ->
    HS = constants:hash_size(),
    PS = constants:pubkey_size(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    #sortition_block{
                       id = ID,
                       validators = V,
                       state_root = SR,
                       side_height = SH,
                       height = H
                     } = S,
    HS = size(ID),
    HS = size(V),
    HS = size(SR),
    <<
      ID/binary,
      V/binary,
      SR/binary,
      SH:HEI,
      H:HEI
    >>.

all() ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Sortition = trees:?MODULE(Trees),
    All = trie:get_all(Sortition, ?MODULE),
    lists:map(fun(X) ->
                      deserialize(leaf:value(X))
              end, All).

test() ->
    {Pub, _Priv} = testnet_sign:new_key(),
    ID = hash:doit(1),
    S = new(ID, 
            hash:doit(2),
            hash:doit(3),
            10,
            hash:doit(4)),
    S = deserialize(serialize(S)),
    Root0 = trees:empty_tree(?MODULE),
    NewLoc = write(S, Root0),
    {Root, S, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(S), Proof),
    {Root2, empty, Proof2} = get(ID, Root0),
    true = verify_proof(Root2, ID, 0, Proof2),
    success.
