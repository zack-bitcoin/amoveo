-module(candidates).
-export([new/6,
	 write/2, get/2, delete/2,%update tree stuff
         dict_update/2, dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
	 all/0,
         id/1, sortition_id/1, layer_number/1, winner/1, height/1, next_candidate/1,
         test/0
]).
-define(id, candidates).
-include("../../records.hrl").
%-record(candidate, {sortition_id, layer_number, winner_pubkey, height, next_candidate}).


new(ID, SID, N, WP, H, NC) ->
    #candidate{
     id = ID,
     sortition_id = SID,
     layer_number = N,
     winner = WP,
     height = H,
     next_candidate = NC
    }.

id(C) -> C#candidate.id.
sortition_id(C) -> C#candidate.sortition_id.
layer_number(C) -> C#candidate.layer_number.
winner(C) -> C#candidate.winner.
height(C) -> C#candidate.height.
next_candidate(C) -> C#candidate.next_candidate. 

write(C, Root) ->
    Key = C#candidate.id,
    SC = serialize(C),
    ID = key_to_int(Key),
    trie:put(ID, SC, 0, Root, ?id).

get(Key, Candidate) ->
    ID = key_to_int(Key),
    {RH, Leaf, Proof} = trie:get(ID, Candidate, ?id),
    S = case Leaf of
            empty -> empty;
            Leaf ->
                deserialize(leaf:value(Leaf))
        end,
    {RH, S, Proof}.
    

delete(Key, Sortition) ->
    ID = key_to_int(Key),
    trie:delete(ID, Sortition, ?id).

dict_update(C, NC) ->
    C#candidate{
      next_candidate = NC
     }.

dict_delete(Key, Dict) ->
    dict:store({candidate, Key}, 0, Dict).

dict_write(C, Dict) ->
    K = id(C),
    dict:store({candidate, K},
               serialize(C), 
               Dict).

dict_get(Key, Dict) ->
    case dict:find({candidate, Key}, Dict) of
	error -> empty;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, Y} -> deserialize(Y)
    end.

verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).

make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).

key_to_int(X) -> 
    <<_:256>> = X,
    <<Y:256>> = hash:doit(X),
    Y.

deserialize(C) ->
%-record(candidate, {id, sortition_id, layer_number, winner_pubkey, height, next_candidate}).%merkle tree
    HS = constants:hash_size(),
    PS = constants:pubkey_size(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    ID = C#candidate.id,
    HS = size(ID),
    SI = C#candidate.sortition_id,
    HS = size(SI),
    Winner = C#candidate.winner,
    PS = size(Winner),
    NC = C#candidate.next_candidate,
    HS = size(NC),
    <<ID/binary,
      SI/binary,
      (C#candidate.layer_number):16,
      Winner/binary,
      (C#candidate.height):HEI,
      NC/binary>>.
      
serialize(B) ->
    HS = constants:hash_size()*8,
    PS = constants:pubkey_size()*8,
    HEI = constants:height_bits(),
    <<
      ID:HS,
      SI:HS,
      LN:16,
      Winner:PS,
      Height:HEI,
      NC:HS
    >> = B,
    #candidate{
           id = <<ID:HS>>,
           sortition_id = <<SI:HS>>,
           layer_number = LN,
           winner = <<Winner:PS>>,
           next_candidate = <<NC:HS>>
          }.

all() ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Sortition = trees:sortition(Trees),
    All = trie:get_all(Sortition, sortition),
    lists:map(fun(X) ->
                      sortition:deserialize(leaf:value(X))
              end, All).

test() ->
    %make a new.
    %serialize deserialize
    %make an empty tree.
    %write to the tree.
    %verify a proof.
    success.
