-module(sortition).
-export([new/8,
         id/1, amount/1, entropy_source/1, creator/1, trading_ends/1, rng_response_delay/1, rng_end/1, rng_value/1, last_modified/1, top_candidate/1, closed/1,
	 write/2, get/2, delete/2,%update tree stuff
         dict_update/3, dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
	 all/0,
         test/0
]).
-define(id, sortition).
-include("../../records.hrl").

new(K, Amount, Entropy, Creator, TE, RRD, RE, Delay) ->
    %delay is how much time you get to provide counter-evidence.
    #sortition{id = K, %256
               amount = Amount,
               entropy_source = Entropy,
               creator = Creator, 
               trading_ends = TE, 
               rng_response_delay = RRD,
               rng_end = RE,
               rng_value = <<0:256>>,
               %expiration = Expiration, %height
               delay = Delay, %height
               last_modified = 0,%height
               top_candidate = 0,%256
               closed = 0}.%1 bit

id(S) -> S#sortition.id.
amount(S) -> S#sortition.amount.
entropy_source(S) -> S#sortition.entropy_source.
creator(S) -> S#sortition.creator.
%expiration(S) -> S#sortition.expiration.
trading_ends(S) -> S#sortition.trading_ends.
rng_response_delay(S) -> S#sortition.rng_response_delay.
rng_end(S) -> S#sortition.rng_end.
rng_value(S) -> S#sortition.rng_value.
delay(S) -> S#sortition.delay.
last_modified(S) -> S#sortition.last_modified.
top_candidate(S) -> S#sortition.top_candidate.
closed(S) -> S#sortition.closed.

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
    

dict_update(S, Height, Close) ->
    case Close of 
        1 -> ok;
        0 -> ok
    end,
    S#sortition{last_modified = Height,
                closed = Close}.

dict_delete(Key, Dict) ->
    dict:store({sortition, Key}, 0, Dict).

dict_write(S, Dict) ->
    K = id(S),
    dict:store({sortition, K},
               serialize(S), 
               Dict).

write(S, Root) ->
    Key = S#sortition.id,
    SS = serialize(S),
    ID = key_to_int(Key),
    trie:put(ID, SS, 0, Root, ?id).

dict_get(Key, Dict) ->
    case dict:find({sortition, Key}, Dict) of
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
    <<ID:HS,
      Amount:BAL,
      ES:HEI,
      Creator:PS,
      TE:HEI,
      RRD:HEI,
      RE:HEI,
      Value:HS,
      Delay:HEI,
      LM:HEI,
      TC:HS,
      Closed:8>> = B,
    #sortition{
                id = <<ID:HS>>,
                amount = Amount,
                entropy_source = ES,
                creator = <<Creator:PS>>,
                trading_ends = TE,
                rng_response_delay = RRD,
                rng_end = RE,
                rng_value = <<Value:HS>>,
                %expiration = Expiration,
                delay = Delay,
                last_modified = LM,
                top_candidate = <<TC:HS>>,
                closed = Closed
              }.

serialize(S) ->
    HS = constants:hash_size(),
    PS = constants:pubkey_size(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    Creator = S#sortition.creator,
    Value = S#sortition.rng_value,
    ID = S#sortition.id,
    PS = size(Creator),
    HS = size(ID),
    HS = size(Value),
    <<ID/binary,
      (S#sortition.amount):BAL,
      (S#sortition.entropy_source):HEI,
      Creator/binary,
      %(S#sortition.expiration):HEI,
      (S#sortition.trading_ends):HEI,
      (S#sortition.rng_response_delay):HEI,
      (S#sortition.rng_end):HEI,
      Value/binary,
      (S#sortition.delay):HEI,
      (S#sortition.last_modified):HEI,
      (S#sortition.top_candidate):HS,
      (S#sortition.closed):8>>.

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
