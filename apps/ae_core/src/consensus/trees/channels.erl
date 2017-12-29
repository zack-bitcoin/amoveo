-module(channels).
-export([new/7,serialize/1,deserialize/1,%update/10,
	 write/2,get/2,delete/2,root_hash/1,
	 acc1/1,acc2/1,id/1,bal1/1,bal2/1,
	 last_modified/1, 
	 nonce/1,delay/1, amount/1, slasher/1,
	 closed/1, verify_proof/4,
         dict_update/10, dict_delete/2, dict_write/2, dict_get/2,
         make_leaf/3, key_to_int/1,
	 test/0]).
%This is the part of the channel that is written onto the hard drive.

-record(channel, {id = 0, %the unique id number that identifies this channel
		  acc1 = 0, % a pubkey
		  acc2 = 0, % a different pubkey
		  bal1 = 0, %part of the money initially controlled by acc1.
		  bal2 = 0, %part of the money initially controlled by acc2.
		  amount = 0, %this is how we remember the outcome of the last contract we tested, that way we can undo it.
		  nonce = 1,%How many times has this channel-state been updated. If your partner has a state that was updated more times, then they can use it to replace your final state.
		  timeout_height = 0,%when one partner disappears, the other partner needs to wait so many blocks until they can access their money. This records the time they started waiting. 
		  last_modified = 0,%this is used to know if a channel_timeout_tx can be called yet. 
% we can set timeout_height to 0 to signify that we aren't in timeout mode. So we don't need the timeout flag.
                  %entropy
		  delay = 0,%this is the minimum of how long you have to wait since "last_modified" to do a channel_timeout_tx. 
                  %every time a channel_slash_tx happens, this delay is updated. This is how long you need to wait before you can do a channel_timeout tx.
		  slasher = 0, %If the channel was slashed, then we shouldn't allow grow_channel txs any more.
		  closed = 0 %when a channel is closed, set this to 1. The channel can no longer be modified, but the VM has access to the state it was closed on. So you can use a different channel to trustlessly pay whoever slashed.
		  }%
       ).
acc1(C) -> C#channel.acc1.
acc2(C) -> C#channel.acc2.
id(C) -> C#channel.id.
bal1(C) -> C#channel.bal1.
bal2(C) -> C#channel.bal2.
amount(C) -> C#channel.amount.
last_modified(C) -> C#channel.last_modified.
%mode(C) -> C#channel.mode.
nonce(C) -> C#channel.nonce.
delay(C) -> C#channel.delay.
slasher(C) -> C#channel.slasher.
closed(C) -> C#channel.closed.
%shares(C) -> C#channel.shares.

dict_update(Slasher, ID, Dict, Nonce, Inc1, Inc2, Amount, Delay, Height, Close) ->
    true = (Close == 1) or (Close == 0),
    true = Inc1 + Inc2 >= 0,
    Channel = dict_get(ID, Dict),
    CNonce = Channel#channel.nonce,
    NewNonce = if
		   Nonce == none -> CNonce;
		   true -> 
		       Nonce
	       end,
    T1 = Channel#channel.last_modified,
    DH = Height - T1,
    Bal1a = Channel#channel.bal1 + Inc1,% - RH,
    Bal2a = Channel#channel.bal2 + Inc2,% - RH,
    Bal1b = max(Bal1a, 0),
    Bal2b = max(Bal2a, 0),
    Bal1c = min(Bal1b, Bal1a+Bal2a),
    Bal2c = min(Bal2b, Bal1a+Bal2a),
    C = Channel#channel{bal1 = Bal1c,
                        bal2 = Bal2c,
                        amount = Amount,
                        nonce = NewNonce,
                        last_modified = Height,
                        delay = Delay,
                        slasher = Slasher,
                        closed = Close
		       },
    C.
    
new(ID, Acc1, Acc2, Bal1, Bal2, Height, Delay) ->
    #channel{id = ID, acc1 = Acc1, acc2 = Acc2, 
	     bal1 = Bal1, bal2 = Bal2, 
	     last_modified = Height, 
	     delay = Delay}.
serialize(C) ->
    %ACC = constants:address_bits(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    NON = constants:channel_nonce_bits(),
    KL = id_size(),
    CID = C#channel.id,
    Delay = constants:channel_delay_bits(),
    true = (CID - 1) < math:pow(2, KL),
    Amount = C#channel.amount,
    HB = constants:half_bal(),
    true = Amount < HB,
    true = Amount > -HB,
    HS = constants:hash_size(),
    %Shares = shares:root_hash(C#channel.shares),
    %HS = size(Shares),
    true = size(C#channel.acc1) == constants:pubkey_size(),
    true = size(C#channel.acc2) == constants:pubkey_size(),
    << CID:(HS*8),
       (C#channel.bal1):BAL,
       (C#channel.bal2):BAL,
       (Amount+HB):BAL,
       (C#channel.nonce):NON,
       (C#channel.timeout_height):HEI,
       (C#channel.last_modified):HEI,
       (C#channel.delay):Delay,
       (C#channel.closed):8,
       (C#channel.acc1)/binary,
       (C#channel.acc2)/binary
    >>.
deserialize(B) ->
    PS = constants:pubkey_size()*8,
    ACC = constants:address_bits(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    NON = constants:channel_nonce_bits(),
    KL = constants:key_length(),
    Delay = constants:channel_delay_bits(),
    HS = constants:hash_size()*8,
    << ID:HS,
       B3:BAL,
       B4:BAL,
       B8:BAL,
       B5:NON,
       B6:HEI,
       B7:HEI,
       B12:Delay,
       Closed:8,
       B1:PS,
       B2:PS
       %_:HS
    >> = B,
    #channel{id = ID, acc1 = <<B1:PS>>, acc2 = <<B2:PS>>, 
	     bal1 = B3, bal2 = B4, amount = B8-constants:half_bal(),
	     nonce = B5, timeout_height = B6, 
	     last_modified = B7,
	     delay = B12, closed = Closed}.
dict_write(Channel, Dict) ->
    ID = Channel#channel.id,
    dict:store({channels, ID},
               serialize(Channel),
               Dict).
write(Channel, Root) ->
    ID = Channel#channel.id,
    M = serialize(Channel),
    %Shares = Channel#channel.shares,
    trie:put(key_to_int(ID), M, 0, Root, channels). %returns a pointer to the new root
id_size() -> constants:key_length().
key_to_int(X) when is_integer(X) -> 
    <<Y:256>> = hash:doit(<<X:256>>),
    Y.
get(ID, Channels) ->
    true = (ID - 1) < math:pow(2, 256),
    {RH, Leaf, Proof} = trie:get(key_to_int(ID), Channels, channels),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
dict_get(Key, Dict) ->
    X = dict:fetch({channels, Key}, Dict),
    case X of
        0 -> empty;
        empty -> empty;
        _ -> deserialize(X)
    end.
dict_delete(Key, Dict) ->      
    dict:store({channels, Key}, 0, Dict).
delete(ID,Channels) ->
    trie:delete(ID, Channels, channels).
root_hash(Channels) ->
    trie:root_hash(channels, Channels).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
    
test() ->
    ID = 1,
    Acc1 = constants:master_pub(),
    Acc2 = constants:master_pub(),
    Bal1 = 200,
    Bal2 = 300,
    Height = 1,
    Delay = 11,
    A = new(ID,Acc1,Acc2,Bal1,Bal2,Height,Delay),
    A = deserialize(serialize(A)),
    C = A,
    NewLoc = write(C, constants:root0()),
    {Root, C, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(C), Proof),
    success.
    

