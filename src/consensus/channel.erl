-module(channel).
-export([new/9,serialize/1,deserialize/1,update/9,
	 write/2,get/2,delete/2,root_hash/1,
	 acc1/1,acc2/1,id/1,bal1/1,bal2/1,
	 last_modified/1, mode/1,entropy/1,
	 nonce/1,delay/1,rent/1,rent_direction/1,
	 test/0]).
%This is the part of the channel that is written onto the hard drive.

-record(channel, {id = 0, %the unique id number that identifies this channel
		  acc1 = 0, 
		  acc2 = 0, 
		  bal1 = 0, 
		  bal2 = 0, 
		  nonce = 0,%How many times has this channel-state been updated. If your partner has a state that was updated more times, then they can use it to replace your final state.
		  timeout_height = 0,%when one partner disappears, the other partner needs to wait so many blocks until they can access their money. This records the time they started waiting. 
		  last_modified = 0,%this is used so that the owners of the channel can pay a fee for how long the channel has been open.
		  rent = 0,
		  rent_direction = 0,%0 or 1
% we can set timeout_height to 0 to signify that we aren't in timeout mode. So we don't need the timeout flag.
		  mode = 0,%0 means an active channel where money can be spent. 1 means that the channel is being closed by acc1. 2 means that the channel is being closed by acc2.
		  entropy = 0, %This is a nonce so that old channel contracts can't be reused, even if you make a new channel with the same partner you previously had a channel with.
		  delay = 0%this is how long you have to wait since "last_modified" to do a channel_timeout_tx.
		  }%
       ).
acc1(C) -> C#channel.acc1.
acc2(C) -> C#channel.acc2.
id(C) -> C#channel.id.
bal1(C) -> C#channel.bal1.
bal2(C) -> C#channel.bal2.
last_modified(C) -> C#channel.last_modified.
mode(C) -> C#channel.mode.
entropy(C) -> C#channel.entropy.
nonce(C) -> C#channel.nonce.
delay(C) -> C#channel.delay.
rent(C) -> C#channel.rent.
rent_direction(C) -> C#channel.rent_direction.


update(ID, Channels, Nonce, NewRent,Inc1, Inc2, Mode, Delay, Height) ->
    true = Inc1 + Inc2 >= 0,
    {_, Channel, _} = get(ID, Channels),
    CNonce = Channel#channel.nonce,
    NewNonce = if
		   Nonce == none -> CNonce;
		   true -> 
		       true = Nonce > CNonce,
		       Nonce
	       end,
    %true = Nonce > Channel#channel.nonce,
    T1 = Channel#channel.last_modified,
    DH = Height - T1,
    Rent = constants:channel_rent() * DH,
    RH = Rent div 2,%everyone needs to pay the network for the cost of having a channel open.
    S = case Channel#channel.rent_direction of
	0 -> -1;
	1 -> 1
    end,
    NewRD = if
		NewRent > 0 -> 1;
		true -> 0
	    end,
			    
    CR = S * Channel#channel.rent,
    Bal1a = Channel#channel.bal1 + Inc1 - RH + CR,
    Bal2a = Channel#channel.bal2 + Inc2 - RH - CR,
    Bal1b = max(Bal1a, 0),
    Bal2b = max(Bal2a, 0),
    Bal1c = min(Bal1b, Bal1a+Bal2a),
    Bal2c = min(Bal2b, Bal1a+Bal2a),
    %true = Bal1 >= 0,
    %true = Bal2 >= 0,
    true = lists:any(fun(X) -> X==Mode end, [0,1,2]),
    Channel#channel{bal1 = Bal1c,
		    bal2 = Bal2c,
		    nonce = NewNonce,
		    rent = NewRent,
		    rent_direction = NewRD,
		    last_modified = Height,
		    delay = Delay,
		    mode = Mode
		   }.
    
new(ID, Acc1, Acc2, Bal1, Bal2, Height, Entropy, Rent, Delay) ->
    D = if
	Rent > 0 -> 1;
	true -> 0
    end,
    #channel{id = ID, acc1 = Acc1, acc2 = Acc2, 
	     bal1 = Bal1, bal2 = Bal2, 
	     last_modified = Height, entropy = Entropy,
	     rent = abs(Rent), rent_direction = D,
	     delay = Delay}.
serialize(C) ->
    ACC = constants:acc_bits(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    NON = constants:channel_nonce_bits(),
    Rent = constants:channel_rent_bits(),
    Pad = constants:channel_padding(),
    KL = id_size(),
    ENT = constants:channel_entropy(),
    CID = C#channel.id,
    Entropy = C#channel.entropy,
    true = (Entropy - 1) < math:pow(2, ENT),
    true = (CID - 1) < math:pow(2, KL),
    << CID:KL,
       (C#channel.acc1):ACC,
       (C#channel.acc2):ACC,
       (C#channel.bal1):BAL,
       (C#channel.bal2):BAL,
       (C#channel.nonce):NON,
       (C#channel.timeout_height):HEI,
       (C#channel.last_modified):HEI,
       (C#channel.rent):Rent,
       (C#channel.rent_direction):1,
       (C#channel.mode):2,
       Entropy:ENT,
       0:Pad>>.
deserialize(B) ->
    ACC = constants:acc_bits(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    NON = constants:channel_nonce_bits(),
    Rent = constants:channel_rent_bits(),
    Pad = constants:channel_padding(),
    KL = constants:key_length(),
    ENT = constants:channel_entropy(),
    << ID:KL,
       B1:ACC,
       B2:ACC,
       B3:BAL,
       B4:BAL,
       B5:NON,
       B6:HEI,
       B7:HEI,
       B8:Rent,
       B9:1,
       B10:2,
       B11:ENT,
       _:Pad>> = B,
    #channel{id = ID, acc1 = B1, acc2 = B2, 
	     bal1 = B3, bal2 = B4,
	     nonce = B5, timeout_height = B6, 
	     last_modified = B7,
	     rent = B8, rent_direction = B9,
	     mode = B10, entropy = B11}.
write(Channel, Root) ->
    ID = Channel#channel.id,
    M = serialize(Channel),
    trie:put(ID, M, Root, channels). %returns a pointer to the new root
id_size() -> constants:key_length().
get(ID, Channels) ->
    true = (ID - 1) < math:pow(2, id_size()),
    {RH, Leaf, Proof} = trie:get(ID, Channels, channels),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
delete(ID,Channels) ->
    trie:delete(ID, Channels, channels).
root_hash(Channels) ->
    trie:root_hash(channels, Channels).
    
test() ->
    ID = 1,
    Acc1 = 1,
    Acc2 = 2,
    Bal1 = 200,
    Bal2 = 300,
    Height = 1,
    Rent = -4,
    Entropy = 500,
    Delay = 11,
    C = new(ID,Acc1,Acc2,Bal1,Bal2,Height,Entropy,Rent, Delay),
    C = deserialize(serialize(C)),
    NewLoc = write(C, 0),
    {_, C, _} = get(ID, NewLoc),
    success.
    

