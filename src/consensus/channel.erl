-module(channel).
-export([new/7,serialize/1,deserialize/1,update/7,write/2,get/2,delete/2,root_hash/1,acc1/1,acc2/1,id/1,bal1/1,bal2/1,last_modified/1, test/0]).
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
		  rent_direction = 0%0 or 1
% we can set timeout_height to 0 to signify that we aren't in timeout mode. So we don't need the timeout flag.
		  }%
       ).
acc1(C) -> C#channel.acc1.
acc2(C) -> C#channel.acc2.
id(C) -> C#channel.id.
bal1(C) -> C#channel.bal1.
bal2(C) -> C#channel.bal2.
last_modified(C) -> C#channel.last_modified.


update(ID, Channels, Nonce, NewRent,Inc1, Inc2, Height) ->
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
    RH = Rent div 2,
    S = case Channel#channel.rent_direction of
	0 -> -1;
	1 -> 1
    end,
    NewRD = if
		NewRent > 0 -> 1;
		true -> 0
	    end,
			    
    CR = S * Channel#channel.rent,
    Bal1 = Channel#channel.bal1 + Inc1 - RH + CR,
    Bal2 = Channel#channel.bal2 + Inc2 - RH - CR,
    Channel#channel{bal1 = Bal1,
	      bal2 = Bal2,
	      nonce = NewNonce,
	      rent = NewRent,
	      rent_direction = NewRD,
	      last_modified = Height
	     }.
    
new(ID, Acc1, Acc2, Bal1, Bal2, Height, Rent) ->
    RS = if
	     (Rent > 0) -> 0;
	     true -> 1
	 end,
    #channel{id = ID, acc1 = Acc1, acc2 = Acc2, bal1 = Bal1, bal2 = Bal2, last_modified = Height, rent = abs(Rent), rent_direction = RS}.
serialize(C) ->
    ACC = constants:acc_bits(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    NON = constants:channel_nonce_bits(),
    Rent = constants:channel_rent_bits(),
    Pad = constants:channel_padding(),
    KL = constants:key_length(),
    << (C#channel.id):KL,
       (C#channel.acc1):ACC,
       (C#channel.acc2):ACC,
       (C#channel.bal1):BAL,
       (C#channel.bal2):BAL,
       (C#channel.nonce):NON,
       (C#channel.timeout_height):HEI,
       (C#channel.last_modified):HEI,
       (C#channel.rent):Rent,
       (C#channel.rent_direction):1,
       0:Pad>>.
deserialize(B) ->
    ACC = constants:acc_bits(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    NON = constants:channel_nonce_bits(),
    Rent = constants:channel_rent_bits(),
    Pad = constants:channel_padding(),
    KL = constants:key_length(),
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
       _:Pad>> = B,
    #channel{id = ID, acc1 = B1, acc2 = B2, 
	     bal1 = B3, bal2 = B4,
	     nonce = B5, timeout_height = B6, 
	     last_modified = B7,
	     rent = B8, rent_direction = B9}.
write(Channel, Root) ->
    ID = Channel#channel.id,
    M = serialize(Channel),
    trie:put(ID, M, Root, channels). %returns a pointer to the new root
get(ID, Channels) ->
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
    C = new(ID,Acc1,Acc2,Bal1,Bal2,Height,Rent),
    C = deserialize(serialize(C)),
    NewLoc = write(C, 0),
    {_, C, _} = get(ID, NewLoc),
    success.
    

