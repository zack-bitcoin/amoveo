-module(channel).
-export([new_channel/5,serialize/1,deserialize/1,update_channel/7,test/0]).
%This is the part of the channel that is written onto the hard drive.

-record(channel, {id = 0, %the unique id number that identifies this channel
		  acc1 = 0, 
		  acc2 = 0, 
		  bal1 = 0, 
		  bal2 = 0, 
		  nonce = 0,%How many times has this channel-state been updated. If your partner has a state that was updated more times, then they can use it to replace your final state.
		  rent = 0,
		  rent_direction = 0,%0 or 1
		  timeout_height = 0,%when one partner disappears, the other partner needs to wait so many blocks until they can access their money. This records the time they started waiting. 
% we can set timeout_height to 0 to signify that we aren't in timeout mode. So we don't need the timeout flag.
		  last_modified = 0%this is used so that the owners of the channel can pay a fee for how long the channel has been open.
		  }%
       ).

update_channel(Channel, Nonce, NewRent, RentDirection, Inc1, Inc2, Height) ->
    true = Nonce > Channel#channel.nonce,
    T1 = Channel#channel.last_modified,
    DH = Height - T1,
    S = case Channel#channel.rent_direction of
	0 -> -1;
	1 -> 1
    end,
    CR = S * Channel#channel.rent,
    Rent = constants:channel_rent() * DH,
    RH = Rent div 2,
    Bal1 = Channel#channel.bal1 + Inc1 - RH + CR,
    Bal2 = Channel#channel.bal2 + Inc2 - RH - CR,
    Channel#channel{bal1 = Bal1,
	      bal2 = Bal2,
	      nonce = Nonce,
	      rent = NewRent,
	      rent_direction = RentDirection,
	      last_modified = Height
	     }.
    
test() ->
    C = new_channel(0,1,2,3,-4),
    C = deserialize(serialize(C)).
new_channel(Acc1, Acc2, Bal1, Bal2, Rent) ->
    RS = if
	     (Rent > 0) -> 0;
	     true -> 1
	 end,
    #channel{acc1 = Acc1, acc2 = Acc2, bal1 = Bal1, bal2 = Bal2, rent = abs(Rent), rent_direction = RS}.
serialize(C) ->
    ACC = constants:acc_bits(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    NON = constants:channel_nonce_bits(),
    Rent = constants:channel_rent_bits(),
    Pad = constants:channel_padding(),
    << (C#channel.acc1):ACC,
       (C#channel.acc2):ACC,
       (C#channel.bal1):BAL,
       (C#channel.bal2):BAL,
       (C#channel.nonce):NON,
       (C#channel.timeout_height):HEI,
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
    << B1:ACC,
       B2:ACC,
       B3:BAL,
       B4:BAL,
       B5:NON,
       B6:HEI,
       B7:Rent,
       B8:1,
       _:Pad>> = B,
    #channel{acc1 = B1, acc2 = B2, bal1 = B3, bal2 = B4,
	     nonce = B5, timeout_height = B6,
	     rent = B7, rent_direction = B8}.
    
    
    
