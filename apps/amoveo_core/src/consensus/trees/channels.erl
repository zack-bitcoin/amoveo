-module(channels).
-export([new/7, acc1/1, acc2/1, id/1, bal1/1, bal2/1, last_modified/1, nonce/1, delay/1, amount/1, closed/1, %custom for this tree
	 write/2, get/2, delete/2,%update tree stuff
         dict_update/9, dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
	 all/0, close_many/0,
	 test/0]).%common tree stuff
%This is the part of the channel that is written onto the hard drive.

-include("../../records.hrl").
-record(channel, {id = 0, %the unique id number that identifies this channel
		  acc1 = 0, % a pubkey
		  acc2 = 0, % a different pubkey
		  bal1 = 0, %part of the money initially controlled by acc1.
		  bal2 = 0, %part of the money initially controlled by acc2.
		  amount = 0, %this is how we remember the outcome of the last contract we tested, that way we can undo it.
		  nonce = 1,%How many times has this channel-state been updated. If your partner has a state that was updated more times, then they can use it to replace your final state.
		  last_modified = 0,%this is used to know if a channel_timeout_tx can be called yet. 
		  delay = 0,%this is the minimum of how long you have to wait since "last_modified" to do a channel_timeout_tx. 
                  %every time a channel_slash_tx happens, this delay is updated. This is how long you need to wait before you can do a channel_timeout tx.
		  closed = 0 %when a channel is closed, set this to 1. The channel can no longer be modified, but the VM has access to the state it was closed on. So you can use a different channel to trustlessly pay whoever slashed.
		  %channels closed flag is unused because we delete closed channels.
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
closed(C) -> C#channel.closed.
%shares(C) -> C#channel.shares.

dict_update(ID, Dict, Nonce, Inc1, Inc2, Amount, Delay, Height, Close0) ->
    Close = case Close0 of 
                1 -> 1;
                0 -> 0;
                true -> 1;
                false -> 0
            end,
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
                        closed = Close
		       },
    %io:fwrite(packer:pack(C)),
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
    CID = C#channel.id,
    Delay = constants:channel_delay_bits(),
    %<<CID2:256>> = <<CID:256>>, 
    %CID2 = CID,
    Amount = C#channel.amount,
    HB = constants:half_bal(),
    true = Amount < HB,
    true = Amount > -HB,
    HS = constants:hash_size(),
    %Shares = shares:root_hash(C#channel.shares),
    %HS = size(Shares),
    true = size(C#channel.acc1) == constants:pubkey_size(),
    true = size(C#channel.acc2) == constants:pubkey_size(),
    <<_:256>> = CID,
    << CID/binary,
       (C#channel.bal1):BAL,
       (C#channel.bal2):BAL,
       (Amount+HB):BAL,
       (C#channel.nonce):NON,
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
    Delay = constants:channel_delay_bits(),
    HS = constants:hash_size()*8,
    << ID:HS,
       B3:BAL,
       B4:BAL,
       B8:BAL,
       B5:NON,
       B7:HEI,
       B12:Delay,
       Closed:8,
       B1:PS,
       B2:PS
       %_:HS
    >> = B,
    #channel{id = <<ID:HS>>, acc1 = <<B1:PS>>, acc2 = <<B2:PS>>, 
	     bal1 = B3, bal2 = B4, amount = B8-constants:half_bal(),
	     nonce = B5, 
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
key_to_int(X) -> 
    <<_:256>> = X,
    <<Y:256>> = hash:doit(X),
    Y.
dict_get(Key, Dict) ->
    <<_:256>> = Key,
    X = dict:find({channels, Key}, Dict),
    case X of
	error -> empty;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, Y} -> deserialize(Y)
    end.
get(ID, Channels) ->
    <<_:256>> = ID,
    {RH, Leaf, Proof} = trie:get(key_to_int(ID), Channels, channels),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
dict_delete(Key, Dict) ->      
    dict:store({channels, Key}, 0, Dict).
delete(ID,Channels) ->
    trie:delete(ID, Channels, channels).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).

%function to look up all open channels.
all() ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Channels = trees:channels(Trees),
    All = trie:get_all(Channels, channels),
    lists:map(
      fun(Leaf) ->
	      channels:deserialize(leaf:value(Leaf))
      end, All).
close_many() ->
    %if you have already solo-closed or slashed some channels, and you have waited long enough for those channels to be closed, this is how you can close them.
    A = all(),
    K = keys:pubkey(),
    H = block:height(),
    {ok, Fee} = application:get_env(amoveo_core, minimum_tx_fee),
    close_many2(A, K, H, Fee+1).

close_many2([], _, _, _) -> ok;
close_many2([A|T], K, H, Fee) ->
    A2 = A#channel.acc2,
    H2 = A#channel.last_modified + A#channel.delay,
    if
	(not (A2 == K)) -> ok; %only close the ones that are opened with the server, 
	H2 < H -> ok; %only close the ones that have waited long enough to be closed, 
	true ->
	    Tx = channel_timeout_tx:make_dict(A#channel.acc1, A#channel.id, Fee),
	    Stx = keys:sign(Tx),
	    tx_pool_feeder:absorb_async(Stx)
    end,
    close_many2(T, K, H, Fee).

%keep the ones that are opened with the server, 
    %B = lists:filter(fun(C) -> C#channel.acc2 == K end, A),
%keep the ones that have waited long enough to be closed, 
    %C = lists:filter(fun(C) -> (C#channel.last_modified + C#channel.delay) > H end, B),
%then make a channel_timeout_tx for them all.
    %lists:map(fun(C) -> tx_pool_feeder:absorb_async(keys:sign(channel_timeout_tx(C#channel.acc1, C#channel.id, Fee))) end, C).
    
    
test() ->
    ID = <<1:256>>,
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
    

