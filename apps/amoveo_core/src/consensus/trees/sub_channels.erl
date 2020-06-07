-module(sub_channels).
-export([new/6, accounts/1, id/1, last_modified/1, nonce/1, delay/1, amount/1, closed/1, contract_id/1, type/1,%custom for this tree
	 write/2, get/2, delete/2,%update tree stuff
         dict_update/6, dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
	 all/0, %close_many/0,
	 test/0]).%common tree stuff
%This is the part of the channel that is written onto the hard drive.

-include("../../records.hrl").


accounts(C) -> C#sub_channel.accounts.
id(C) -> C#sub_channel.id.
amount(C) -> C#sub_channel.amount.
last_modified(C) -> C#sub_channel.last_modified.
%mode(C) -> C#sub_channel.mode.
nonce(C) -> C#sub_channel.nonce.
delay(C) -> C#sub_channel.delay.
closed(C) -> C#sub_channel.closed.
contract_id(C) -> C#sub_channel.contract_id.
type(C) -> C#sub_channel.type.
                  
%shares(C) -> C#sub_channel.shares.

dict_update(ID, Dict, Nonce, Delay, Height, Close0) ->
    Close = case Close0 of 
                1 -> 1;
                0 -> 0;
                true -> 1;
                false -> 0
            end,
    true = (Close == 1) or (Close == 0),
    Channel = dict_get(ID, Dict),
    CNonce = Channel#sub_channel.nonce,
    NewNonce = if
		   Nonce == none -> CNonce;
		   true -> 
		       Nonce
	       end,
    T1 = Channel#sub_channel.last_modified,
    DH = Height - T1,
    Channel#sub_channel{
      nonce = NewNonce,
      last_modified = Height,
      delay = Delay,
      closed = Close
     }.
    
new(ID, CID, Type, Accs, Height, Delay) ->
    %maybe we should hash the accounts together here?
    AH = hash_accounts(Accs),
    #sub_channel{
     id = ID, accounts = AH,
     last_modified = Height, 
     delay = Delay, contract_id = CID,
     type = Type}.
hash_accounts(Accs) ->
    hash:doit(Accs).
serialize(C) ->
    %ACC = constants:address_bits(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    NON = constants:channel_nonce_bits(),
    CID = C#sub_channel.id,
    Delay = constants:channel_delay_bits(),
    %<<CID2:256>> = <<CID:256>>, 
    %CID2 = CID,
    HS = constants:hash_size(),
    %Shares = shares:root_hash(C#sub_channel.shares),
    %HS = size(Shares),
    Accs = C#sub_channel.accounts,
    ContractID = C#sub_channel.contract_id,
    Amount = C#sub_channel.amount,
    true = size(Accs) == HS,
    true = size(CID) == HS,
    <<_:256>> = CID,
    << CID/binary,
       Accs/binary,
       Amount:BAL,
       (C#sub_channel.nonce):NON,
       (C#sub_channel.last_modified):HEI,
       (C#sub_channel.delay):Delay,
       (C#sub_channel.closed):8,
       ContractID/binary,
       (C#sub_channel.type):16
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
       Accs:HS,
       Amount:BAL,
       B5:NON,
       B7:HEI,
       B12:Delay,
       Closed:8,
       ContractID:HS,
       Type:16
    >> = B,
    #sub_channel{id = <<ID:HS>>, accounts = <<Accs:HS>>, 
                 amount = Amount,
                 nonce = B5, 
                 last_modified = B7,
                 delay = B12, closed = Closed,
                 contract_id = <<ContractID:HS>>,
                 type = Type}.
dict_write(Channel, Dict) ->
    ID = Channel#sub_channel.id,
    dict:store({sub_channels, ID},
               serialize(Channel),
               Dict).
write(Channel, Root) ->
    ID = Channel#sub_channel.id,
    M = serialize(Channel),
    %Shares = Channel#sub_channel.shares,
    trie:put(key_to_int(ID), M, 0, Root, sub_channels). %returns a pointer to the new root
key_to_int(X) -> 
    <<_:256>> = X,
    <<Y:256>> = hash:doit(X),
    Y.
dict_get(Key, Dict) ->
    <<_:256>> = Key,
    X = dict:find({sub_channels, Key}, Dict),
    case X of
	%error -> error;
	error -> empty;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, Y} -> deserialize(Y)
    end.
get(ID, Channels) ->
    <<_:256>> = ID,
    {RH, Leaf, Proof} = trie:get(key_to_int(ID), Channels, sub_channels),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
dict_delete(Key, Dict) ->      
    dict:store({sub_channels, Key}, 0, Dict).
delete(ID,Channels) ->
    trie:delete(ID, Channels, sub_channels).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).

%function to look up all open channels.
all() ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Channels = trees:channels(Trees),
    All = trie:get_all(Channels, sub_channels),
    lists:map(
      fun(Leaf) ->
	      channels:deserialize(leaf:value(Leaf))
      end, All).
%close_many() ->
    %if you have already solo-closed or slashed some channels, and you have waited long enough for those channels to be closed, this is how you can close them.
%    A = all(),
%    K = keys:pubkey(),
%    H = block:height(),
%    {ok, Fee} = application:get_env(amoveo_core, minimum_tx_fee),
%    close_many2(A, K, H, Fee+1).

%close_many2([], _, _, _) -> ok;
%close_many2([A|T], K, H, Fee) ->
    %A2 = A#sub_channel.acc2,
%    H2 = A#sub_channel.last_modified + A#sub_channel.delay,
%    if
	%(not (A2 == K)) -> ok; %only close the ones that are opened with the server, 
%	H2 < H -> ok; %only close the ones that have waited long enough to be closed, 
%	true ->
%	    Tx = channel_timeout_tx:make_dict(keys:pubkey(), A#sub_channel.id, Fee),
%	    Stx = keys:sign(Tx),
%	    tx_pool_feeder:absorb_async(Stx)
%    end,
%    close_many2(T, K, H, Fee).

%keep the ones that are opened with the server, 
    %B = lists:filter(fun(C) -> C#sub_channel.acc2 == K end, A),
%keep the ones that have waited long enough to be closed, 
    %C = lists:filter(fun(C) -> (C#sub_channel.last_modified + C#sub_channel.delay) > H end, B),
%then make a channel_timeout_tx for them all.
    %lists:map(fun(C) -> tx_pool_feeder:absorb_async(keys:sign(channel_timeout_tx(C#sub_channel.acc1, C#sub_channel.id, Fee))) end, C).
    
    
test() ->
    ID = <<1:256>>,
    Acc1 = constants:master_pub(),
    Acc2 = constants:master_pub(),
    Height = 1,
    Delay = 11,
    CID = hash:doit(2),
    A = new(ID,CID,1,[Acc1,Acc2],Height,Delay),
    io:fwrite(packer:pack(A)),
    io:fwrite("\n"),
    A = deserialize(serialize(A)),
    C = A,
    R = trees:empty_tree(sub_channels),
    %NewLoc = write(C, constants:root0()),
    NewLoc = write(C, R),
    {Root, C, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(C), Proof),
    success.
    

