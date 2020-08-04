-module(contracts).
-export([new/4, new/2,
         code/1, many_types/1, last_modified/1, nonce/1, delay/1, volume/1, closed/1, %custom for this tree
	 write/2, get/2, delete/2,%update tree stuff
         dict_update/9, dict_delete/2, dict_write/2, dict_get/2,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
         make_id/1,make_id/4,
	 all/0,
	 test/0]).
%This is the part of the channel that is written onto the hard drive.

-include("../../records.hrl").


code(C) -> C#contract.code.
many_types(C) -> C#contract.many_types.
volume(C) -> C#contract.volume.
last_modified(C) -> C#contract.last_modified.
%mode(C) -> C#contract.mode.
nonce(C) -> C#contract.nonce.
delay(C) -> C#contract.delay.
closed(C) -> C#contract.closed.
%shares(C) -> C#contract.shares.

dict_update(ID, Dict, Nonce, Inc1, Inc2, Amount, Delay, Height, Close0) ->
    Close = case Close0 of 
                1 -> 1;
                0 -> 0;
                true -> 1;
                false -> 0
            end,
    true = (Close == 1) or (Close == 0),
    Channel = dict_get(ID, Dict),
    CNonce = Channel#contract.nonce,
    NewNonce = if
		   Nonce == none -> CNonce;
		   true -> 
		       Nonce
	       end,
    T1 = Channel#contract.last_modified,
    DH = Height - T1,
    C = Channel#contract{
          nonce = NewNonce,
          last_modified = Height,
          delay = Delay,
          closed = Close
         },
    %io:fwrite(packer:pack(C)),
    C.
    
new(Code, Many) ->
    new(Code, Many, <<0:256>>, 0).
new(Code, Many, Source, SourceType) ->
    case Source of
        <<0:256>> ->
            SourceType = 0;
        <<_:256>> ->
            true = is_integer(SourceType)
    end,
    #contract{code = Code, many_types = Many,
             source = Source,
             source_type = SourceType}.
serialize(C) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    NON = constants:channel_nonce_bits(),
    Delay = constants:channel_delay_bits(),
    HS = constants:hash_size(),
    Code = C#contract.code,
    Result = C#contract.result,
    Many = C#contract.many_types,
    true = Many > 1,
    Volume = C#contract.volume,
    ok = if
               Volume >= 0 -> ok;
               true -> 
                   io:fwrite("contract serialize volume negative \n"),
                   io:fwrite(packer:pack(Volume)),
                   io:fwrite("\n"),
                   false
           end,
    Source = C#contract.source,
    SourceType = C#contract.source_type,
    Drain = C#contract.sink,
    32 = size(Code),
    32 = size(Result),
    32 = size(Source),
    32 = size(Drain),
    << Code/binary,
       Result/binary,
       Source/binary,
       Drain/binary,
       SourceType:32,
       Many:16,
       (C#contract.nonce):NON,
       (C#contract.last_modified):HEI,
       (C#contract.delay):Delay,
       (C#contract.closed):8,
       Volume:BAL
    >>.
deserialize(B) ->
    PS = constants:pubkey_size()*8,
    ACC = constants:address_bits(),
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    NON = constants:channel_nonce_bits(),
    DelayBits = constants:channel_delay_bits(),
    HS = constants:hash_size()*8,
    <<Code:HS,
      Result:HS,
      Source:HS,
      Drain:HS,
      SourceType:32,
      Many:16,
      Nonce:NON,
      LastModified:HEI,
      Delay:DelayBits,
      Closed:8,
      Volume:BAL >> = B,
    #contract{code = <<Code:HS>>,
              many_types = Many,
              result = <<Result:HS>>,
              source = <<Source:HS>>,
              source_type = SourceType,
              sink = <<Drain:HS>>,
              nonce = Nonce,
              last_modified = LastModified,
              delay = Delay,
              closed = Closed,
              volume = Volume}.
dict_write(Channel, Dict) ->
    %ID = Channel#contract.id,
    ID = make_id(Channel),
    dict:store({contracts, ID},
               serialize(Channel),
               Dict).
write(Channel, Root) ->
    %ID = Channel#contract.id,
    ID = make_id(Channel),
    M = serialize(Channel),
    %Shares = Channel#contract.shares,
    trie:put(key_to_int(ID), M, 0, Root, contracts). %returns a pointer to the new root
key_to_int(X) -> 
    <<_:256>> = X,
    <<Y:256>> = hash:doit(X),
    Y.
make_id(X = #contract{}) ->
    Code = X#contract.code,
    32 = size(Code),
    make_id(X#contract.code,
            X#contract.many_types,
            X#contract.source,
            X#contract.source_type).

make_id(C,MT,S,ST) ->
    <<_:256>> = C,
    <<_:256>> = S,
    hash:doit(<<C/binary,
                S/binary,
                MT:16,
                ST:16>>).
    

dict_get(Key, Dict) ->
    <<_:256>> = Key,
    X = dict:find({contracts, Key}, Dict),
    case X of
	error -> error;
	%error -> empty;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, Y} -> deserialize(Y)
    end.
get(ID, Channels) ->
    <<_:256>> = ID,
    {RH, Leaf, Proof} = trie:get(key_to_int(ID), Channels, contracts),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
dict_delete(Key, Dict) ->      
    dict:store({contracts, Key}, 0, Dict).
delete(ID,Channels) ->
    trie:delete(ID, Channels, contracts).
make_leaf(Key, V, CFG) ->
    leaf:new(key_to_int(Key), V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).

%function to look up all open channels.
all() ->
    Trees = (tx_pool:get())#tx_pool.block_trees,
    Channels = trees:contracts(Trees),
    All = trie:get_all(Channels, contracts),
    lists:map(
      fun(Leaf) ->
	      deserialize(leaf:value(Leaf))
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
%    A2 = A#contract.acc2,
%    H2 = A#contract.last_modified + A#contract.delay,
%    if
%	(not (A2 == K)) -> ok; %only close the ones that are opened with the server, 
%	H2 < H -> ok; %only close the ones that have waited long enough to be closed, 
%	true ->
%	    Tx = channel_timeout_tx:make_dict(A#contract.acc1, A#contract.id, Fee),
%	    Stx = keys:sign(Tx),
%	    tx_pool_feeder:absorb_async(Stx)
%    end,
%    close_many2(T, K, H, Fee).
%
%keep the ones that are opened with the server, 
    %B = lists:filter(fun(C) -> C#contract.acc2 == K end, A),
%keep the ones that have waited long enough to be closed, 
    %C = lists:filter(fun(C) -> (C#contract.last_modified + C#contract.delay) > H end, B),
%then make a channel_timeout_tx for them all.
    %lists:map(fun(C) -> tx_pool_feeder:absorb_async(keys:sign(channel_timeout_tx(C#contract.acc1, C#contract.id, Fee))) end, C).
    
    
test() ->
    Code = hash:doit(1),
    Many = 2,
    A = new(Code, Many),
    A = deserialize(serialize(A)),
    C = A,
    R = trees:empty_tree(contracts),
    %NewLoc = write(C, constants:root0()),
    NewLoc = write(C, R),
    ID = make_id(A),
    {Root, C, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(C), Proof),
    success.
    

