-module(contracts).
-export([new/4, new/2,
         code/1, many_types/1, last_modified/1, nonce/1, delay/1, volume/1, closed/1, %custom for this tree
	 write/2, get/2, delete/2,%update tree stuff
         dict_update/9, dict_delete/2, dict_write/2, dict_get/2, dict_get/3,%update dict stuff
         verify_proof/4, make_leaf/3, key_to_int/1, 
	 deserialize/1, serialize/1, 
         make_id/1, make_id/4,
         make_v_id/4, make_v_id/1,
	 all/0,
	 test/0]).

-include("../../records.hrl").


code(C) -> C#contract.code.
many_types(C) -> C#contract.many_types.
volume(C) -> C#contract.volume.
last_modified(C) -> C#contract.last_modified.
nonce(C) -> C#contract.nonce.
delay(C) -> C#contract.delay.
closed(C) -> C#contract.closed.

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
    Channel#contract{
      nonce = NewNonce,
      last_modified = Height,
      delay = Delay,
      closed = Close
     }.
    
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
       SourceType:16,
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
      SourceType:16,
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
    ID = make_v_id(Channel),
    csc:update({contracts, ID}, Channel, Dict).
write(Channel, Root) ->
    ID = make_id(Channel),
    M = serialize(Channel),
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
make_v_id(X = #contract{
            code = C, many_types = M, 
            source = S, source_type = T}) ->
    CID = make_id(X),
    %{key, CID}.
    CID.
%{key, C, M, S, T}.
make_v_id(C, M, S, T) ->
    CID = make_id(C, M, S, T),
    %{key, CID}.
    CID.
    %{key, C, M, S, T}.

make_id(C,MT,S,ST) ->
    <<_:256>> = C,
    <<_:256>> = S,
    hash:doit(<<C/binary,
                S/binary,
                MT:16,
                ST:16>>).
    
dict_get(Key, Dict, _) ->
    dict_get(Key, Dict).
dict_get(Key, Dict) ->
    case csc:read({contracts, Key}, Dict) of
        error -> error;
        {empty, _} -> empty;
        {ok, contracts, Val} -> Val
    end.
            
dict_get_old(Key, Dict) ->
    %<<_:256>> = Key,
    X = dict:find({contracts, Key}, Dict),
    case X of
	error -> error;
	%error -> empty;
        {ok, 0} -> empty;
        {ok, empty} -> empty;
        {ok, {contracts, Key}} -> empty;
        {ok, Y} -> Y
%            SY = size(Y),
%            case SY of
%                153 -> trees2:deserialize(7, Y);
%                _ -> deserialize(Y)
%            end
    end.
%deserialize 7
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
    
    
test() ->
    Code = hash:doit(1),
    Many = 2,
    A = new(Code, Many),
    A = deserialize(serialize(A)),
    C = A,
    R = trees:empty_tree(contracts),
    NewLoc = write(C, R),
    ID = make_id(A),
    {Root, C, Proof} = get(ID, NewLoc),
    true = verify_proof(Root, ID, serialize(C), Proof),
    success.
    

