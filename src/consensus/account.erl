-module(account).
-export([serialize/1,deserialize/1,new/4,nonce/1,write/3,get/2,update/5,addr/1,id/1,root_hash/1, test/0]).
-record(acc, {balance = 0, %amount of money you have
	      nonce = 0, %increments with every tx you put on the chain. 
	      height = 0,  %The last height at which you paid the tax
	      addr = [], %addr is the hash of the public key we use to spend money.
	      id = 0}). %id is your location in the merkle trie. It is also used as an identification for sending money, since it is shorter than your address.
addr(X) -> X#acc.addr.
id(X) -> X#acc.id.
update(Id, Accounts, Amount, NewNonce, NewHeight) ->
    {_, Acc, _} = get(Id, Accounts),
    OldNonce = Acc#acc.nonce,
    OldHeight = Acc#acc.height,
    true = NewNonce > OldNonce,
    true = NewHeight > OldHeight,
    Rent = constants:account_rent()*(NewHeight - OldHeight),
    NewBalance = Amount + Acc#acc.balance - Rent,
    true = NewBalance > 0,
    #acc{balance = NewBalance,
	 nonce = NewNonce,
	 height = NewHeight, 
	 addr = Acc#acc.addr}.
new(Id, Addr, Balance, Height) ->
    #acc{id = Id, addr = Addr, balance = Balance, nonce = 0, height = Height}.
nonce(X) -> X#acc.nonce.
serialize(A) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    Addr = A#acc.addr,
    Baddr = testnet_sign:address2binary(Addr),
    SizeAddr = size(Baddr),
    SizeAddr = hash:hash_depth(),
    Nbits = constants:account_nonce_bits(),
    AP = constants:account_padding(),
    KL = constants:key_length(),
    Out = <<(A#acc.balance):BAL, 
	    (A#acc.nonce):(Nbits), 
	    (A#acc.height):HEI,
	    (A#acc.id):KL,
	    Baddr/binary,
	    0:AP>>,
    Size = size(Out),
    Size = constants:account_size(),
    Out.

deserialize(A) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    HD = hash:hash_depth()*8,
    Nbits = constants:account_nonce_bits(),
    AP = constants:account_padding(),
    KL = constants:key_length(),
    <<B1:BAL,
      B2:Nbits,
      B4:HEI,
      B5:KL,
      B6:HD,
      _:AP>> = A,
    #acc{balance = B1, nonce = B2, height = B4, id = B5, addr = testnet_sign:binary2address(<<B6:HD>>)}.
    
write(Root, Account, ID) ->
    M = serialize(Account),
    trie:put(ID, M, Root, 0, accounts).%returns a pointer to the new root.
get(Id, Accounts) ->
    %io:fwrite("ID is "),
    %io:fwrite(integer_to_list(Id)),
    {RH, Leaf, Proof} = trie:get(Id, Accounts, accounts),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    %V = deserialize(leaf:value(Leaf)),
    {RH, V, Proof}.

root_hash(Accounts) ->
    trie:root_hash(accounts, Accounts).

test() ->
    {Address, _Pub, _Priv} = testnet_sign:hard_new_key(),
    Acc = new(3, Address, 0, 0),
    %io:fwrite(Acc),
    S = serialize(Acc),
    Acc = deserialize(S),
    ID = 1,
    NewLoc = write(0, Acc, ID),
    {_, Acc, _} = get(ID, NewLoc),
    success.
