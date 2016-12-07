-module(account).
-export([serialize/1,deserialize/1,new/3,nonce/1,overwrite/3,write/2,get/2,update/4,addr/1,root_hash/1, test/0]).
-record(acc, {balance = 0, %amount of money you have
	      nonce = 0, %increments with every tx you put on the chain. 
	      height = 0,  %The last height at which you paid the tax
	      addr = []}). %addr is the hash of the public key we use to spend money.
addr(X) -> X#acc.addr.
update(Acc, Amount, NewNonce, NewHeight) ->
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
new(Addr, Balance, Height) ->
    #acc{addr = Addr, balance = Balance, nonce = 0, height = Height}.
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
    Out = <<(A#acc.balance):BAL, 
      (A#acc.nonce):(Nbits), 
      (A#acc.height):HEI,
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
    <<B1:BAL,
      B2:Nbits,
      B4:HEI,
      B6:HD,
      _:AP>> = A,
    #acc{balance = B1, nonce = B2, height = B4, addr = testnet_sign:binary2address(<<B6:HD>>)}.
    
write(Root, Account) ->
    M = serialize(Account),
    trie:put(M, Root, 0, accounts).%returns {key, root}
overwrite(Root, Account, ID) ->
    M = serialize(Account),
    trie:overwrite(ID, M, Root, 0, accounts).
get(Id, Accounts) ->
    trie:get(Id, Accounts, accounts).%{roothash, leaf, proof}
    
root_hash(Accounts) ->
    trie:root_hash(accounts, Accounts).

test() ->
    {Address, _Pub, _Priv} = testnet_sign:hard_new_key(),
    Acc = new(Address, 0, 0),
    S = serialize(Acc),
    Acc = deserialize(S),
    write(0, Acc).
