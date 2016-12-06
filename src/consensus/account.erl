-module(account).
-export([serialize/1,deserialize/1,new/3,nonce/1,overwrite/3,write/2, update/4, test/0]).
-record(acc, {balance = 0, %amount of money you have
	      nonce = 0, %increments with every tx you put on the chain. 
	      height = 0,  %The last height at which you paid the tax
	      addr = []}). %addr is the hash of the public key we use to spend money.
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
    SizeAddr = size(Addr),
    SizeAddr = hash:hash_depth(),
    Nbits = constants:account_nonce_bits(),
    AP = constants:account_padding(),
    Out = <<(A#acc.balance):BAL, 
      (A#acc.nonce):(Nbits), 
      (A#acc.height):HEI,
      Addr/binary,
	    0:AP>>,
    Size = size(Out),
    Size = constants:account_size() div 8,
    Out.
    

deserialize(A) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    HD = merkle_constants:hash_depth(),
    Nbits = constants:account_nonce_bits(),
    AP = constants:account_padding(),
    <<B1:BAL,
      B2:Nbits,
      B4:HEI,
      B6:HD,
      _:AP>> = A,
    #acc{balance = B1, nonce = B2, height = B4, addr = <<B6:HD>>}.
    
write(Root, Account) ->
    Balance = Account#acc.balance,
    M = account:serialize(Account),
    trie:put(M, Root, Balance, accounts).
overwrite(Root, Account, ID) ->
    Balance = Account#acc.balance,
    M = account:serialize(Account),
    trie:overwrite(ID, M, Root, Balance, accounts).

test() ->

    ok.
