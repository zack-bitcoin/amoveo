
-module(accounts).
-export([new/4,nonce/1,write/2,get/2,update/5,update/7,addr/1,id/1,balance/1,root_hash/1,now_balance/4,delete/2,
	 receive_shares/4, send_shares/4,
	 shares/1, bets/1, update_bets/2,
	 serialize/1, garbage/0, test/0]).
-record(acc, {balance = 0, %amount of money you have
	      nonce = 0, %increments with every tx you put on the chain. 
	      height = 0,  %The last height at which you paid the tax
	      addr = [], %addr is the hash of the public key we use to spend money.
	      id = 0,%id is your location in the merkle trie. It is also used as an identification for sending money, since it is shorter than your address.
	      bets = 0,%This is a pointer to the merkel tree that stores how many bets you have made in each oracle.
	      shares = 0}). %shares is a pointer to a merkel tree that stores how many shares you have at each price.
-define(id, accounts).
addr(X) -> X#acc.addr.
id(X) -> X#acc.id.
balance(X) -> X#acc.balance.
shares(X) -> X#acc.shares.
bets(X) -> X#acc.bets.
update_bets(X, B) ->
    X#acc{bets = B}.
receive_shares(Acc, Shares, Height, Trees) ->
    SharesTree = Acc#acc.shares,
    {Tokens, NewTree} = shares:receive_shares(Shares, SharesTree, Height, Trees),
    Acc#acc{shares = NewTree, balance =Acc#acc.balance + Tokens}.
send_shares(Acc, Shares, Height, Trees) ->
    SharesTree = Acc#acc.shares,
    {Tokens, NewTree} = shares:send_shares(Shares, SharesTree, Height, Trees),
    Acc#acc{shares = NewTree, balance = Acc#acc.balance + Tokens}.
now_balance(Acc, Amount, NewHeight, Trees) ->
    OldHeight = Acc#acc.height,
    Governance = trees:governance(Trees),
    AR = governance:get_value(account_rent, Governance),
    Rent = AR*(NewHeight - OldHeight),
    Amount + Acc#acc.balance - Rent.
    
update(Id, Trees, Amount, NewNonce, NewHeight) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _} = get(Id, Accounts),
    update(Id, Trees, Amount, NewNonce, NewHeight, Acc#acc.shares, Acc#acc.bets).
update(Id, Trees, Amount, NewNonce, NewHeight, Shares, Bets) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _} = get(Id, Accounts),
    OldNonce = Acc#acc.nonce,
    FinalNonce = case NewNonce of
	none -> Acc#acc.nonce;
	N -> true = N > OldNonce,
	     N
    end,
    OldHeight = Acc#acc.height,
    true = NewHeight >= OldHeight,
    NewBalance = now_balance(Acc, Amount, NewHeight, Trees),
    true = NewBalance > 0,
    Acc#acc{balance = NewBalance,
	    nonce = FinalNonce,
	    height = NewHeight,
	    shares = Shares,
	    bets = Bets}.
new(Id, Addr, Balance, Height) ->
    #acc{id = Id, addr = Addr, balance = Balance, nonce = 0, height = Height, bets = 0, shares = 0}.
nonce(X) -> X#acc.nonce.
serialize(A) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    Addr = A#acc.addr,
    Baddr = testnet_sign:address2binary(Addr),
    SizeAddr = size(Baddr),
    SizeAddr = constants:hash_size(),
    Nbits = constants:account_nonce_bits(),
    KL = key_length(),
    BetsRoot = oracle_bets:root_hash(A#acc.bets),
    SharesRoot = shares:root_hash(A#acc.shares),
    HS = constants:hash_size(),
    HS = size(BetsRoot),
    HS = size(SharesRoot),
    ID = A#acc.id,
    true = (ID - 1) < math:pow(2, KL),
    Out = <<(A#acc.balance):BAL, 
	    (A#acc.nonce):(Nbits), 
	    (A#acc.height):HEI,
	    ID:KL,
	    Baddr/binary,
	    BetsRoot/binary,
	    SharesRoot/binary>>,
    Size = size(Out),
    Size = constants:account_size(),
    Out.

deserialize(A) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    HS = constants:hash_size(),
    HD = HS*8,
    Nbits = constants:account_nonce_bits(),
    KL = constants:key_length(),
    <<B1:BAL,
      B2:Nbits,
      B4:HEI,
      B5:KL,
      B6:HD,
      _:HD,
      _:HD
    >> = A,
    #acc{balance = B1, nonce = B2, height = B4, id = B5, addr = testnet_sign:binary2address(<<B6:HD>>)}.
    
write(Root, Account) ->%These are backwards.
    ID = Account#acc.id,
    M = serialize(Account),
    %HS2 = constants:hash_size() * 2,
    KL = constants:key_length(),
    KL2 = KL * 2,
    %BetsRoot = oracle_bets:root_hash(Account#acc.bets),
    %SharesRoot = shares:root_hash(Account#acc.shares),
    <<Meta:KL2>> = <<(Account#acc.bets):KL, (Account#acc.shares):KL>>,
    trie:put(ID, M, Meta, Root, ?id).%returns a pointer to the new root.
delete(ID, Accounts) ->
    trie:delete(ID, Accounts, ?id).
key_length() ->
    constants:key_length().
get(Id, Accounts) ->
    KL = constants:key_length(),
    KL2 = KL * 2,
    true = Id > 0,
    true = Id  < math:pow(2, KL),
    {RH, Leaf, Proof} = trie:get(Id, Accounts, ?id),
    V = case Leaf of
	    empty -> empty;
	    L -> X = deserialize(leaf:value(L)),
		 Meta = leaf:meta(L),
		 <<Bets:KL, Shares:KL>> = <<Meta:KL2>>,
		 X#acc{bets = Bets, shares = Shares}
	end,
    {RH, V, Proof}.

root_hash(Accounts) ->
    trie:root_hash(?id, Accounts).

garbage() -> trees:garbage(?id).

test() ->
    {Address, _Pub, _Priv} = testnet_sign:hard_new_key(),
    ID = 3,
    Acc = new(ID, Address, 0, 0),
    %io:fwrite(Acc),
    S = serialize(Acc),
    Acc = deserialize(S),
    NewLoc = write(0, Acc),
    {_, Acc, _} = get(ID, NewLoc),
    success.
