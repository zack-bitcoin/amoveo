-module(accounts).
-export([new/3,nonce/1,write/2,get/2,update/5,update/7,
	 %addr/1, id/1,
	 balance/1,root_hash/1,now_balance/4,delete/2,
	 receive_shares/4, send_shares/4,
	 shares/1, bets/1, update_bets/2,
	 pub_decode/1, height/1, verify_proof/4,
	 serialize/1, pubkey/1, test/0]).
-record(acc, {balance = 0, %amount of money you have
	      nonce = 0, %increments with every tx you put on the chain. 
	      height = 0,  %The last height at which you paid the tax
	      pubkey = [],
	      %addr = [], %addr is the hash of the public key we use to spend money.
	      %id = 0,%id is your location in the merkle trie. It is also used as an identification for sending money, since it is shorter than your address.
	      bets = 0,%This is a pointer to the merkel tree that stores how many bets you have made in each oracle.
	      shares = 0}). %shares is a pointer to a merkel tree that stores how many shares you have at each price.
-define(id, accounts).
pubkey(X) -> X#acc.pubkey.
%addr(X) -> X#acc.addr.
%id(X) -> X#acc.id.
balance(X) -> X#acc.balance.
shares(X) -> X#acc.shares.
bets(X) -> X#acc.bets.
height(X) -> X#acc.height.
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
    %ID = Acc#acc.id,
    Pub = Acc#acc.pubkey,
    DH = NewHeight - OldHeight,
    MasterPub = constants:master_pub(),
    Rent = 
	case Pub of
	    MasterPub ->
		-(governance:get_value(developer_reward, Governance));
	    _ ->
		%governance:get_value(account_rent, Governance)
		0
	end,
    Amount + Acc#acc.balance - (Rent * DH).
    
update(Pub0, Trees, Amount, NewNonce, NewHeight) ->
    Pub = pub_decode(Pub0),
    Accounts = trees:accounts(Trees),
    {_, Acc, _} = get(Pub, Accounts),
    update(Pub, Trees, Amount, NewNonce, NewHeight, Acc#acc.shares, Acc#acc.bets).
update(Pub, Trees, Amount, NewNonce, NewHeight, Shares, Bets) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _} = get(Pub, Accounts),
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
new(Pub0, Balance, Height) ->
    %Pub = pub_decode(Pub0),
    %PubSize = constants:pubkey_size(),
    %PubSize2 = size(Pub),
    #acc{pubkey = Pub0, balance = Balance, nonce = 0, height = Height, bets = 0, shares = 0}.
nonce(X) -> X#acc.nonce.
serialize(A) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    Pubkey = A#acc.pubkey,
    PubSize = constants:pubkey_size(),
    PubSize2 = size(Pubkey),
    
    SizePubkey = constants:pubkey_size(),
    SizePubkey = size(Pubkey),
    Nbits = constants:account_nonce_bits(),
    SharesRoot = shares:root_hash(A#acc.shares),
    BetsRoot = oracle_bets:root_hash(A#acc.bets),
    HS = constants:hash_size(),
    HS = size(BetsRoot),
    HS = size(SharesRoot),
    %PubkeyBits = SizePubkey * 8,
    PS = size(A#acc.pubkey),
    PS = SizePubkey,
    Out = <<(A#acc.balance):BAL, 
	     (A#acc.nonce):Nbits, 
	     (A#acc.height):HEI,
	     (A#acc.pubkey)/binary,
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
    SizePubkey = constants:pubkey_size(),
    PubkeyBits = SizePubkey * 8,
    <<B1:BAL,
      B2:Nbits,
      B4:HEI,
      B5:PubkeyBits,
      _:HD,
      _:HD
    >> = A,
    #acc{balance = B1, nonce = B2, height = B4, pubkey = <<B5:PubkeyBits>>}.
    
write(Root, Account) ->%These are backwards.
    Pub = Account#acc.pubkey,
    SizePubkey = constants:pubkey_size(),
    SizePubkey = size(Pub),
    HP = pub_decode(Pub),
    M = serialize(Account),
    true = size(M) == constants:account_size(),
    KL = constants:key_length(),
    KL2 = KL * 2,
    <<Meta:KL2>> = <<(Account#acc.bets):KL, (Account#acc.shares):KL>>,
    HPID = trees:hash2int(HP),
    trie:put(HPID, M, Meta, Root, ?id).%returns a pointer to the new root.

delete(Pub0, Accounts) ->
    HP = pub_decode(Pub0),
    trie:delete(trees:hash2int(HP), Accounts, ?id).
pub_decode(Pub) ->
    HS = constants:hash_size(),
    SizePubkey = constants:pubkey_size(),
    case size(Pub) of
	HS -> Pub;
	SizePubkey ->
	    testnet_hasher:doit(Pub);
	_ -> 
	    io:fwrite("pub decode problem "),
	    io:fwrite(Pub),
	    io:fwrite("\n"),
	    testnet_hasher:doit(base64:decode(Pub))
    end.
    
get(Pub, Accounts) ->
    HP = pub_decode(Pub),
    KL = constants:key_length(),
    KL2 = KL * 2,
    HPID = trees:hash2int(HP),
    {RH, Leaf, Proof} = trie:get(HPID, Accounts, ?id),
    %{RH, Leaf, Proof} = trees:get(HPID, Accounts, ?id),
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

%garbage() -> trees:garbage(?id).
cfg() ->
    KL = constants:key_length(),
    MetaSize = KL div 4,%all in bytes
    HashSize = constants:hash_size(),
    ValueSize = constants:account_size(),
    PathSize = constants:hash_size()*8,
    cfg:new(PathSize, ValueSize, accounts, MetaSize, HashSize).
verify_proof(RootHash, Key, Value, Proof) ->
    CFG = cfg(),
    V = case Value of
	    0 -> empty;
	    X -> serialize(X)
	end,
    verify:proof(RootHash, 
		 leaf:new(trees:hash2int(pub_decode(Key)), 
			  V, 0, CFG), 
		 Proof, CFG).

test() ->
    {Pub, _Priv} = testnet_sign:new_key(),
    Acc = new(Pub, 0, 0),
    %io:fwrite(Acc),
    S = serialize(Acc),
    Acc = deserialize(S),
    NewLoc = write(0, Acc),
    {Root, Acc, Proof} = get(Pub, NewLoc),
    true = verify_proof(Root, Pub, Acc, Proof),
    {Root2, empty, Proof2} = get(Pub, 0),
    true = verify_proof(Root2, Pub, 0, Proof2),
    success.
