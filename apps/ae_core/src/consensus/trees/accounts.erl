-module(accounts).
-export([new/3,nonce/1,write/2,get/2,update/5,update/6,
         dict_update/5, dict_update/6, dict_get/2,
	 %addr/1, id/1,
	 balance/1,root_hash/1,now_balance/4,delete/2,
	 bets/1, bets_hash/1, update_bets/2,
	 ensure_decoded_hashed/1, height/1, verify_proof/4,
         dict_write/2, dict_delete/2,
	 serialize/1, deserialize/1, pubkey/1, test/0]).
-record(acc, {balance = 0, %amount of money you have
	      nonce = 0, %increments with every tx you put on the chain. 
	      height = 0,  %The last height at which you paid the tax
	      pubkey = <<>>,
	      bets = 0,%This is a pointer to the merkel tree that stores how many bets you have made in each oracle.
              bets_hash = <<>>}).
	      %shares = 0}). %shares is a pointer to a merkel tree that stores how many shares you have at each price.
-define(id, accounts).

balance(Account) -> Account#acc.balance.
nonce(Account) -> Account#acc.nonce.
height(Account) -> Account#acc.height.
pubkey(Account) -> Account#acc.pubkey.
bets(Account) -> Account#acc.bets.
bets_hash(Account) -> Account#acc.bets_hash.
%shares(Account) -> Account#acc.shares.

root_hash(Accounts) ->
    trie:root_hash(?id, Accounts).

new(Pub, Balance, Height) ->
    #acc{pubkey = Pub, balance = Balance, nonce = 0, height = Height, bets = 0, bets_hash = oracle_bets:root_hash(0)}.

dict_update(Pub, Dict, Amount, NewNonce, NewHeight) ->
    Account = dict_get(Pub, Dict),
    dict_update(Pub, Dict, Amount, NewNonce, NewHeight, Account#acc.bets).
dict_update(Pub, Dict, Amount, NewNonce, NewHeight, Bets) ->
    PubHash = ensure_decoded_hashed(Pub),
    Account = dict_get(Pub, Dict),
    OldNonce = Account#acc.nonce,
    FinalNonce = case NewNonce of
                     none ->
                         Account#acc.nonce;
                     NewNonce ->
                         true = NewNonce > OldNonce,
                         NewNonce
                 end,
    OldHeight = Account#acc.height,
    true = NewHeight >= OldHeight,
    NewBalance = new_balance_dict(Account, Amount, NewHeight, Dict),
    true = NewBalance > 0,
    Account#acc{balance = NewBalance,
                nonce = FinalNonce,
                height = NewHeight,
                bets = Bets,
                bets_hash = oracle_bets:root_hash(Bets)}.

update(Pub, Trees, Amount, NewNonce, NewHeight) ->
    PubHash = ensure_decoded_hashed(Pub),
    Accounts = trees:accounts(Trees),
    {_, Account, _} = get(PubHash, Accounts),
    update(PubHash, Trees, Amount, NewNonce, NewHeight, Account#acc.bets).

update(PubHash, Trees, Amount, NewNonce, NewHeight, Bets) ->
    Accounts = trees:accounts(Trees),
    {_, Account, _} = get(PubHash, Accounts),
    OldNonce = Account#acc.nonce,
    FinalNonce = case NewNonce of
                     none ->
                         Account#acc.nonce;
                     NewNonce ->
                         true = NewNonce > OldNonce,
                         NewNonce
                 end,
    OldHeight = Account#acc.height,
    true = NewHeight >= OldHeight,
    NewBalance = new_balance(Account, Amount, NewHeight, Trees),
    true = NewBalance > 0,
    Account#acc{balance = NewBalance,
                nonce = FinalNonce,
                height = NewHeight,
                bets = Bets,
                bets_hash = oracle_bets:root_hash(Bets)}.

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
            _ ->%governance:get_value(account_rent, Governance)
                0
                    end,
    Amount + Acc#acc.balance - (Rent * DH).

update_bets(Account, Bets) ->
    Account#acc{bets = Bets,
                bets_hash = oracle_bets:root_hash(Bets)}.

get(Pub, Accounts) ->
    PubHash = ensure_decoded_hashed(Pub),
    PubId = trees:hash2int(PubHash),
    {RH, Leaf, Proof} = trie:get(PubId, Accounts, ?id),
    Account = case Leaf of
                  empty -> empty;
                  Leaf ->
                      Account0 = deserialize(leaf:value(Leaf)),
                      Meta = leaf:meta(Leaf),
                      Account0#acc{bets = Meta}
              end,
    {RH, Account, Proof}.
dict_write(Account, Dict) ->
    Pub = Account#acc.pubkey,
    dict:store({accounts, Pub}, 
               serialize(Account),
               Dict).
write(Root, Account) ->
    Pub = Account#acc.pubkey,
    SizePubkey = constants:pubkey_size(),
    SizePubkey = size(Pub),
    PubHash = ensure_decoded_hashed(Pub),
    SerializedAccount = serialize(Account),
    true = size(SerializedAccount) == constants:account_size(),
    KeyLength = constants:key_length(),
    <<Meta:KeyLength>> = <<(Account#acc.bets):KeyLength>>,
    PubId = trees:hash2int(PubHash),
    trie:put(PubId, SerializedAccount, Meta, Root, ?id). % returns a pointer to the new root

delete(Pub0, Accounts) ->
    PubHash = ensure_decoded_hashed(Pub0),
    PubId = trees:hash2int(PubHash),
    trie:delete(PubId, Accounts, ?id).
dict_delete(Pub, Dict) ->
    dict:store({accounts, Pub}, 
               0,
               Dict).

new_balance_dict(Account, Amount, NewHeight, Dict) ->
    OldHeight = Account#acc.height,
    Pub = Account#acc.pubkey,
    HeightDiff = NewHeight - OldHeight,
    MasterPub = constants:master_pub(),
    Rent =
        case Pub of
            MasterPub ->
                -(governance:dict_get_value(developer_reward, Dict));
            _Other ->
                0
        end,
    Amount + Account#acc.balance - (Rent * HeightDiff).
new_balance(Account, Amount, NewHeight, Trees) ->
    OldHeight = Account#acc.height,
    Governance = trees:governance(Trees),
    Pub = Account#acc.pubkey,
    HeightDiff = NewHeight - OldHeight,
    MasterPub = constants:master_pub(),
    Rent =
        case Pub of
            MasterPub ->
                -(governance:get_value(developer_reward, Governance));
            _Other ->
                0
        end,
    Amount + Account#acc.balance - (Rent * HeightDiff).

serialize(Account) ->
    true = size(Account#acc.pubkey) == constants:pubkey_size(),
    BalanceSize = constants:balance_bits(),
    HeightSize = constants:height_bits(),
    NonceSize = constants:account_nonce_bits(),
    
    BetsRoot = oracle_bets:root_hash(Account#acc.bets),
    HashSize = constants:hash_size(),
    true = size(BetsRoot) == HashSize,
    SerializedAccount =
        <<(Account#acc.balance):BalanceSize,
          (Account#acc.nonce):NonceSize,
          (Account#acc.height):HeightSize,
          (Account#acc.pubkey)/binary,
         BetsRoot/binary>>,
    true = size(SerializedAccount) == constants:account_size(),
    SerializedAccount.


%% Internals

deserialize(SerializedAccount) ->
    BalanceSize = constants:balance_bits(),
    NonceSize = constants:account_nonce_bits(),
    HeightSize = constants:height_bits(),
    SizePubkey = constants:pubkey_size(),
    PubkeyBits = SizePubkey * 8,
    HashSize = constants:hash_size(),
    HashSizeBits = HashSize * 8,
    <<Balance:BalanceSize,
      Nonce:NonceSize,
      Height:HeightSize,
      Pubkey:PubkeyBits,
      BetsRoot:HashSizeBits>> = SerializedAccount,
    #acc{balance = Balance,
         nonce = Nonce,
         height = Height,
         pubkey = <<Pubkey:PubkeyBits>>,
         bets_hash = <<BetsRoot:HashSizeBits>>}.

ensure_decoded_hashed(Pub) ->
    HashSize = constants:hash_size(),
    PubkeySize = constants:pubkey_size(),
    case size(Pub) of
        HashSize ->
            Pub;
        PubkeySize ->
            testnet_hasher:doit(Pub);
        _Other ->
            lager:warning("Pub decode problem: ~p", [Pub]),
            testnet_hasher:doit(base64:decode(Pub))
    end.
    
cfg() ->
    KL = constants:key_length(),
    MetaSize = KL div 8,%all in bytes
    HashSize = constants:hash_size(),
    ValueSize = constants:account_size(),
    PathSize = constants:hash_size()*8,
    cfg:new(PathSize, ValueSize, accounts, MetaSize, HashSize).
verify_proof(RootHash, Key, Value, Proof) ->
    CFG = cfg(),
    V = case Value of
	    0 -> empty;
	    X -> X
	end,
    verify:proof(RootHash, 
		 leaf:new(trees:hash2int(ensure_decoded_hashed(Key)), 
			  V, 0, CFG), 
		 Proof, CFG).
dict_get(Key, Dict) ->
    X = dict:fetch({accounts, Key}, Dict),
    case X of
        0 -> empty;
        _ -> deserialize(X)
    end.

test() ->
    {Pub, _Priv} = testnet_sign:new_key(),
    Acc = new(Pub, 0, 0),
    %io:fwrite(Acc),
    S = serialize(Acc),
    Acc = deserialize(S),
    NewLoc = write(0, Acc),
    {Root, Acc, Proof} = get(Pub, NewLoc),
    true = verify_proof(Root, Pub, serialize(Acc), Proof),
    {Root2, empty, Proof2} = get(Pub, 0),
    true = verify_proof(Root2, Pub, 0, Proof2),
    success.
