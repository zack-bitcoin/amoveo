-module(accounts).
-export([new/3,nonce/1,write/2,get/2,%update/5,%update/6,
         dict_update/5, dict_update/6, dict_get/2,
	 balance/1,new_balance/4,delete/2,
	 bets/1, bets_hash/1, update_bets/2,
	 verify_proof/4,
         dict_write/2, dict_write/3, dict_delete/2,
         make_leaf/3, key_to_int/1,
	 serialize/1, deserialize/1, pubkey/1, test/0]).
-define(id, accounts).
-include("../../records.hrl").

balance(Account) -> Account#acc.balance.
nonce(Account) -> Account#acc.nonce.
pubkey(Account) -> Account#acc.pubkey.
bets(Account) -> Account#acc.bets.
bets_hash(Account) -> Account#acc.bets_hash.

new(Pub, Balance, Height) ->
    Root0 = constants:root0(),
    #acc{pubkey = Pub, balance = Balance, nonce = 0, height = Height, bets = Root0, bets_hash = oracle_bets:root_hash(Root0)}.

dict_update(Pub, Dict, Amount, NewNonce, NewHeight) ->
    Account = dict_get(Pub, Dict),
    dict_update(Pub, Dict, Amount, NewNonce, NewHeight, Account#acc.bets).
dict_update(Pub, Dict, Amount, NewNonce, NewHeight, Bets) ->
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
    BH = case Bets of
             0 -> Account#acc.bets_hash;
             X -> oracle_bets:root_hash(X)
         end,
    %BH = oracle_bets:root_hash(Bets),
    Account#acc{balance = NewBalance,
                nonce = FinalNonce,
                height = NewHeight,
                bets = Bets,
                bets_hash = BH}.
update_bets(Account, Bets) ->
    Account#acc{bets = Bets,
                bets_hash = oracle_bets:root_hash(Bets)}.
key_to_int(X) ->
    trees:hash2int(ensure_decoded_hashed(X)).
get(Pub, Accounts) ->
    PubId = key_to_int(Pub),
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
    dict_write(Account, 0, Dict).
dict_write(Account, Meta, Dict) ->
    Pub = Account#acc.pubkey,
    Out = dict:store({accounts, Pub}, 
                     {serialize(Account), Meta},
                     Dict),
    Out.
write(Account, Root) ->
    Pub = Account#acc.pubkey,
    SizePubkey = constants:pubkey_size(),
    SizePubkey = size(Pub),
    SerializedAccount = serialize(Account),
    true = size(SerializedAccount) == constants:account_size(),
    KeyLength = constants:key_length(),
    <<Meta:KeyLength>> = <<(Account#acc.bets):KeyLength>>,
    PubId = key_to_int(Pub),
    trie:put(PubId, SerializedAccount, Meta, Root, ?id). % returns a pointer to the new root
delete(Pub0, Accounts) ->
    PubId = key_to_int(Pub0),
    trie:delete(PubId, Accounts, ?id).
dict_delete(Pub, Dict) ->
    dict:store({accounts, Pub}, 0, Dict).
new_balance_dict(Account, Amount, NewHeight, Dict) ->
    OldHeight = Account#acc.height,
    Pub = Account#acc.pubkey,
    HeightDiff = NewHeight - OldHeight,
    Rent = 0,
    Amount + Account#acc.balance - (Rent * HeightDiff).
new_balance(Account, Amount, NewHeight, Trees) ->
    OldHeight = Account#acc.height,
    Governance = trees:governance(Trees),
    Pub = Account#acc.pubkey,
    HeightDiff = NewHeight - OldHeight,
    Rent = 0,
    Amount + Account#acc.balance - (Rent * HeightDiff).

serialize(Account) ->
    true = size(Account#acc.pubkey) == constants:pubkey_size(),
    BalanceSize = constants:balance_bits(),
    HeightSize = constants:height_bits(),
    NonceSize = constants:account_nonce_bits(),
    HS = constants:hash_size()*8,
    BetsRoot = case Account#acc.bets of
                   0 -> Account#acc.bets_hash;
                   X -> oracle_bets:root_hash(X)
               end,
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
            hash:doit(Pub);
        _ ->
            hash:doit(base64:decode(Pub))
    end.
   
make_leaf(Key, V, CFG)  ->
    leaf:new(key_to_int(Key),
             V, 0, CFG).
verify_proof(RootHash, Key, Value, Proof) ->
    trees:verify_proof(?MODULE, RootHash, Key, Value, Proof).
dict_get(Key, Dict) ->
    X = dict:fetch({accounts, Key}, Dict),
    case X of
        0 -> empty;
        {0, _} -> empty;
        {Y, Meta} -> 
            Y2 = deserialize(Y),
            Y2#acc{bets = Meta}
    end.
test() ->
    {Pub, _Priv} = testnet_sign:new_key(),
    Acc = new(Pub, 0, 0),
    S = serialize(Acc),
    Acc1 = deserialize(S),
    Acc = Acc1#acc{bets = Acc#acc.bets},
    Root0 = constants:root0(),
    NewLoc = write(Acc, Root0),
    {Root, Acc, Proof} = get(Pub, NewLoc),
    true = verify_proof(Root, Pub, serialize(Acc), Proof),
    {Root2, empty, Proof2} = get(Pub, Root0),
    true = verify_proof(Root2, Pub, 0, Proof2),
    success.
