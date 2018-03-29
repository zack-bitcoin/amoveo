-module(accounts).
-export([bets/1, update_bets/2, new/2,%custom for this tree
         write/2, get/2, delete/2,%update tree stuff
         dict_update/4, dict_update/5, dict_get/2, dict_write/2, dict_write/3, dict_delete/2,%update dict stuff
	 meta_get/1,
	 verify_proof/4,make_leaf/3,key_to_int/1,serialize/1,test/0, deserialize/1, all_accounts/0]).%common tree stuff
-define(id, accounts).
-include("../../records.hrl").
bets(Account) -> Account#acc.bets.
new(Pub, Balance) ->
    Root0 = constants:root0(),
    #acc{pubkey = Pub, balance = Balance, nonce = 0, bets = Root0, bets_hash = oracle_bets:root_hash(Root0)}.
dict_update(Pub, Dict, Amount, NewNonce) ->
    Account = dict_get(Pub, Dict),
    dict_update(Pub, Dict, Amount, NewNonce, Account#acc.bets).
dict_update(Pub, Dict, Amount, NewNonce, Bets) ->
    Account = dict_get(Pub, Dict),
    OldNonce = Account#acc.nonce,
    FinalNonce = case NewNonce of
                     none ->
                         Account#acc.nonce;
                     NewNonce ->
                         true = NewNonce > OldNonce,
                         NewNonce
                 end,
    NewBalance = Amount + Account#acc.balance,
    true = NewBalance > 0,
    BH = case Bets of
             0 -> Account#acc.bets_hash;
             X -> oracle_bets:root_hash(X)
         end,
    Account#acc{balance = NewBalance,
                nonce = FinalNonce,
                bets = Bets,
                bets_hash = BH}.
update_bets(Account, Bets) ->
    Account#acc{bets = Bets,
                bets_hash = oracle_bets:root_hash(Bets)}.
key_to_int(X) ->
    trees:hash2int(ensure_decoded_hashed(X)).
dict_get(Key, Dict) ->
    %X = dict:fetch({accounts, Key}, Dict),
    X = dict:find({accounts, Key}, Dict),
    case X of
        error -> empty;
        {ok, 0} -> empty;
        {ok, {0, _}} -> empty;
        {ok, {Y, Meta}} -> 
            Y2 = deserialize(Y),
            Y2#acc{bets = Meta}
    end.
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
meta_get(A) -> A#acc.bets.
write(Account, Root) ->
    Pub = Account#acc.pubkey,
    SizePubkey = constants:pubkey_size(),
    SizePubkey = size(Pub),
    SerializedAccount = serialize(Account),
    true = size(SerializedAccount) == constants:account_size(),
    %KeyLength = constants:key_length(),
    %<<Meta:KeyLength>> = <<(Account#acc.bets):KeyLength>>,
    Meta = Account#acc.bets,
    PubId = key_to_int(Pub),
    trie:put(PubId, SerializedAccount, Meta, Root, ?id). % returns a pointer to the new root
dict_delete(Pub, Dict) ->
    dict:store({accounts, Pub}, 0, Dict).
delete(Pub0, Accounts) ->
    PubId = key_to_int(Pub0),
    trie:delete(PubId, Accounts, ?id).

serialize(Account) ->
    true = size(Account#acc.pubkey) == constants:pubkey_size(),
    BalanceSize = constants:balance_bits(),
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
          (Account#acc.pubkey)/binary,
         BetsRoot/binary>>,
    true = size(SerializedAccount) == constants:account_size(),
    SerializedAccount.

deserialize(SerializedAccount) ->
    BalanceSize = constants:balance_bits(),
    NonceSize = constants:account_nonce_bits(),
    SizePubkey = constants:pubkey_size(),
    PubkeyBits = SizePubkey * 8,
    HashSize = constants:hash_size(),
    HashSizeBits = HashSize * 8,
    <<Balance:BalanceSize,
      Nonce:NonceSize,
      Pubkey:PubkeyBits,
      BetsRoot:HashSizeBits>> = SerializedAccount,
    #acc{balance = Balance,
         nonce = Nonce,
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
all_accounts() ->
    %print out a list of all the accounts and their balances.
    Accounts = trees:accounts((tx_pool:get())#tx_pool.block_trees),
    Leafs = trie:get_all(Accounts, accounts),
    A2 = lists:map(fun(A) -> deserialize(leaf:value(A)) end, Leafs),
    A3 = lists:reverse(lists:keysort(2, A2)),
    lists:map(fun(A) -> io:fwrite(integer_to_list(A#acc.balance div 100000000)),
			io:fwrite(" "),
			<<X:80, _/binary>> = base64:encode(A#acc.pubkey),
			io:fwrite(<<X:80>>),
			io:fwrite("\n") end, A3),
    A2.

test() ->
    {Pub, _Priv} = testnet_sign:new_key(),
    Acc = new(Pub, 0),
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
