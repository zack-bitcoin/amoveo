-module(sub_accounts).
-export([new/4,%custom for this tree
         write/2, get/2, delete/2,%update tree stuff
         dict_update/4, dict_get/2, dict_write/2, dict_delete/2,%update dict stuff
         make_key/1, make_key/3,
	 verify_proof/4,make_leaf/3,key_to_int/1,serialize/1,test/0, deserialize/1, all_accounts/0]).%common tree stuff
-define(id, sub_accounts).
-include("../../records.hrl").
new(Pub, Balance, CID, T) ->
    #sub_acc{pubkey = Pub, balance = Balance, nonce = 0, type = T, contract_id = CID}.
dict_update(Key, Dict, Amount, NewNonce) ->
    Account = dict_get(Key, Dict),
    OldNonce = Account#sub_acc.nonce,
    FinalNonce = case NewNonce of
                     none ->
                         Account#sub_acc.nonce;
                     NewNonce ->
                         true = NewNonce > OldNonce,
                         NewNonce
                 end,
    NewBalance = Amount + Account#sub_acc.balance,
    true = NewBalance > 0,
    Account#sub_acc{balance = NewBalance,
                nonce = FinalNonce}.
key_to_int(X) ->
    trees:hash2int(ensure_decoded_hashed(X)).
make_key(#sub_acc{pubkey = Pub, type = T, contract_id = CID}) ->
    make_key(Pub, CID, T).
make_key(Pub, CID, T) ->
    T2 = <<T:256>>,
    hash:doit(<<Pub/binary, CID/binary, T2/binary>>).
dict_get(Key, Dict) ->
    %X = dict:fetch({accounts, Key}, Dict),
    X = dict:find({?id, Key}, Dict),
    case X of
        error -> empty;
        %error -> error;
        {ok, 0} -> empty;
        {ok, {0, _}} -> empty;
        {ok, {Y, Meta}} -> 
            deserialize(Y)
    end.
get(Key, Accounts) ->
    PubId = key_to_int(Key),
    {RH, Leaf, Proof} = trie:get(PubId, Accounts, ?id),
    Account = case Leaf of
                  empty -> empty;
                  Leaf ->
                      deserialize(leaf:value(Leaf))
              end,
    {RH, Account, Proof}.
dict_write(Account, Dict) ->
    Key = make_key(Account),
    Out = dict:store({?id, Key}, 
                     {serialize(Account), 0},
                     Dict),
    Out.
write(Account, Root) ->
    Key = make_key(Account),
    32 = size(Key),
    SerializedAccount = serialize(Account),
    true = size(SerializedAccount) == constants:sub_account_size(),
    PubId = key_to_int(Key),
    trie:put(PubId, SerializedAccount, 0, Root, ?id). % returns a pointer to the new root
dict_delete(Key, Dict) ->
    dict:store({?id, Key}, 0, Dict).
delete(Pub0, Accounts) ->
    PubId = key_to_int(Pub0),
    trie:delete(PubId, Accounts, ?id).

serialize(Account) ->
    true = size(Account#sub_acc.pubkey) == constants:pubkey_size(),
    BalanceSize = constants:balance_bits(),
    NonceSize = constants:account_nonce_bits(),
    HS = constants:hash_size()*8,
    HashSize = constants:hash_size(),
    CID = Account#sub_acc.contract_id,
    32 = size(CID),
    SerializedAccount =
        <<(Account#sub_acc.balance):BalanceSize,
          (Account#sub_acc.nonce):NonceSize,
          (Account#sub_acc.type):32,
          (Account#sub_acc.pubkey)/binary,
          CID/binary>>,
    true = size(SerializedAccount) == constants:sub_account_size(),
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
      Type:32,
      Pubkey:PubkeyBits,
      CID:HashSizeBits
      >> = SerializedAccount,
    #sub_acc{balance = Balance,
             nonce = Nonce,
             pubkey = <<Pubkey:PubkeyBits>>,
             type = Type,
             contract_id = <<CID:HashSizeBits>>}.

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
    Accounts = trees:sub_accounts((tx_pool:get())#tx_pool.block_trees),
    Leafs = trie:get_all(Accounts, ?id),
    A2 = lists:map(fun(A) -> deserialize(leaf:value(A)) end, Leafs),
    A3 = lists:reverse(lists:keysort(2, A2)),
    lists:map(fun(A) -> io:fwrite(integer_to_list(A#sub_acc.balance div 100000000)),
			io:fwrite(" "),
			<<X:80, _/binary>> = base64:encode(A#sub_acc.pubkey),
			io:fwrite(<<X:80>>),
			io:fwrite("\n") end, A3),
    A2.

test() ->
    {Pub, _Priv} = testnet_sign:new_key(),
    Acc = new(Pub, 0, hash:doit(2), 1),
    S = serialize(Acc),
    Acc1 = deserialize(S),
    Acc = Acc1,
    %Root0 = constants:root0(),
    Root0 = trees:empty_tree(?id),
    NewLoc = write(Acc, Root0),
    Key = make_key(Acc),
    {Root, Acc, Proof} = get(Key, NewLoc),
    true = verify_proof(Root, Key, serialize(Acc), Proof),
    {Root2, empty, Proof2} = get(Key, Root0),
    true = verify_proof(Root2, Key, 0, Proof2),
    success.
