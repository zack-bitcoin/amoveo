-module(accounts).

-export([balance/1, nonce/1, height/1, pubkey/1, bets/1, shares/1,
         root_hash/1, new/3, update/5, update_bets/2,
         get/2, write/2, delete/2,
         now_balance/4,
         receive_shares/4, send_shares/4,
         pub_decode/1,
         serialize/1, test/0]).

-record(acc, {balance = 0, %% Amount of money you have
              nonce = 0, %% Increments with every tx you put on the chain 
              height = 0, %% The last height at which you paid the tax
              pubkey = [],
              bets = 0, %% Pointer to the merkel tree that stores how many bets you have made in each oracle.
              shares = 0}). %% Pointer to a merkel tree that stores how many shares you have at each price.

-define(id, accounts).

balance(Account) -> Account#acc.balance.
nonce(Account) -> Account#acc.nonce.
height(Account) -> Account#acc.height.
pubkey(Account) -> Account#acc.pubkey.
bets(Account) -> Account#acc.bets.
shares(Account) -> Account#acc.shares.

root_hash(Accounts) ->
    trie:root_hash(?id, Accounts).

new(Pub, Balance, Height) ->
    #acc{pubkey = Pub, balance = Balance, nonce = 0, height = Height, bets = 0, shares = 0}.

update(Pub, Trees, Amount, NewNonce, NewHeight) ->
    DecodedPub = pub_decode(Pub),
    Accounts = trees:accounts(Trees),
    {_, Acc, _} = get(DecodedPub, Accounts),
    update(DecodedPub, Trees, Amount, NewNonce, NewHeight, Acc#acc.shares, Acc#acc.bets).
update(Pub, Trees, Amount, NewNonce, NewHeight, Shares, Bets) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _} = get(Pub, Accounts),
    OldNonce = Acc#acc.nonce,
    FinalNonce = case NewNonce of
                     none ->
                         Acc#acc.nonce;
                     N ->
                         true = N > OldNonce,
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

update_bets(Account, B) ->
    Account#acc{bets = B}.

get(Pub, Accounts) ->
    HashedPub = pub_decode(Pub),
    KeyLength = constants:key_length(),
    DoubledKeyLength = KeyLength * 2,
    PubId = trees:hash2int(HashedPub),
    {RH, Leaf, Proof} = trie:get(PubId, Accounts, ?id),
    V = case Leaf of
            empty ->
                empty;
            L ->
                X = deserialize(leaf:value(L)),
                Meta = leaf:meta(L),
                <<Bets:KeyLength, Shares:KeyLength>> = <<Meta:DoubledKeyLength>>,
                X#acc{bets = Bets, shares = Shares}
        end,
    {RH, V, Proof}.

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

now_balance(Acc, Amount, NewHeight, Trees) ->
    OldHeight = Acc#acc.height,
    Governance = trees:governance(Trees),
    Pub = Acc#acc.pubkey,
    DH = NewHeight - OldHeight,
    MasterPub = constants:master_pub(),
    Rent =
        case Pub of
            MasterPub ->
                -(governance:get_value(developer_reward, Governance));
            _ ->
                0
        end,
    Amount + Acc#acc.balance - (Rent * DH).

receive_shares(Acc, Shares, Height, Trees) ->
    SharesTree = Acc#acc.shares,
    {Tokens, NewTree} = shares:receive_shares(Shares, SharesTree, Height, Trees),
    Acc#acc{shares = NewTree, balance =Acc#acc.balance + Tokens}.

send_shares(Acc, Shares, Height, Trees) ->
    SharesTree = Acc#acc.shares,
    {Tokens, NewTree} = shares:send_shares(Shares, SharesTree, Height, Trees),
    Acc#acc{shares = NewTree, balance = Acc#acc.balance + Tokens}.

serialize(A) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    Pubkey = A#acc.pubkey,

    SizePubkey = constants:pubkey_size(),
    SizePubkey = size(Pubkey),
    Nbits = constants:account_nonce_bits(),
    SharesRoot = shares:root_hash(A#acc.shares),
    BetsRoot = oracle_bets:root_hash(A#acc.bets),
    HS = constants:hash_size(),
    HS = size(BetsRoot),
    HS = size(SharesRoot),
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


%% Internals

deserialize(A) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    HS = constants:hash_size(),
    HD = HS*8,
    Nbits = constants:account_nonce_bits(),
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

pub_decode(Pub) ->
    HS = constants:hash_size(),
    SizePubkey = constants:pubkey_size(),
    case size(Pub) of
        HS ->
            Pub;
        SizePubkey ->
            testnet_hasher:doit(Pub);
        _ ->
            lager:warning("Pub decode problem: ~p", [Pub]),
            testnet_hasher:doit(base64:decode(Pub))
    end.


%% Unused

verify_proof(RootHash, Path, Proof) ->
    KL = constants:key_length(),
    MetaSize = KL div 4,%all in bytes
    HashSize = constants:hash_size(),
    IDSize = constants:hash_size(),
    ValueSize = constants:account_size(),
    PathSize = constants:hash_size(),
    CFG = cfg:new(PathSize, ValueSize, IDSize, MetaSize, HashSize),
    verify:proof(RootHash, Path, Proof, CFG).


%% Tests

test() ->
    {Pub, _Priv} = testnet_sign:new_key(),
    Acc = new(Pub, 0, 0),
    S = serialize(Acc),
    Acc = deserialize(S),
    NewLoc = write(0, Acc),
    {_, Acc, _} = get(Pub, NewLoc),
    success.
