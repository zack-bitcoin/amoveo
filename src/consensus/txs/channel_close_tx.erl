%If you did not get slashed, and you waited delay since channel_timeout, then this is how you close the channel and get the money out.

-module(channel_close_tx).
-export([doit/7, slow_close/2, id/1]).
-record(channel_close, {acc = 0, nonce = 0, id = 0, fee = 0}).
id(X) -> X#channel_close.id.

doit(Tx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    Id = Tx#channel_close.id,
    Channel = block_tree:channel(Id, ParentKey, Channels),
    SignedOriginTimeout = channel_block_tx:origin_tx(channels:timeout_height(Channel), ParentKey, Id),
    OriginTimeout = testnet_sign:data(SignedOriginTimeout),
    SignedOriginTx = channel_timeout_tx:channel_block(OriginTimeout),
    OriginTx = testnet_sign:data(SignedOriginTx),
    T = block_tree:read(top),
    Top = block_tree:height(T),
    true = channels:timeout_height(Channel) < (Top - channel_block_tx:delay(OriginTx) + 1),
    Acc = block_tree:account(Tx#channel_close.acc, ParentKey, Accounts),
    NAcc = accounts:update(Acc, NewHeight, -Tx#channel_close.fee, 0, 1, TotalCoins),
    NewAccounts = dict:store(Tx#channel_close.acc, NAcc, Accounts),
    Nonce = accounts:nonce(NAcc),
    Nonce = Tx#channel_close.nonce,
    channel_block_tx:channel(SignedOriginTx, ParentKey, Channels, NewAccounts, TotalCoins, S, NewHeight).
slow_close(Id, MyId) ->
    Acc = block_tree:account(MyId),
    #channel_close{acc = MyId, nonce = accounts:nonce(Acc) + 1, id = Id}.

