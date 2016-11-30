-module(channel_slash_tx).
-export([doit/7, channel_block/1, make_tx/3]).
-record(channel_slash, {acc = 0, nonce = 0, channel_block = 0, fee=0}).
channel_block(Tx) ->
    Tx#channel_slash.channel_block.
make_tx(Id, CB, Fee) ->
    %Id = keys:id(),
    Acc = block_tree:account(Id),
    Nonce = accounts:nonce(Acc),
    #channel_slash{acc = Id, nonce = Nonce + 1, channel_block = CB, fee = Fee}.
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, Secrets, NewHeight) ->
    SignedCB = Tx#channel_slash.channel_block,
    testnet_sign:verify(SignedCB, Accounts),
    CB = testnet_sign:data(SignedCB),
    Id = channel_block_tx:id(CB),
    Channel = block_tree:channel(Id, ParentKey, Channels),
    A = case channels:called_timeout(Channel) of
	    0 -> B = Tx#channel_slash.acc, B = channels:acc2(Channel), B;
	    1 -> B = Tx#channel_slash.acc, B = channels:acc1(Channel), B
    end,
    OriginTimeout = channel_block_tx:origin_tx(channels:timeout_height(Channel), ParentKey, Id),
    SignedOriginTx = channel_timeout_tx:channel_block(testnet_sign:data(OriginTimeout)),
    OriginTx = testnet_sign:data(SignedOriginTx),

    SlashedCB = channel_block_tx:slash_bet(CB),
    SignedSlashedCB = testnet_sign:empty(SlashedCB),
    NewReveal = channel_block_tx:reveal_union(SlashedCB, testnet_sign:revealed(SignedCB), OriginTx, testnet_sign:revealed(SignedOriginTx)),
    NewSignedCB = testnet_sign:set_revealed(SignedSlashedCB, NewReveal),
    SlasherType1 = (channel_block_tx:nonce(CB) > channel_block_tx:nonce(OriginTx)),
    SlasherType2 = not(testnet_sign:revealed(SignedCB) == NewReveal),
    true = SlasherType1 or SlasherType2,
    Acc = block_tree:account(A, ParentKey, Accounts),
    NAcc = accounts:update(Acc, NewHeight, -Tx#channel_slash.fee, 0, 1, TotalCoins),
    NewAccounts = dict:store(A, NAcc, Accounts),

    Nonce = accounts:nonce(NAcc),
    Nonce = Tx#channel_slash.nonce,

    channel_block_tx:channel(NewSignedCB, ParentKey, Channels, NewAccounts, TotalCoins, Secrets, NewHeight).
