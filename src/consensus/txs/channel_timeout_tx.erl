-module(channel_timeout_tx).
-export([doit/7, timeout_channel/2, channel_block/1]).
-record(timeout, {acc = 0, nonce = 0, fee = 0, channel_block = 0}).
%If your partner is not helping you, this is how you start the process of closing the channel. 
%You should only use the final channel-state, or else your partner can punish you for cheating.
channel_block(X) -> X#timeout.channel_block.
timeout_channel(Id, ChannelTx) ->
    Acc = block_tree:account(Id),
    #timeout{acc = Id, nonce = accounts:nonce(Acc) + 1, channel_block = ChannelTx}.

doit(Tx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    SignedCB = Tx#timeout.channel_block, 
    testnet_sign:verify(SignedCB, Accounts),
    CB = testnet_sign:data(SignedCB),
    channel_block_tx:channel(SignedCB, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight),
    Acc = block_tree:account(Tx#timeout.acc, ParentKey, Accounts),
    N = accounts:update(Acc, NewHeight, (- Tx#timeout.fee), 0, 1, TotalCoins),
    Nonce = accounts:nonce(N),
    Nonce = Tx#timeout.nonce,
    Id = channel_block_tx:id(CB),
    NewAccounts = dict:store(Tx#timeout.acc, N, Accounts),
    OldCh = block_tree:channel(Id, ParentKey, Channels),
    Acc1 = channels:acc1(OldCh),
    Acc2 = channels:acc2(OldCh),
    A = if
        Tx#timeout.acc == Acc1 -> 0;
        Tx#timeout.acc == Acc2 -> 1
    end,
    Ch = channels:timeout(OldCh, Tx#timeout.nonce, NewHeight, A),
    NewChannels = dict:store(Id, Ch, Channels),
    {NewChannels, NewAccounts, TotalCoins, S}.
