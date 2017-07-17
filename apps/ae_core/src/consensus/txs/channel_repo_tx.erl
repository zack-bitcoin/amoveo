%If a channel has less than 0 money, then anyone can delete it for a small reward.

-module(channel_repo_tx).
-export([make/4,doit/3]).
-record(cr, {from = 0, nonce = 0, fee = 0, id = 0}).

make(From, ID, Fee, Trees) ->
    Accounts = trees:accounts(Trees),
    Channels = trees:channels(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    {_, _, CProof} = channels:get(ID, Channels),
    N = accounts:nonce(Acc) + 1, 
    Tx = #cr{from = From, nonce = N, fee = Fee, id = ID},
    {Tx, [Proof, CProof]}.

doit(Tx, Trees, NewHeight) ->
    io:fwrite("channel repo doit\n"),
    Channels = trees:channels(Trees),
    Accounts = trees:accounts(Trees),
    From = Tx#cr.from,
    CID = Tx#cr.id,
    A = constants:delete_channel_reward(),
    io:fwrite("channel repo doit 1\n"),
    Facc = accounts:update(From, Trees, A, Tx#cr.nonce, NewHeight),
    io:fwrite("channel repo doit 12\n"),
    {_, Channel, _} = channels:get(CID, Channels),
    io:fwrite("channel repo doit 13\n"),
    false = channels:closed(Channel),
    io:fwrite("channel repo doit 14\n"),
    B = channels:bal1(Channel) + channels:bal2(Channel),
    io:fwrite("channel repo doit 15\n"),
    DH = NewHeight - channels:last_modified(Channel),
    io:fwrite("channel repo doit 16\n"),
    Governance = trees:governance(Trees),
    io:fwrite("channel repo doit 17\n"),
    CR = governance:get_value(channel_rent, Governance),
    io:fwrite("channel repo doit 2\n"),
    Rent = CR * DH,
    true = B =< Rent,
    NewAccounts = accounts:write(Accounts, Facc),
    NewChannels = channels:delete(CID, Channels),
    io:fwrite("channel repo doit 3\n"),
    Trees2 = trees:update_channels(Trees, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).

