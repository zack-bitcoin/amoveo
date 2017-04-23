%If a channel has less than 0 money, then anyone can delete it for a small reward.

-module(channel_repo_tx).
-export([make/5,doit/3]).
-record(cr, {from = 0, nonce = 0, fee = 0, id = 0}).

make(From, ID, Fee, Accounts, Channels) ->
    {_, Acc, Proof} = account:get(From, Accounts),
    {_, _, CProof} = channel:get(ID, Channels),
    N = account:nonce(Acc) + 1, 
    Tx = #cr{from = From, nonce = N, fee = Fee, id = ID},
    {Tx, [Proof, CProof]}.

doit(Tx, Trees, NewHeight) ->
    Channels = trees:channels(Trees),
    Accounts = trees:accounts(Trees),
    From = Tx#cr.from,
    CID = Tx#cr.id,
    A = constants:delete_channel_reward(),
    Facc = account:update(From, Accounts, A, Tx#cr.nonce, NewHeight),
    {_, Channel, _} = channel:get(CID, Channels),
    false = channel:closed(Channel),
    B = channel:bal1(Channel) + channel:bal2(Channel),
    DH = NewHeight - channel:last_modified(Channel),
    Rent = constants:channel_rent() * DH,
    true = B =< Rent,
    NewAccounts = account:write(Accounts, Facc),
    NewChannels = channel:delete(CID, Channels),
    Trees2 = trees:update_channels(Trees, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).

