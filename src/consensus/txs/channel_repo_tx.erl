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
    Channels = trees:channels(Trees),
    Accounts = trees:accounts(Trees),
    From = Tx#cr.from,
    CID = Tx#cr.id,
    A = constants:delete_channel_reward(),
    Facc = accounts:update(From, Trees, A, Tx#cr.nonce, NewHeight),
    {_, Channel, _} = channels:get(CID, Channels),
    false = channels:closed(Channel),
    B = channels:bal1(Channel) + channels:bal2(Channel),
    DH = NewHeight - channels:last_modified(Channel),
    Governance = trees:governance(Trees),
    CR = governance:get_value(channel_rent, Governance),
    Rent = CR * DH,
    true = B =< Rent,
    NewAccounts = accounts:write(Accounts, Facc),
    NewChannels = channels:delete(CID, Channels),
    Trees2 = trees:update_channels(Trees, NewChannels),
    trees:update_accounts(Trees2, NewAccounts).

