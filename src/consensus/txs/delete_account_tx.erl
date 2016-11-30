-module(delete_account_tx).
-export([doit/7, delete_account/3]).
-record(da, {from = 0, nonce = 0, to = 0, fee = 0}).
delete_account(Acc, To, Fee) ->
    A = block_tree:account(Acc),
    Nonce = accounts:nonce(A),
    #da{from = Acc, nonce = Nonce + 1, to = To, fee = Fee}.
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    F = block_tree:account(Tx#da.from, ParentKey, Accounts),
    To = block_tree:account(Tx#da.to, ParentKey, Accounts),
    B = constants:delete_account_reward() + accounts:balance(F),
    true = Tx#da.fee < B,
    NT = accounts:update(To, NewHeight, B - Tx#da.fee, 0, 0, TotalCoins),
    Nonce = accounts:nonce(F) + 1,
    Nonce = Tx#da.nonce,
    Accounts2 = dict:store(Tx#da.to, NT, Accounts),
    Accounts3 = dict:store(Tx#da.from, accounts:empty(), Accounts2),
    {Channels, Accounts3, TotalCoins + constants:delete_account_reward(), S}.

