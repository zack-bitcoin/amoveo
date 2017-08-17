-module(existence_tx).
-export([doit/3, go/3, make/4, from/1, commit/1]).
-record(ex, {from, nonce = 0, fee = 0, commit = 0}).

from(X) -> X#ex.from.
commit(X) -> existence:hash(X#ex.commit).
make(From, Fee, C, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Nonce = accounts:nonce(Acc) + 1,
    %C = existence:new(testnet_hasher:doit(Data)),
    Tx = #ex{from = From,fee=Fee,nonce=Nonce,commit=C},
    {Tx, [Proof]}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    Commits = trees:existence(Trees),
    From = Tx#ex.from,
    C = Tx#ex.commit,
    {_, empty, _} =existence:get(existence:hash(C),Commits),
    NewCommits = existence:write(C, Commits),
    Acc = accounts:update(From, Trees, -Tx#ex.fee, Tx#ex.nonce, NewHeight),
    NewAccounts = accounts:write(Accounts, Acc),
    Trees2 = trees:update_accounts(Trees, NewAccounts),
    trees:update_existence(Trees2, NewCommits).
go(Tx, Dict, NewHeight) ->
    Dict.

    
