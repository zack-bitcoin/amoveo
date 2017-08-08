-module(create_account_tx).
-export([doit/3, make/5, from/1, pubkey/1]).
-record(ca, {from = 0, nonce = 0, fee = 0, pubkey = <<"">>, amount = 0}).

from(X) ->
    X#ca.from.
pubkey(X) ->
    X#ca.pubkey.

make(Pub, Amount, Fee, From, Trees) -> %To is a new ID. set it to any unused ID.
    PS = size(Pub),
    PS = size(From),
    PS = constants:pubkey_size(),
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Tx = #ca{from = From, nonce = accounts:nonce(Acc) + 1, pubkey = Pub, amount = Amount, fee = Fee},
    {Tx, [Proof]}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    Pubkey = Tx#ca.pubkey,
    {_RH, empty, _Proof} = accounts:get(Pubkey, Accounts),
    A = Tx#ca.amount,
    From = Tx#ca.from,
    Governance = trees:governance(Trees),
    CAF = governance:get_value(create_account_fee, Governance),
    Facc2 = accounts:update(From, Trees, -A-Tx#ca.fee-CAF, Tx#ca.nonce, NewHeight),
    Nacc = accounts:new(Pubkey, A, NewHeight),
    Accounts2 = accounts:write(Accounts, Nacc),
    NewAccounts = accounts:write(Accounts2, Facc2),
    trees:update_accounts(Trees, NewAccounts).

