
-module(create_account_tx).
-export([doit/3, make/5]).
-record(ca, {from = 0, pubkey = 0, fee = 0, nonce = 0, amount = 0}).

make(Pubkey, Amount, Fee, From, Trees) -> %To is a new ID. set it to any unused ID.
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Tx = #ca{from = From, pubkey = Pubkey, nonce = accounts:nonce(Acc) + 1, amount = Amount, fee = Fee},
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
    Nacc = accounts:new(Tx#ca.pubkey, A, NewHeight),
    Accounts2 = accounts:write(Accounts, Nacc),
    NewAccounts = accounts:write(Accounts2, Facc2),
    trees:update_accounts(Trees, NewAccounts).

