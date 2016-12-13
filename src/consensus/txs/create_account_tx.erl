-module(create_account_tx).
-export([doit/4, create_account/6]).
-record(ca, {from = 0, to = 0, nonce = 0, address = <<"">>, amount = 0, fee = 0}).

create_account(Addr, Amount, Fee, From, To, Accounts) -> %To is a new ID. set it to any unused ID.
    A = if
	    size(Addr) > 85 -> testnet_sign:pubkey2address(Addr);
	    true -> Addr
	end,
    {_, Acc, Proof} = account:get(From, Accounts),
    Tx = #ca{from = From, to = To, nonce = account:nonce(Acc) + 1, address = A, amount = Amount, fee = Fee},
    {Tx, [Proof]}.
doit(Tx, Channels, Accounts, NewHeight) ->
    To = Tx#ca.to,
    {_RH, empty, _Proof} = account:get(To, Accounts),
    A = Tx#ca.amount,
    From = Tx#ca.from,
    Facc2 = account:update(From, Accounts, -A-Tx#ca.fee, Tx#ca.nonce, NewHeight),
    Nacc = account:new(To, Tx#ca.address, A, NewHeight),
    Accounts2 = account:write(Accounts, Nacc, To),
    NewAccounts = account:write(Accounts2, Facc2, From),
    {Channels, NewAccounts}.

