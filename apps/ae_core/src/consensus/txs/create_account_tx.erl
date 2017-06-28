-module(create_account_tx).
-export([doit/3, make/6]).
-record(ca, {from = 0, to = 0, fee = 0, nonce = 0, address = <<"">>, amount = 0}).

make(Addr, Amount, Fee, From, To, Trees) -> %To is a new ID. set it to any unused ID.
    Accounts = trees:accounts(Trees),
    A = if
	    size(Addr) > 85 -> testnet_sign:pubkey2address(Addr);
	    true -> Addr
	end,
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Tx = #ca{from = From, to = To, nonce = accounts:nonce(Acc) + 1, address = A, amount = Amount, fee = Fee},
    {Tx, [Proof]}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    To = Tx#ca.to,
    {_RH, empty, _Proof} = accounts:get(To, Accounts),
    A = Tx#ca.amount,
    From = Tx#ca.from,
    Governance = trees:governance(Trees),
    CAF = governance:get_value(create_account_fee, Governance),
    Facc2 = accounts:update(From, Trees, -A-Tx#ca.fee-CAF, Tx#ca.nonce, NewHeight),
    Nacc = accounts:new(To, Tx#ca.address, A, NewHeight),
    Accounts2 = accounts:write(Accounts, Nacc),
    NewAccounts = accounts:write(Accounts2, Facc2),
    KID = keys:id(),
    MyAddress = keys:address(),
    KID = keys:id(),
    if
	KID < 1 -> keys:update_id(Tx#ca.to);
	(Tx#ca.address) == MyAddress ->
	    if
		KID < 1 -> keys:update_id(Tx#ca.to);
		true ->
		    {_, MyAccount, _} = accounts:get(keys:id(), Accounts),
		    case MyAccount of
			empty -> keys:update_id(Tx#ca.to);
			MA ->
			    CurrentBal = accounts:balance(MA),
			    if 
				(A > CurrentBal) ->
				    keys:update_id(Tx#ca.to);
				true -> ok
			    end
		    end
		end;
	true -> ok
    end,
    trees:update_accounts(Trees, NewAccounts).

