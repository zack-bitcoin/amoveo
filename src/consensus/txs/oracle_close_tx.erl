-module(oracle_close_tx).
-export([test/0, make/4, doit/3]).
-record(oracle_close, {from, nonce, fee, oracle_id}).
%If there is a lot of open orders for one type of share in an oracle for a long enough period of time, then this transaction can be done.
%This ends betting in the market.
%The fee that was used to start the oracle is the final bet included. It bets against the winning outcome.
make(From, Fee, OID, Accounts) ->
    {_, Acc, _} = account:get(From, Accounts),
    Tx = #oracle_close{from = From, fee = Fee, oracle_id = OID, nonce = account:nonce(Acc) + 1},
    {Tx, []}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    Acc = account:update(Tx#oracle_close.from, Accounts, -Tx#oracle_close.fee, Tx#oracle_close.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Acc),

    OID = Tx#oracle_close.oracle_id,
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(OID, Oracles),
    true = oracles:starts(Oracle) < NewHeight,
    %if the volume of orders in the oracle is too low, then set the oracle:type to 3.
    Result = oracles:type(Oracle),
    Oracle2 = oracles:set_result(Oracle, Result),
    Oracle3 = oracles:set_done_timer(Oracle2, NewHeight),
    io:fwrite("after setting result "),
    io:fwrite(packer:pack(Oracle3)),
    io:fwrite("\n"),
    Oracles2 = oracles:write(Oracle3, Oracles),
    Trees2 = trees:update_oracles(Trees, Oracles2),
    Gov = oracles:governance(Oracle3),
    Trees3 = 
	case Gov of
	    0 -> Trees2;
	    G ->
		GA = oracles:governance_amount(Oracle3),
		case Result of
		    1 -> 
			true = oracles:done_timer(Oracle3) < NewHeight,
			Gov2=governance:change(G, GA, Gov),
			trees:update_governance(Gov2, Trees2);
		    2 ->
			true = oracles:done_timer(Oracle3) < NewHeight,
			Gov2=governance:change(G, -GA,Gov),
			trees:update_governance(Gov2, Trees2);
		    3 -> 
			true = oracle:starts(Oracle3) + constants:maximum_oracle_time() < NewHeight,
			Trees2
		end
	end,
    OraclesEE = trees:oracles(Trees3),
    {_, Oracle4, _} = oracles:get(OID, OraclesEE),
    io:fwrite("after setting result 2 "),
    io:fwrite(packer:pack(Oracle4)),
    io:fwrite("\n"),
    trees:update_accounts(Trees3, NewAccounts).
test() ->
    success.
