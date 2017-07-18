-module(coinbase_tx).
-export([doit/3, make/2, from/1]).
-record(coinbase, {from = 0, nonce = 0}).
from(X) -> X#coinbase.from.
make(From, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    Tx = #coinbase{from = From},
    {Tx, [Proof]}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    Governance = trees:governance(Trees),
    BlockReward = governance:get_value(block_reward, Governance),
    From = Tx#coinbase.from,
    {_, X, _} = accounts:get(From, Accounts),
    Nacc = case X of 
	       empty -> accounts:new(From, BlockReward, NewHeight);
	       _ -> accounts:update(From, Trees, BlockReward, none, NewHeight)
	   end,
    NewAccounts = accounts:write(Accounts, Nacc),
    trees:update_accounts(Trees, NewAccounts).
