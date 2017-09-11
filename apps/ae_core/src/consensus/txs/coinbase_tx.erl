-module(coinbase_tx).
-export([doit/3, go/3, make/2, from/1]).
-record(coinbase, {from = 0, nonce = 0, fee = 0}).
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
    NewAccounts = accounts:write(Nacc, Accounts),
    trees:update_accounts(Trees, NewAccounts).
go(Tx, Dict, NewHeight) ->
    From = Tx#coinbase.from,
    X = accounts:dict_get(From, Dict),
    BlockReward = governance:dict_get_value(block_reward, Dict),
    Nacc = case X of
               empty -> accounts:new(From, BlockReward, NewHeight);
               _ -> accounts:dict_update(From, Dict, BlockReward, none, NewHeight)
           end,
    accounts:dict_write(Nacc, Dict).
