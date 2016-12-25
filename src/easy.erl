-module(easy).
-compile(export_all).


sync() ->
    Height = block:height(block:read(top:doit())),
    download_blocks:sync_all(peers:all(), Height).
   
tx_maker(F) -> 
    {Accounts, Channels,_,_} = tx_pool:data(),
    {Tx, _} = F(Accounts, Channels),
    Stx = keys:sign(Tx, Accounts),
    tx_pool_feeder:absorb(Stx).

create_account(NewAddr, Amount, Fee, ID) ->
    F = fun(Accounts, _) ->
		create_account_tx:make(NewAddr, Amount, Fee, keys:id(), ID, Accounts) end,
    tx_maker(F).

spend(ID, Amount, Fee) ->
    F = fun(Accounts, _) ->
		spend_tx:make(ID, Amount, Fee, keys:id(), Accounts) end,
    tx_maker(F).
    
delete_account(ID, Fee) ->
    F = fun(Accounts, _) ->
		delete_account_tx:make(keys:id(), ID, Fee, Accounts) end,
    tx_maker(F).

repo_account(ID, Fee) ->   
    F = fun(Accounts, _) ->
		repo_tx:make(ID, Fee, keys:id(), Accounts) end,
    tx_maker(F).

new_channel(CID, ID, Bal1, Bal2, Rent, Fee) ->
    %be careful that an attacker doesn't make us generate entropy over and over to make the entropy they want.
    Entropy = crypto:strong_rand_bytes(constants:channel_entropy() div 8),
    F = fun(Accounts) ->
		new_channel_tx:make(CID, Accounts, keys:id(), ID, Bal1, Bal2, Rent, Entropy, Fee) end,
    tx_maker(F).

grow_channel(CID, Bal1, Bal2, Rent, Fee) ->
    F = fun(Accounts, Channels) ->
		grow_channel_tx:make(CID, Accounts, Channels, Bal1, Bal2, Rent, Fee) end,
    tx_maker(F).

channel_team_close(CID, Amount, Fee) ->
    F = fun(Accounts, Channels) ->
		channel_team_close_tx:make(CID, Accounts, Channels, Amount, Fee) end,
    tx_maker(F).

channel_repo(CID, Fee) ->
    F = fun(Accounts, Channels) ->
		channel_repo_tx:make(keys:id(), CID, Fee, Accounts, Channels) end,
    tx_maker(F).

channel_solo_close(CID, Fee, SPK, ScriptSig) ->
    F = fun(Accounts, Channels) ->
		channel_solo_close:make(keys:id(), CID, Fee, SPK, ScriptSig, Accounts, Channels) end,
    tx_maker(F).

channel_timeout(CID, Fee) ->
    F = fun(Accounts, Channels) ->
		channel_timeout_tx:make(keys:id(), Accounts, Channels, CID, Fee) end,
    tx_maker(F).

channel_slash(CID, Fee, SPK, SS) ->
    F = fun(Accounts, Channels) ->
		channel_slash_tx:make(keys:id(), CID, Fee, SPK, SS, Accounts, Channels) end,
    tx_maker(F).

account(ID) ->
    {Accounts, _,_,_} = tx_pool:data(),
    {_, A, _} = account:get(ID, Accounts),
    A.

account() -> account(keys:id()).
balance() -> account:balance(account()).





mine() -> block:mine_blocks(10000000000, 200000). 
%second number is how many nonces we try per round.
%first number is how many rounds we do.
