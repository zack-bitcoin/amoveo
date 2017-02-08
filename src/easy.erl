-module(easy).
-compile(export_all).

-define(Fee, free_constants:tx_fee()).

sync() ->
    Height = block:height(block:read(top:doit())),
    download_blocks:sync_all(peers:all(), Height),
    sync2(Height, 100).
sync2(_Height, 0) -> ok;
sync2(Height, N) ->
    timer:sleep(100),
    Height2 = block:height(block:read(top:doit())),
    if
	Height2 > Height -> 
	    timer:sleep(1400),
	    sync();
	true -> sync2(Height, N-1)
   end. 
   
tx_maker(F) -> 
    {Accounts, Channels,_,_} = tx_pool:data(),
    {Tx, _} = F(Accounts, Channels),
    case keys:sign(Tx, Accounts) of
	{error, locked} -> ok;
	Stx -> tx_pool_feeder:absorb(Stx)
    end.
create_account(NewAddr, Amount, ID) ->
    create_account(NewAddr, Amount, ?Fee, ID).
create_account(NewAddr, Amount, Fee, ID) ->
    F = fun(Accounts, _) ->
		create_account_tx:make(NewAddr, Amount, Fee, keys:id(), ID, Accounts) end,
    tx_maker(F).
spend(ID, Amount) ->
    K = keys:id(),
    if 
	ID == K -> io:fwrite("you can't spend money to yourself\n");
	true -> 
	    A = round(Amount * constants:token_decimals()),
	    spend(ID, A, ?Fee)
    end.
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
new_channel(Bal1, Bal2, Fee) ->
    {_,Channels, _,_} = tx_pool:data(),
    CID = new_channel2(1, Channels),
    new_channel(constants:server_ip(), constants:server_port(), CID, Bal1, Bal2, 0, Fee).
new_channel2(ID, Channels) ->
    <<X:8>> = crypto:strong_rand_bytes(1),
    case channel:get(ID+X, Channels) of
	{_, empty, _} -> ID+X;
	X -> new_channel2(ID+256, Channels)
    end.
new_channel(IP, Port, CID, Bal1, Bal2, Rent, Fee) ->
    internal_handler:doit({new_channel, IP, Port, CID, Bal1, Bal2, Rent, Fee}).
channel_balance() ->
    {ok, Other} = talker:talk({id}, constants:server_ip(), constants:server_port()),
    {ok, CD} = channel_manager:read(Other),
    SSPK = channel_feeder:them(CD),
    SPK = testnet_sign:data(SSPK),
    SS = channel_feeder:script_sig_them(CD),
    {Accounts, Channels, NewHeight, _Txs} = tx_pool:data(),
    {Amount, _} = spk:run(fast, SS, SPK, NewHeight, 0, Accounts, Channels),
    CID = spk:cid(SPK),
    Channel = channel:get(CID, Channels),
    channel:bal1(Channel)-Amount.
dice(Amount) ->
    internal_handler:doit({dice, Amount, constants:ip(), constants:port()}).
    
    
    
    

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
    case account:get(ID, Accounts) of
	{_,empty,_} ->
	    io:fwrite("this account does not yet exist\n"),
	    account:new(-1,0,0,0);
	{_, A, _} -> A
    end.

account() -> account(keys:id()).
integer_balance() -> account:balance(account()).
balance() ->
    I = integer_balance(),
    F = I / constants:token_decimals(),
    io_lib:format("~.8f", [F]).
off() -> testnet_sup:stop().

%mine() ->
%    mine:start().
    %mine(10000000000).
%mine(N) -> 
    %sync(),
    %block:mine_blocks(N, 100000, 30). 
%second number is how many nonces we try per round.
%first number is how many rounds we do.
