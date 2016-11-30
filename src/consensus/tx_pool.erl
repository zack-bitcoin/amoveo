-module(tx_pool).
-behaviour(gen_server).
%this module holds the txs ready for the next block.
%It needs to use txs:digest to keep track of the Accounts and Channels dicts. This module needs to be ready to share either of those dicts.
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, absorb/5,dump/1,secrets/0,accounts/0,channels/0,txs/0,total_coins/0,test/0]).
-record(f, {txs = [], accounts = dict:new(), channels = dict:new(), total_coins = block_tree:total_coins(), secrets = dict:new()}).
init(ok) -> 
    process_flag(trap_exit, true),
    {ok, #f{}}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("block tree died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_call(total_coins, _From, F) -> {reply, F#f.total_coins, F};
handle_call(accounts, _From, F) -> {reply, F#f.accounts, F};
handle_call(channels, _From, F) -> {reply, F#f.channels, F};
handle_call(secrets, _From, F) -> {reply, F#f.secrets, F};
handle_call(txs, _From, F) -> {reply, F#f.txs, F}.
handle_cast({dump, TotalCoins}, _) -> {noreply, #f{total_coins = TotalCoins}};
handle_cast({absorb, NewChannels, NewAccounts, NewTotalCoins, NewSecrets, Txs}, _) ->
    {noreply, #f{txs = Txs, accounts = NewAccounts, channels = NewChannels, total_coins = NewTotalCoins, secrets = NewSecrets}}.
dump(TotalCoins) -> gen_server:cast(?MODULE, {dump, TotalCoins}).
total_coins() -> gen_server:call(?MODULE, total_coins).
accounts() -> gen_server:call(?MODULE, accounts).
channels() -> gen_server:call(?MODULE, channels).
secrets() -> gen_server:call(?MODULE, secrets).
flip(In) -> flip(In, []).
flip([], Out) -> Out;
flip([H|T], Out) -> flip(T, [H|Out]).
txs() -> flip(gen_server:call(?MODULE, txs)).
absorb(A, B, C, D, E) -> gen_server:cast(?MODULE, {absorb, A, B, C, D, E}).
%-record(spend, {from = 0, nonce = 0, to = 0, amount = 0}).
%-record(ca, {from = 0, nonce = 0, pub = <<"">>, amount = 0}).
test() ->
    %{_Pub, _Priv} = testnet_sign:new_key(),
    %CreateAccount = keys:sign(#ca{from = 0, nonce = 1, pub=Pub, amount=12020}),
    %Spend = keys:sign(#spend{from = 0, nonce = 2, to = 1, amount=122}),
    %absorb(CreateAccount),
    %absorb(Spend),
    accounts().
