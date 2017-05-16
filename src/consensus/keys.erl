%the hard drive stores {f, pubkey, encrypted(privkey), encrypted("sanity")).
%the ram stores either {pubkey, privkey} or {pubkey, ""} depending on if this node is locked.
-module(keys).

-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2, 
	 pubkey/0,sign/2,raw_sign/1,load/4,unlock/1,
	 lock/0,status/0,change_password/2,new/1,
	 shared_secret/1,id/0,update_id/1,address/0,
	 test/0,format_status/2]).
%-define(LOC, "keys.db").
-define(LOC, constants:keys()).
-define(SANE(), <<"sanity">>).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:fwrite("keys died\n"), ok.
%missing function to stop attacks!
format_status(_,[_,_]) -> [{[], [{"State", []}]}].
-record(f, {pub = "", priv = "", sanity = "", id = -1}).
%sanity is only used on the hard drive, not in ram.
init(ok) -> 
    io:fwrite("start keys\n"),
    X = db:read(?LOC),
    Ka = if
	     X == "" -> 
		 {_, Pub, Priv} = 
		     testnet_sign:hard_new_key(),
		 store(Pub, Priv, "", -1),
		 K = #f{pub = Pub, priv=Priv},
		 %K = #f{},
		 %db:save(?LOC,#f{}),
		 K;
	     true -> #f{pub=X#f.pub, id=X#f.id}
	 end,
    {ok, Ka}.
store(Pub, Priv, Brainwallet, Id) -> 
    X = #f{pub=Pub, priv=encryption:encrypt(Priv, Brainwallet), sanity=encryption:encrypt(?SANE(), Brainwallet), id = Id},
    db:save(?LOC, X),
    X.
handle_call({ss, Pub}, _From, R) ->
    {reply, testnet_sign:shared_secret(Pub, R#f.priv), R};
handle_call({raw_sign, _}, _From, R) when R#f.priv=="" ->
    {reply, "need to unlock passphrase", R};
handle_call({raw_sign, M}, _From, X) when not is_binary(M) ->
    {reply, "not binary", X};
handle_call({raw_sign, M}, _From, R) ->
    {reply, testnet_sign:sign(M, R#f.priv), R};
handle_call({sign, M, Accounts}, _From, R) -> 
    {reply, testnet_sign:sign_tx(M, R#f.pub, R#f.priv, R#f.id, Accounts), R};
handle_call(status, _From, R) ->
    Y = db:read(?LOC),
    Out = if
              Y#f.priv == "" -> empty;
              R#f.priv == "" -> locked;
              true -> unlocked
          end,
    {reply, Out, R};
handle_call(pubkey, _From, R) -> {reply, R#f.pub, R};
handle_call(id, _From, R) -> {reply, R#f.id, R}.
handle_cast({load, Pub, Priv, Brainwallet, Id}, _R) ->
    io:fwrite("load 2\n"),
    store(Pub, Priv, Brainwallet, Id),
    {noreply, #f{pub=Pub, priv=Priv, id = Id}};
handle_cast({id_update, Id}, R) -> 
    DB = db:read(?LOC),
    X = DB#f{id = Id},
    db:save(?LOC, X),
    {noreply, #f{pub = R#f.pub, priv = R#f.priv, id = Id}};
handle_cast({new, Brainwallet}, _R) ->
    {_, Pub, Priv} = testnet_sign:hard_new_key(),
    store(Pub, Priv, Brainwallet, -1),
    {noreply, #f{pub=Pub, priv=Priv}};
handle_cast({unlock, Brainwallet}, _) ->
    X = db:read(?LOC),
    
    ?SANE() = encryption:decrypt(X#f.sanity, Brainwallet),
    Priv = encryption:decrypt(X#f.priv, Brainwallet),%err
    {noreply, #f{pub=X#f.pub, priv=Priv, id=X#f.id}};
handle_cast(lock, R) -> {noreply, #f{pub=R#f.pub, id=R#f.id}};
handle_cast({change_password, Current, New}, R) ->
    X = db:read(?LOC),
    ?SANE() = encryption:decrypt(X#f.sanity, Current),
    Priv = encryption:decrypt(X#f.priv, Current),
    store(R#f.pub, Priv, New, X#f.id),
    {noreply, R};
handle_cast(_, X) -> {noreply, X}.
handle_info(_, X) -> {noreply, X}.
pubkey() -> gen_server:call(?MODULE, pubkey).
address() -> testnet_sign:pubkey2address(pubkey()).
%sign(M) -> gen_server:call(?MODULE, {sign, M, tx_pool:accounts()}).
sign(M, Accounts) -> 
    S = status(),
    case S of
	unlocked ->
	    gen_server:call(?MODULE, {sign, M, Accounts});
	_ -> io:fwrite("you need to unlock your account before you can sign transactions. use keys:unlock(\"password\").\n"),
	     {error, locked}
    end.
raw_sign(M) -> gen_server:call(?MODULE, {raw_sign, M}).
load(Pub, Priv, Brainwallet, ID) when is_list(Pub) -> 
    load(list_to_binary(Pub), Priv, Brainwallet, ID);
load(Pub, Priv, Brainwallet, ID) when is_list(Priv) -> 
    load(Pub, list_to_binary(Priv), Brainwallet, ID);
load(Pub, Priv, Brainwallet, ID) when (is_binary(Pub) and is_binary(Priv))-> 
    io:fwrite("load key"),
    gen_server:cast(?MODULE, {load, Pub, Priv, Brainwallet, ID}).
unlock(Brainwallet) -> gen_server:cast(?MODULE, {unlock, Brainwallet}).
lock() -> gen_server:cast(?MODULE, lock).
status() -> gen_server:call(?MODULE, status).
change_password(Current, New) -> gen_server:cast(?MODULE, {change_password, Current, New}).
new(Brainwallet) -> gen_server:cast(?MODULE, {new, Brainwallet}).
shared_secret(Pub) -> gen_server:call(?MODULE, {ss, Pub}).
id() -> gen_server:call(?MODULE, id).
update_id(Id) -> gen_server:cast(?MODULE, {id_update, Id}).
    
test() ->
    unlocked = keys:status(),
    Tx = {spend, 1, 1, 2, 1, 1},
    Stx = sign(Tx, 1),
    %{signed,{spend,1,1,2,1,1},
    %<<"MEQCIHfFk8egH3Jz15NyipyuTxBBY9bP1u078CFn+lhDbsKoAiB4rMgteg8mXXJ2GGbfcvySR7RmoK6xn5kbNoIE88drjw==">>,
    %<<"QkF4eUUvV2htL1NyMG5PTmJjN2pjaXlBZjhvNHZSZXhOc0ovaVZweVRpMmxTd0lMb0ZJTm1JUjNVdDNpMGRTaEIrd1Fz"...>>,
    % [],[],[]},
    true = testnet_sign:verify(Stx, 1),
    success.
