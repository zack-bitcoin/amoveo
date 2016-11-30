-module(all_secrets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,add/2,exists/2,remove/2,check/0,reset/0]).
%-define(LOC, "all_secrets.db").
-define(LOC, constants:all_secrets()).
-define(Start, 0).
-record(x, {start = ?Start, blocks = [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]]}).%length of the empty lists is constants:max_reveal - constants:min_reveal + 2.
save(DB) -> db:save(?LOC, database2bytes(DB)).
read() -> db:read(?LOC).
init(ok) -> 
    process_flag(trap_exit, true),
    X = read(),
    Ka = if
	     X == "" -> 
		 K = #x{},
		 save(K),
		 K;
	     true -> bytes2database(X)
	 end,
    {ok, Ka}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    save(K),
    io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
remove_sh(X, [X|T]) -> T;
remove_sh(X, [H|T]) -> [H|remove_sh(X, T)].
replace_height(0, [_|T], NewBlock) -> [NewBlock|T];
replace_height(N, [Block|T], NewBlock) -> [Block|replace_height(N-1, T, NewBlock)].
handle_cast(reset, _) ->
    {noreply, bytes2database(read())};
handle_cast({remove, Height, SH}, X) -> 
    H = Height - X#x.start,
    NewX = #x{start = X#x.start, blocks = replace_height(H, X#x.blocks, remove_sh(SH, nth(H, X#x.blocks)))},
    save(NewX),
    {noreply, NewX};
handle_cast({add, Height, SH}, X) -> 
    if
	Height < X#x.start  -> {noreply, X};
	true ->
	    Gap = constants:max_reveal() - constants:min_reveal(),
	    Start = X#x.start,
	    NewStart = max(Height - Gap + 1, Start),
	    H = Height-NewStart,
	    Keep = buffer(Gap+5, remove_front(NewStart - Start, X#x.blocks)),
	    NewX = #x{start = NewStart, blocks = replace_height(H, Keep, [SH|nth(H, Keep)])},
	    save(NewX),
	    {noreply, NewX}
    end.
buffer(Width, L) ->
    if
	length(L) >= Width -> L;
	true -> buffer(Width, L ++ [[]])
    end.
remove_front(0, T) -> T;
remove_front(X, [_|T]) -> remove_front(X-1, T).
nth(0, [H|_]) -> H;
nth(N, [_|T]) -> nth(N-1, T).
in_list(_, []) -> false;
in_list(X, [X|_]) -> true;
in_list(X, [_|T]) -> in_list(X, T).
handle_call(check, _From, X) -> {reply, X, X};
handle_call({exists, Height, SH}, _From, X) -> 
    H = Height - X#x.start,
    Gap = constants:max_reveal() - constants:min_reveal(),
    O = if
	    H > Gap -> false;
	    Height < X#x.start -> false;
	    true -> 
		Block = nth(H, X#x.blocks),
		in_list(SH, Block)
	end,
    {reply, O, X}.
check() -> gen_server:call(?MODULE, check).
exists(Height, SH) -> 
    if
	Height < 1 -> false;
	true ->
	    gen_server:call(?MODULE, {exists, Height, SH})
    end.
add(Height, SH) -> 
    E = exists(Height, SH),
    if 
	E -> ok;
	true -> gen_server:cast(?MODULE, {add, Height, SH})
    end.
remove(Height, SH) ->
    E = exists(Height, SH),
    if
	E -> gen_server:cast(?MODULE, {remove, Height, SH});
	true -> ok
    end.
reset() -> gen_server:cast(?MODULE, reset).

%234 blocks between minreveal and maxreveal * 54 secrets per block * 32 bytes per secret = 404352 bytes = ~1/2 megabyte, so we can keep it in ram.
%once a secret gets revealed, we should remove it.
%This is like accounts and blocks. This module stores how it looked at finality. Recent changes are in the blocktree.
%we should garbage collect everything before maxreveal.

%db = {Start, List} %Start is the start from where garbage collection happened. List is about 234 long, one for each height between minreveal and maxreveal.
%List = [SecretList1, SecretList2, ...] 
%SecretList = [SecretHash, SecretHash, ...] this is the secrets from a single block.
secrets2bytes([]) -> <<>>; %<<1a, 1b, ... 1z>>
secrets2bytes([Secret|T]) ->
    S = secrets2bytes(T),
    <<Secret/binary, S/binary>>.
list2bytes([]) -> <<>>; %<<Size1, 1a, 1b, ... 1z, Size2, 2a, 2b, ...>>
list2bytes([Secrets|T]) -> 
    S = secrets2bytes(Secrets),
    L = list2bytes(T),
    A = length(Secrets),
    <<A:8, S/binary, L/binary>>.
database2bytes(X) ->
    Start = X#x.start,
    List = X#x.blocks,
    %<<Height:38, Size1, 1a, 1b, ... 1z, Size2, 2a, 2b, ...>>
    L = list2bytes(List),
    <<Start:38, L/binary>>.
bytes2secrets(<<>>) -> [];
bytes2secrets(<<S:256, T/binary>>) -> [<<S:256>>|bytes2secrets(T)];
bytes2secrets(X) -> io:fwrite(X).
bytes2list(<<>>) -> [];
bytes2list(<<L:8, X/binary>>) -> bytes2list2(L, X).
bytes2list2(L, X) ->
    M = 256 * L,
    <<BinarySecrets:M, Y/binary>> = X,
    [bytes2secrets(<<BinarySecrets:M>>)|bytes2list(Y)].
    
bytes2database(B) ->
    <<Start:38, X/binary>> = B,
    L = bytes2list(X),
    #x{start = Start, blocks = L}.

test() ->
    DB = [[hash:doit(1), hash:doit(2)],[hash:doit(3), hash:doit(4)]],
    S = 4,
    Database = #x{start = S, blocks = DB},
    A = database2bytes(Database),
    #x{start = S, blocks = DB} = bytes2database(A),
    Database = bytes2database(A),
    add(4, hash:doit(5)),
    add(6, hash:doit(5)),
    add(5, hash:doit(6)),
    add(6, hash:doit(6)),
    false = exists(3, hash:doit(5)),
    true = exists(4, hash:doit(5)),
    true = exists(5, hash:doit(6)),
    true = exists(6, hash:doit(5)),
    true = exists(6, hash:doit(6)),
    remove(6, hash:doit(5)),
    remove(6, hash:doit(5)),
    false = exists(6, hash:doit(5)),
    true = exists(4, hash:doit(5)),
    add(250, hash:doit(1)),
    success.
