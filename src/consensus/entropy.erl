%This module needs to remember all the revealed secrets as far back as we need entropy from.
-module(entropy).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,doit/1,reveal/3,read/1]).
%-define(LOC, "entropy.db").
-define(LOC, constants:entropy()).
-record(x, {start = 0, entropy = []}).
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Ka = if
	     X == "" -> 
		 K = #x{},
		 db:save(?LOC,K),
		 K;
	     true -> X
	 end,
    {ok, Ka}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, K) -> 
    db:save(?LOC,K),
    io:format("entropy died"),
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({reveal, Height, Order, Secret}, X) -> 
    B = << Order:16, Secret:2 >>,
    NewX=store(X, B, Height),
    {noreply, NewX}.
handle_call({read, Height}, _From, X) -> 
    Out = nth(X#x.entropy, Height - X#x.start),
    {reply, Out, X}.
reveal(Height, Pubkey, Secret) ->
    %pull up the block, make sure it matches secretHash
    CurrentHeight = block_tree:height(),
    H = hash:doit([Height, Pubkey]),
    << Order:16, _/binary>> = H,
    true = is_integer(Secret),
    true = Secret > -1,
    true = Secret < 4,
    true = Height < CurrentHeight - constants:finality(),
    gen_server:cast(?MODULE, {reveal, Height, Order, Secret}).
read(Height) ->
    case gen_server:call(?MODULE, {read, Height}) of
	<<"none">> -> <<"none">>;
	B -> unpack(B)
    end.
unpack(B) -> unpack(B, <<>>).
unpack(<<>>, Out) -> Out;
unpack(<< _:16, Secret:2, R/binary>>, Out) -> 
    unpack(R, <<Secret:2, Out/binary>>).
store(X, Bytes, Height) ->
    H = Height - X#x.start,
    L = store_list(X#x.entropy, H, Bytes),
    #x{start = X#x.start, entropy = L}.
store_list(Entropy, H, Bytes) -> store_list(Entropy, H, Bytes, []).
store_list([X|Entropy], 0, Bytes, Out) -> flip(Out) ++ ordered_store(Bytes, X) ++ Entropy;
store_list([X|Entropy], H, Bytes, Out) -> store_list(Entropy, H - 1, Bytes, [X|Out]).
ordered_store(B, Y) -> ordered_store(B, Y, <<>>).
ordered_store(B, <<>>, Out) -> << Out/binary, B/binary >>;
ordered_store(<< Order:16, Secret:2>>, <<Order:16, _:2, Y/binary>>, Out) ->  <<Out/binary, Order:16, Secret:2, Y/binary>>;
ordered_store(B = << Order:16, _:2>>, <<TOrder:16, TSecret:2, Y/binary>>, Out) when Order > TOrder-> 
    ordered_store(B, Y, <<Out/binary, TOrder:16, TSecret:2>>);
ordered_store(<<Order:16,Secret:2>>, R, Out) ->
    <<Out/binary, Order:16, Secret:2, R/binary>>.
flip(X) -> flip(X, []).
flip([], Out) -> Out;
flip([H|T], Out) -> flip(T, [H|Out]).
nth([H|_], 0) -> H;
nth([_|T], N) -> nth(T, N-1);
nth([], _) -> <<"none">>.
doit(Number) -> 
    %Height = block_tree:height(block_tree:read(top)),
    M = constants:max_reveal() + 2,
    %we should delete all the memory older than M+1 ago.
    H = Number - M,
    read(H).
test() ->
    reveal(-32, 3, 1),
    doit(0).
