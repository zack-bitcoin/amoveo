-module(vanity).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	start/1, stop/0, check/0, speed/0,
	start2/2, is_vanity/2, go/0]).
-define(TIMES, 1000).%how many times till we check the gen server if we should stop mining.
-define(LEAD_CHARS, 8). %how close to the front does the word have to appear?
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({store, X}, _) -> {noreply, X};
handle_cast(stop, _) -> {noreply, stop};
handle_cast(go, X) -> {noreply, go};
handle_cast(_, X) -> {noreply, X}.
handle_call(check, _From, X) -> 
    {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.

stop() -> gen_server:cast(?MODULE, stop).
go() -> gen_server:cast(?MODULE, go).
check() -> gen_server:call(?MODULE, check).
store(X) -> gen_server:cast(?MODULE, {store, X}).

start(Word) ->
    go(),
    spawn(fun() -> 
		  Priv = crypto:strong_rand_bytes(32),
		  start2(Word, Priv) end).
		  
start2(Word, Priv) ->
    io:fwrite("start2\n"),
    case check() of
	stop -> fail;
	go -> 
	    case make(Word, Priv, ?TIMES) of
		{ok, X} -> store(X);
		_ -> 
		    <<Y1:256>> = Priv,
		    Y2 = Y1 + 1,
		    Priv2 = <<Y2:256>>,
		    start2(Word, Priv2)
	    end;
	_ -> done
    end.
make(_, _, 0) -> fail;
make(Word, Priv, Times) ->
    {Pub, Priv64} = sign:new_key(base64:encode(Priv)),
    B = is_vanity(Pub, Word),
    if
	B -> {ok, {Pub, Priv64}};
	true -> 
	    <<Y1:24, Rest/binary>> = Priv,
	    %io:fwrite(integer_to_list(Y1)),
	    %io:fwrite("\n"),
	    Y2 = Y1 + 1,
	    Priv2 = <<Y2:24, Rest/binary>>,
	    make(Word, Priv2, Times - 1)
    end.
is_vanity(Pub, Word) ->
    is_vanity(Pub, Word, ?LEAD_CHARS).
is_vanity(Pub, <<>>, N) -> true;
is_vanity(_, _, N) when N < 0 -> false;
is_vanity(<<P1, P2/binary>>, <<P1, W/binary>>, N) ->
    vanity_helper(P1, P2, W, N);
is_vanity(<<P1, P2/binary>>, <<W1, W/binary>>, N) when ((W1 +32) == P1) ->
    vanity_helper(W1, P2, W, N);
is_vanity(<<P1, P2/binary>>, <<W1, W/binary>>, N) ->
    is_vanity(P2, <<W1, W/binary>>, N-1).
vanity_helper(W1, P2, W, N) ->
    B = is_vanity(P2, W, 0),
    if
	B -> true;
	true -> is_vanity(P2, <<W1, W/binary>>, N-1)
    end.
speed() ->
    A = now(),
    Many = 10000,
    speed2(Many),
    D = timer:now_diff(now(), A),
    KPS = Many * 1000000 div D,
    TL = 262144 / KPS,
    io:fwrite(integer_to_list(KPS)),
    io:fwrite(" keys per second\n"),
    io:fwrite(integer_to_list(round(TL))),
    io:fwrite("seconds to make a 3-letter vanity address\n").
speed2(0) -> ok;
speed2(N) -> 
    sign:new_key(base64:encode(<<1000:256>>)),
    speed2(N-1).
