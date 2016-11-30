-module(pow).
-export([data/1,pow/3,above_min/2,test/0]).
-record(pow, {data, difficulty = [0,0], nonce}).
data(P) -> P#pow.data.
above_min(P, [Mina,Minb]) ->
    true = check_pow(P),
    [Diffa, Diffb] = P#pow.difficulty,
    if
	Diffa < Mina -> false;
	Diffa == Mina -> Diffb >= Minb;
	true -> true
    end.
check_pow(P) ->
    N = P#pow.nonce,
    [Diffa, Diffb] = P#pow.difficulty,
    Data = P#pow.data,
    H1 = hash:doit(Data),
    H2 = hash:doit(<<H1/binary, Diffa:8, Diffb:8, N:80>>),
    %H2 = hash:doit([H1, Diffa, Diffb, N]),
    [Ia,Ib] = hash2integer(H2),
    if
	Ia > Diffa -> true;
	Ia == Diffa -> Ib > Diffb;
	true -> false
    end.
pow(Data, Difficulty, Times) ->
    %bitcoin is 1,500,000 terahashes per second or 900,000,000,000,000,000,000 hashes per 10 minutes
    %in 10 years, bitcoin will find a collision of 88.6 bits. 
    T = math:pow(10,23),
    R = round(random:uniform() * T),
    pow2(Data, Difficulty, R, Times).
pow2(Data, Difficulty, Nonce, Times) ->
    P = #pow{data = Data, difficulty = Difficulty, nonce = Nonce},
    B = check_pow(P),
    if
	Times < 1 -> false;
	B -> P;
	true -> pow2(Data, Difficulty, Nonce+1, Times-1)
    end.
hash2integer(H) -> hash2integer(H, 0).
hash2integer(<<0:128, T/bitstring>>, X) -> hash2integer(T, X+128);
hash2integer(<<0:64, T/bitstring>>, X) -> hash2integer(T, X+64);
hash2integer(<<0:32, T/bitstring>>, X) -> hash2integer(T, X+32);
hash2integer(<<0:16, T/bitstring>>, X) -> hash2integer(T, X+16);
hash2integer(<<0:8, T/bitstring>>, X) -> hash2integer(T, X+8);
hash2integer(<<0:4, T/bitstring>>, X) -> hash2integer(T, X+4);
hash2integer(<<0:2, T/bitstring>>, X) -> hash2integer(T, X+2);
hash2integer(<<0:1, T/bitstring>>, X) -> hash2integer(T, X+1);
hash2integer(<<B:8, _/bitstring>>, X) -> [X, B].
test() ->
    Data = <<5,2,6,0,10>>,
    D = 16,
    D2 = 5,
    Difficulty = [D, D2],
    %scientific notation, [A,B] means we did about (2^(A-2))*B hashes
    SearchTimes = round(math:pow(2, D)),
    case pow(Data, Difficulty, SearchTimes) of
	false -> io:fwrite("failed to find a block quick enough\n");
	P ->
	    io:fwrite("found a block\n"),
	    true = above_min(P, Difficulty),
	    false = above_min(P, [D,D2+1]),
	    false = above_min(P, [D+1,D2]),
	    true = check_pow(P),
	    P
    end.
