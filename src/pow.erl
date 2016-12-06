-module(pow).
-export([data/1,pow/3,above_min/2,recalculate/2,test/0]).
-record(pow, {data, difficulty = [0,0], nonce}).
data(P) -> P#pow.data.
above_min(P, Min) ->
    true = check_pow(P),
    Diff = P#pow.difficulty,
    Diff >= Min.
check_pow(P) ->
    N = P#pow.nonce,
    Diff = P#pow.difficulty,
    Data = P#pow.data,
    H1 = hash:doit(Data),
    H2 = hash:doit(<<H1/binary, Diff:16, N:80>>),
    I = hash2integer(H2),
    I > Diff.
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
hash2integer(H) -> hash2integer(<<H/binary, 255:8>>, 0).
hash2integer(<<0:128, T/bitstring>>, X) -> hash2integer(T, X+128);
hash2integer(<<0:64, T/bitstring>>, X) -> hash2integer(T, X+64);
hash2integer(<<0:32, T/bitstring>>, X) -> hash2integer(T, X+32);
hash2integer(<<0:16, T/bitstring>>, X) -> hash2integer(T, X+16);
hash2integer(<<0:8, T/bitstring>>, X) -> hash2integer(T, X+8);
hash2integer(<<0:4, T/bitstring>>, X) -> hash2integer(T, X+4);
hash2integer(<<0:2, T/bitstring>>, X) -> hash2integer(T, X+2);
hash2integer(<<0:1, T/bitstring>>, X) -> hash2integer(T, X+1);
hash2integer(<<B:8, _/bitstring>>, X) -> pair2sci([X, B]).
exponent(_, 0) -> 1;
exponent(A, 1) -> A;
exponent(A, N) when N rem 2 == 0 -> 
    exponent(A*A, N div 2);
exponent(A, N) -> 
    A*exponent(A, N-1).
pair2sci([A, B]) ->
    256*A+B.
pair2int([A, B]) ->
    exponent(2, A) * B.
sci2pair(I) ->
    A = I div 256,
    B = I rem 256,
    [A, B].
int2pair(I) -> int2pair(I, 0).
int2pair(X, N) when X rem 2 == 0 ->
    int2pair(X div 2, N+1);
int2pair(X, N) ->
    [N, X].
    
recalculate(OldD, Ratio) ->
    %difficulty is usually stored in scientific notation, so when I calculate the new difficulty, I have to transform to integer, do calculation, and then transform back to scientific notation.
    Old = pair2int(sci2pair(OldD)),
    New = fractions:multiply_int(Ratio, Old),
    pair2sci(int2pair(New)).
    
test() ->
    
    Data = <<5,2,6,0,10>>,
    D = 16,
    D2 = 5,
    [D, D2] = sci2pair(pair2sci([D, D2])),
    [D, D2] = int2pair(pair2int([D, D2])),
    Difficulty = pair2sci([D,D2]),
    %scientific notation, means we did about (2^(D-2))*(D2) hashes
    SearchTimes = round(math:pow(2, D)),
    case pow(Data, Difficulty, SearchTimes) of
	false -> io:fwrite("failed to find a block quick enough\n");
	P ->
	    io:fwrite("found a block\n"),
	    true = above_min(P, Difficulty),
	    false = above_min(P, Difficulty+1),
	    true = check_pow(P),
	    P
    end.
