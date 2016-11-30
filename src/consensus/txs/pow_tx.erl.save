-module(pow_tx).
-export([doit/7, pow/4, test/0, exponent/1]).
-record(pow, {id = 0, nonce = 0, amount = 0, fee = 0, recent_hash = 0, difficulty = 0, mining_nonce = 0}).
pow(Goal, TimeLimit, Difficulty, RecentHash) ->
    Id = keys:id(),
    Acc = block_tree:account(Id),
    Tx = #pow{id = Id, nonce = accounts:nonce(Acc) + 1, amount = Goal, fee = 0, mining_nonce = crypto:rand_bytes(8), recent_hash = RecentHash, difficulty = Difficulty},
    S = size(RecentHash),
    S = 32,
    case work(Tx, TimeLimit, now(), 0) of
	{ok, T, N} -> {T, N};
	    %tx_pool_feeder:absorb(keys:sign(T));
	{error, R} -> R
    end.
	    
work(Tx, TimeLimit, Start, N) ->
    X = timer:now_diff(now(), Start),
    W = enough_work(Tx),
    if 
	W -> {ok, Tx, N};
	X > TimeLimit -> {error, time_limit};
	true ->
	    NTx = #pow{id = Tx#pow.id, nonce = Tx#pow.nonce, amount = Tx#pow.amount, fee = Tx#pow.fee, mining_nonce = next_nonce(Tx#pow.mining_nonce), recent_hash = Tx#pow.recent_hash, difficulty = Tx#pow.difficulty},
	    work(NTx, TimeLimit, Start, N+1)
    end.
enough_work(Tx) ->
    %reward is an account id.
%80 bytes total, {4:version, 32:hashPrevBlock, 32:MerkleRoot, 4:time, 4:difficulty, 4:nonce}
    <<Nonce1:32, Nonce2:32>> = Tx#pow.mining_nonce,
    Header = <<1:32, (Tx#pow.id):256, (Tx#pow.recent_hash)/binary, Nonce1:32, (Tx#pow.difficulty):32, Nonce2:32>>,
    H = hash:hash(Header),
    D = exponent(Tx#pow.difficulty),
    %D is how many hashes it takes to produce 1 coin.
    enough(H, D * Tx#pow.amount).
enough(_, 0) -> true;
enough(<<1:1, _/bitstring>>, _) -> false;
enough(<<0:1, B/bitstring>>, D) -> enough(B, D div 2).
%input of exponent is in range 0-2^32, output needs at least range 0-2^100
exponent(X) -> X*X*X*X.
%sqrt(X) -> sqrt(X, 1, 50).
%sqrt(_, X, 0) -> X;
%sqrt(X, Y, N) -> sqrt(X, (Y+(X div Y)) div 2, N-1).
next_nonce(<<A, B, C, D, E, F, G, X>>) when not(X == 255) -> 
    <<A, B, C, D, E, F, G, (X+1):8>>;
next_nonce(<<A, B, C, D, E, F, X, 255>>) when not(X == 255) -> 
    <<A, B, C, D, E, F, (X+1):8, 0:8>>;
next_nonce(<<A, B, C, D, E, X, 255, 255>>) when not(X == 255) -> 
    <<A, B, C, D, E, (X+1):8, 0:16>>;
next_nonce(<<A, B, C, D, X, 255, 255, 255>>) when not(X == 255) -> 
    <<A, B, C, D, (X+1):8, 0:24>>;
next_nonce(<<A, B, C, X, 255, 255, 255, 255>>) when not(X == 255) -> 
    <<A, B, C, (X+1):8, 0:32>>;
next_nonce(<<A, B, X, 255, 255, 255, 255, 255>>) when not(X == 255) -> 
    <<A, B, (X+1):8, 0:40>>;
next_nonce(<<A, X, 255, 255, 255, 255, 255, 255>>) when not(X == 255) -> 
    <<A, (X+1):8, 0:48>>;
next_nonce(<<X, 255, 255, 255, 255, 255, 255, 255>>) when not(X == 255)-> 
    <<(X+1):8, 0:56>>;
next_nonce(<<255, 255, 255, 255, 255, 255, 255, 255>>) -> 
    <<0:64>>.
doit(Tx, ParentKey, Channels, Accounts, TotalCoins, S, NewHeight) ->
    Goal = Tx#pow.amount,
    %true = enough_work(Tx, Difficulty),

    %From = Tx#pow.from,
    %false = From == Tx#pow.to,
    %To = block_tree:account(Tx#pow.to, ParentKey, Accounts),
    %F = block_tree:account(Tx#pow.from, ParentKey, Accounts),
    %A = Tx#pow.amount,
    %NT = accounts:update(To, NewHeight, A, 0, 0, TotalCoins),
    %NF = accounts:update(F, NewHeight, -A - Tx#pow.fee, 0, 1, TotalCoins),
    %Nonce = accounts:nonce(NF),
    %Nonce = Tx#pow.nonce,
    %Accounts2 = dict:store(Tx#pow.to, NT, Accounts),
    %{Channels, dict:store(Tx#pow.from, NF, Accounts2), TotalCoins, S}.
    ok.

test() ->
    %pow(Goal, TimeLimit, Difficulty, RecentHash)
    Difficulty = 10,
    Goal = 100,
    %number of hashes we have to do is about Goal*sqrt(Difficulty^3)
    pow(Goal, 5000000, Difficulty, <<0:256>>).
