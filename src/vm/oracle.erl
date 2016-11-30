-module(oracle).
-export([doit/2, test/0]).
%4 ways you can vote.
%1) True 1
%2) False -1
%3) Need more time 0
%4) Bad Question 2

    %make a list of the outcomes.
    %make a list of how often each person was wrong.
    %discount the opinions of people who were wrong.
    % make a new list of outcomes using the discounted weightings
    % punish/reward people based off how closely they match this new list.
doit2(Matrix, Weights, F) ->
    Outcomes = outcomes(Matrix, Weights),%on order of oracle participants * decisions. call it O(X^2)
    Wrong = wrong_m(Matrix, Outcomes),% O(X^2)
    NewWeights = normalize(new_weights(F, Weights, Wrong)),% O(X)
    %io:fwrite("newweights "),
    %io:fwrite(packer:pack(NewWeights)),
    %io:fwrite("\n"),
    {Outcomes, NewWeights}.
doit(Matrix, Weights) ->
    %each row of Matrix is a single participant's report.
    {Outcomes, NewWeights} = doit2(Matrix, Weights, fractions:new(3, 5)),
    {_, FinalWeights} = doit2(Matrix, NewWeights, fractions:new(5, 7)),
    %Outcomes2 = outcomes(Matrix, NewWeights),
    %Wrong2 = wrong_m(Matrix, Outcomes2),
    %FinalWeights = normalize(new_weights(fractions:new(5, 7), Wrong2, Weights)),

    %FinalWeights is better than NewWeights because it can punish attackers who were successful.
    %Outcomes is better than _ because it is easier for attackers to double-cross each other. With _, it is more like either all the attackers succeed, or they all fail.
    {FinalWeights, Outcomes}.
normalize(L) -> normalize(L, sum(L)).
normalize([], _) -> [];
normalize([H|T], A) -> 
    [fractions:divide(H, A)|
     normalize(T, A)].
sum([]) -> fractions:new(0,1);
sum([H|T]) -> fractions:add(H, sum(T)).
new_weights(_, [], []) -> [];
new_weights(F, [H|T], [A|B]) -> 
    C = fractions:exponent(F, A),
    D = fractions:multiply(C, H),
    [D|new_weights(F, T, B)].
outcomes([[]|_], _) -> [];
outcomes(M, W) -> 
    H = hd_map(M),
    T = tl_map(M),
    [outcomes2(H, W)|
     outcomes(T, W)].
hd_map([]) -> [];
hd_map([H|T]) -> [hd(H)|hd_map(T)].
tl_map([]) -> [];
tl_map([H|T]) -> [tl(H)|tl_map(T)].
outcomes2(H, W) -> outcomes2(H, W, {f, 0, 1}, {f, 0, 1}, {f, 0, 1}, {f, 0, 1}).
outcomes2([], _, A, B, C, D)-> 
    E = fractions:add(A, B),
    F = fractions:add(C, E),
    Bool = fractions:less_than(F, D),
    if
	Bool -> 2;
	true -> outcomes3(A, B, C)
    end;
outcomes2([H|T], [Wh|Wt], A, B, C, D) ->
    case H of
	1 -> outcomes2(T, Wt, fractions:add(A, Wh), B, C, D);
	-1 -> outcomes2(T, Wt, A, fractions:add(B, Wh), C, D);
	0 -> outcomes2(T, Wt, A, B, fractions:add(C, Wh), D);
	2 -> outcomes2(T, Wt, A, B, C, fractions:add(D, Wh))
    end.
outcomes3(Yes, No, Middle) ->
    YN = fractions:add(Yes, No),
    Bool = fractions:less_than(YN, Middle),
    if
	Bool -> 0;
	true -> outcomes3(Yes, No)
    end.
outcomes3(Yes, No) ->
    Bool = fractions:less_than(Yes, No),
    if
	Bool -> -1;
	true -> 1
    end.
wrong_m([], _) -> [];
wrong_m([H|T], OC) -> [wrong(H, OC)|wrong_m(T, OC)].
wrong([], []) -> 0;
wrong([H|T], [H|B]) -> wrong(T, B);
wrong([1|T], [0|B]) -> 1 + wrong(T, B);
wrong([0|T], [1|B]) -> 1 + wrong(T, B);
wrong([0|T], [-1|B]) -> 1 + wrong(T, B);
wrong([-1|T], [0|B]) -> 1 + wrong(T, B);
wrong([_|T], [_|B]) -> 2 + wrong(T, B).
    
test() ->
    M = [[ 1, 1, 1, 1, 1, 2],
	 [ 1, 1, 1, 1, 1, 2],
	 [-1,-1, 1,-1, 1, 2],
	 [-1,-1,-1, 1, 1, 2],
	 [-1, 1,-1, 1, 1, 2]],
    W = [{f, 1, 5},{f, 1, 5},{f, 1, 5},{f, 1, 5},{f, 1, 5}],
    {A, B} = doit(M, W),
    [B, A].
%OC = outcomes(M, W),
%wrong_m(M, OC).
