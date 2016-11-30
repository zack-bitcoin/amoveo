-module(language).
-export([run/2, test/0, remove_till/2, assemble/1, hashlock/1, extract_sh/1, valid_secret/2, run_script/2, cost/1]).
power(A, B) when B == 1 -> A;
power(A, B) when (B rem 2) == 0 -> power(A*A, B div 2);
power(A, B) -> A*power(A, B-1).
-define(max_integer, power(2, 256)).
int_arith(2, X, Y) -> X + Y;
int_arith(3, X, Y) -> X - Y;
int_arith(4, X, Y) -> X * Y;
int_arith(5, X, Y) -> X div Y;
int_arith(6, X, Y) -> X > Y;
int_arith(7, X, Y) -> X < Y;
int_arith(8, X, Y) -> X == Y;
int_arith(9, X, Y) -> round(math:pow(X, Y)).
frac_arith(2, X, Y) -> fractions:add(X, Y);
frac_arith(3, X, Y) -> fractions:subtract(X, Y);
frac_arith(4, X, Y) -> fractions:multiply(X, Y);
frac_arith(5, X, Y) -> fractions:divide(X, Y);
frac_arith(6, X, Y) -> fractions:less_than(Y, X);
frac_arith(7, X, Y) -> fractions:less_than(X, Y);
frac_arith(8, X, Y) -> fractions:equal(X, Y);
frac_arith(9, X, Y) -> fractions:exponent(X, Y).
remove_till(X, T) -> remove_till(X, [], T, 0).
remove_till(X, H, [X|T], 0) -> {flip(H), T};
remove_till(_, _, _, N) when N < 0 -> N = 0;%N is how many if-statements deep we are.
remove_till(X, H, [17|T], N) -> remove_till(X, [17|H], T, N+1);
remove_till(X, H, [19|T], N) -> remove_till(X, [19|H], T, N-1);
remove_till(X, H, [A|T], N) -> remove_till(X, [A|H], T, N);
remove_till(_, _, X, _) -> 
    io:fwrite("error, you forgot to include 'else' or 'then' somewhere."),
    X = [0].
match([], [], _) -> true;
match([L|F], [{integer, -9}|Code], L) -> 
    % -9 is the code for recursively using the same list.
    match(F, Code, L);
match([{integer, _}|F], [{integer, -10}|Code], L) -> 
    % -10 is the code for integers.
    match(F, Code, L);
match([{f, A, B}|F], [{integer, -13}|Code], L) -> 
    % -13 is the code for fractions.
    true = is_integer(A),
    true = is_integer(B),
    false = (B == 0),
    match(F, Code, L);
match([M|F], [{integer, -12}|Code], L) -> 
    % -12 is the code for anything other than call.
    false = M == 38, %can't use call
    match(F, Code, L);
match([_|F], [{integer, -11}|Code], L) -> 
    % -11 is the code for anything.
    match(F, Code, L);
match([C|Function], [C|Code], L) -> 
    match(Function, Code, L).
match_check([{integer, Fu}|_], [{integer, MatchCode}|_], 0, MatchCode) -> Fu;
match_check([Fu|_], [{integer, MatchCode}|_], 0, MatchCode) -> Fu;
match_check([_|Function], [MatchCode|Format], I, MatchCode) ->
    match_check(Function, Format, I-1, MatchCode);
match_check([_|Function], [_|Format], I, MatchCode) ->
    match_check(Function, Format, I, MatchCode).

flip(X) -> flip(X, []).
flip([], O) -> O;
flip([H|T], O) -> flip(T, [H|O]).
run(Code, Gas) -> run(Code, dict:new(), dict:new(), [], [], Gas).
run(_, _, _, _, _, Gas) when Gas < 0 -> 
    io:fwrite("out of gas"),
    Gas = 0;
run([], _, _, _, Stack, _) -> Stack;
run([10|Code], Functions, Variables, Alt, [X|Stack], Gas) -> %drop
    run(Code, Functions, Variables, Alt, Stack, Gas-(list_length(X) * cost(10)));
run([56|Code], Functions, Variables, Alt, Stack, Gas) -> %print
    L = list_length(Stack),
    run(Code, Functions, Variables, Alt, Stack, Gas-L-cost(56));
run([11|Code], Functions, Variables, Alt, [X|Stack], Gas) -> %dup
    NewGas = Gas-(list_length(X) * cost(11)),
    %even though it is a new reference to the same data, we still need to charge a fee as if it was duplicated data. Otherwise they can keep deleting the list and get paid negative gas to delete.
    %NewGas = Gas-(cost(11)),
    run(Code, Functions, Variables, Alt, [X|[X|Stack]], NewGas);
run([14|Code], Functions, Variables, Alt, [X|[Y|Stack]], Gas) -> %2dup
    NewGas = Gas - (cost(14) *(list_length(X) + list_length(Y))),
    run(Code, Functions, Variables, Alt, [X|[Y|[X|[Y|Stack]]]], NewGas);
run([17|Code], Functions, Variables, Alt, [true|Stack], Gas) -> %if (case)
    run(Code, Functions, Variables, Alt, Stack, Gas-cost(17));
run([17|Code], Functions, Variables, Alt, [false|Stack], Gas) -> %if (case)
    {Skip, T} = remove_till(18, Code),
    run(T, Functions, Variables, Alt, Stack, Gas-(cost(17)*length(Skip)));
run([18|Code], Functions, Variables, Alt, Stack, Gas) -> %else
    {Skip, T} = remove_till(19, Code),
    run(T, Functions, Variables, Alt, Stack, Gas-(cost(18)*length(Skip)));
run([35|Code], Functions, Variables, Alt, [X|[Y|Stack]], Gas) ->%equals
    %10 is drop.
    NewGas = Gas - cost(35) - (cost(10) * (list_length(X)+list_length(Y))),
    run(Code, Functions, Variables, Alt, [(X == Y)|Stack],NewGas);
run([36|Code], Functions, Variables, Alt, Stack, Gas) ->%define
    {H, T} = remove_till(37, Code),
    B = hash:doit(H),
    run(T, dict:store(B, H, Functions), Variables, Alt, Stack, Gas-cost(36)-length(H));
run([38|Code], Functions, Variables, Alt, [B|Stack], Gas) ->%call
    case dict:find(B, Functions) of
	error -> 
	    io:fwrite("is not a function: "),
	    io:fwrite(packer:pack(B)),
	    io:fwrite("\n"),
	    io:fwrite("undefined function");
	{ok, F} ->
	    G = replace(41, B, F),%recursion
	    run(G++Code, Functions, Variables, Alt, Stack, Gas-cost(37))
    end;
run([55|Code], Functions, Variables, Alt, [Function|[Format|[I|[MatchCode|Stack]]]], Gas) ->%check
    Fu = dict:fetch(Function, Functions),
    Fo = dict:fetch(Format, Functions),
    X = match_check(Fu, Fo, I, MatchCode),
    L = length(Fo),
    run(Code, Functions, Variables, Alt, [X|Stack], Gas - (L*cost(55)));

run([42|Code], Functions, Variables, Alt, [L|[B|Stack]], Gas) ->%match
    {A, N} = case dict:find(B, Functions) of
	    error -> 
		     %io:fwrite("error in match, undefined function "),
		{false, 10};
	    {ok, F} ->
		case dict:find(L, Functions) of
		    error ->
			%io:fwrite("error in match, undefined function2 ");
			{false, 10};
		    {ok, G} ->
			%G is the format, F is the function, L is the name of the format.
			{match(F, G, L), length(F)}
		end
	end,
    run(Code, Functions, Variables, Alt, [A|Stack], Gas-(cost(42)*N));
run([39|Code], Functions, Variables, Alt, [N|Stack], Gas) ->%moves the top of the stack to the top of the alt stack.
    run(Code, Functions, Variables, [N|Alt], Stack, Gas-cost(39));
run([40|Code], Functions, Vars, [N|Alt], Stack, Gas) ->%moves the top of the alt stack to the top of the stack.
    run(Code, Functions, Vars, Alt, [N|Stack], Gas - cost(40));
run([28|_], _, _, _, _, _) -> %die. Neither person gets money.
    [delete];
run([44|Code], Functions, Vars, Alt, [Name|[Value|Stack]], Gas) when is_binary(Value)-> %store
    NVars = dict:store(Name, Value, Vars),
    run(Code, Functions, NVars, Alt, Stack, Gas-(cost(44)+size(Value)));
run([44|Code], Functions, Vars, Alt, [Name|[Value|Stack]], Gas) when is_list(Value)-> %store
    NVars = dict:store(Name, Value, Vars),
    run(Code, Functions, NVars, Alt, Stack, Gas-(cost(44)+length(Value)));
run([44|Code], Functions, Vars, Alt, [Name|[Value|Stack]], Gas) -> %store
    NVars = dict:store(Name, Value, Vars),
    run(Code, Functions, NVars, Alt, Stack, Gas-cost(44));
run([45|Code], Functions, Vars, Alt, [Name|Stack], Gas) -> %fetch
    X = case dict:find(Name, Vars) of
	error -> 0;
	{ok, Val} -> Val
    end,
    C = if
	    is_binary(X) -> size(X);
	    is_list(X) -> length(X);
	    true -> 1
	end,
    run(Code, Functions, Vars, Alt, [X|Stack], Gas-(cost(45)*C));
run([47|Code], Functions, Vars, Alt, Stack, Gas) -> %gas
    run(Code, Functions, Vars, Alt, [Gas|Stack], Gas);
run([Word|Code], Functions, Vars, Alt, Stack, Gas) ->
    run(Code, Functions, Vars, Alt, run_helper(Word, Stack), Gas-cost(Word)).
run_helper(0, [H|Stack]) -> 
    [hash:doit(H)|Stack];%hash
run_helper(1, [Pub|[Data|[Sig|Stack]]]) ->%verify_sig
    %true = is_binary(Data),
    [testnet_sign:verify_sig(Data, Sig, Pub)|Stack];

run_helper(Word, [Y|[X|Stack]]) when (is_integer(Word) and ((Word > 1) and (Word < 9))) ->
    Z = if
	    (is_integer(X) and is_integer(Y)) ->
		I = int_arith(Word, X, Y),
		if
		    is_integer(I) ->
			(I rem ?max_integer);
		    true -> I
		end;
	    is_integer(X) ->
		frac_arith(Word, fractions:new(X, 1), Y);
	    is_integer(Y) ->
		frac_arith(Word, X, fractions:new(Y, 1));
	    true ->
		frac_arith(Word, X, Y)
	end,
    [Z|Stack];
run_helper(9, [X|[Y|Stack]]) -> %[Y|[X|Stack]];%pow
    true = is_integer(Y),
    A = if
	is_integer(X) -> round(math:pow(X, Y));
	true -> fractions:exponent(X, Y)
	end,
    [A|Stack];
%run_helper(10, [_|Stack]) -> Stack;
run_helper(12, [X|[Y|[Z|Stack]]]) -> [Y|[Z|[X|Stack]]];%rot
run_helper(13, [X|[Y|[Z|Stack]]]) -> [Z|[X|[Y|Stack]]];%-rot (tor)
run_helper(15, [N|[X|Stack]]) -> %tuckn 
    H = lists:sublist(Stack, 1, N),
    T = lists:sublist(Stack, N+1, 10000000000000000),
    H ++ [X] ++ T;
run_helper(16, [N|Stack]) -> %pickn 
    H = lists:sublist(Stack, 1, N - 1),
    T = lists:sublist(Stack, N, 10000000000000000),
    [hd(T)] ++ H ++ tl(T);
run_helper(19, Stack) -> Stack;%then 
run_helper(20, [X|[Y|Stack]]) -> [(X and Y)|Stack];%and (both)
run_helper(21, [X|[Y|Stack]]) -> [(X or Y)|Stack];%or (either)
run_helper(22, [X|[Y|Stack]]) -> [(X xor Y)|Stack];%xor (only_one)
run_helper(23, [X|Stack]) -> [(not X)|Stack];%not (invert)
run_helper(24, [X|[Y|Stack]]) -> [<<X/binary, Y/binary>>|Stack];%append binaries
run_helper(25, [X|[Binary|Stack]]) -> %strip right
    T = (size(Binary)*8 - X*8),
    <<A:T, _/binary>> = Binary,
    [<<A:T>>|Stack];
run_helper(26, [X|[Binary|Stack]]) -> %strip left
    Y = X*8,
    <<_:Y, A/binary>> = Binary,
    [A|Stack];
run_helper(27, Stack) -> flip(Stack);
run_helper(29, [F|Stack]) -> [fractions:to_int(F)|Stack]; %fraction2int
run_helper(30, [A|[B|Stack]]) -> [fractions:new(B, A)|Stack];%int2fraction
run_helper(31, Stack) -> [block_tree:total_coins()|Stack];%total_caoins
run_helper(32, Stack) -> [block_tree:height()|Stack];%height
run_helper(33, Stack) -> [length(Stack)|Stack];%stack size
run_helper(34, Stack) -> [false|Stack];%this returns true if called from a channel_slash tx.
run_helper(35, [X |[Y |Stack]]) -> [(X == Y)|Stack];%check if 2 non-numerical values are equal. like binary.
%run_helper(37, [Stack]) -> [Stack];%check if 2 non-numerical values are equal. like binary.
run_helper(43, [X |[Y |Stack]]) -> [(Y rem X)|Stack];%check remainder after division of 2 values.
%run_helper(36, Stack) -> Stack;
run_helper(46, Stack) -> 
    io:fwrite(packer:pack(Stack)),
    io:fwrite("\n"),
    Stack;
%47 is gas.
run_helper(48, [A|[B|Stack]]) -> %cons
    true = is_list(A),
    [[B|A]|Stack];
run_helper(49, [A|Stack]) -> [hd(A)|[tl(A)|Stack]];%car/cdr
run_helper(50, [ID|Stack]) -> 
    Pub = accounts:pub(block_tree:account(ID)),
    Y = case Pub of
	[] -> false;
	X -> X
    end,
    [Y|Stack];%id to pub
run_helper(51, Stack) -> [[]|Stack];% '()
run_helper(52, [L|Stack]) -> [lists:reverse(L)|Stack];
run_helper(53, [X|[Y|Stack]]) -> [Y|[X|Stack]];%swap
run_helper(54, [X|[Y|Stack]]) -> [(X++Y)|Stack];%append lists
run_helper(56, [ID|Stack]) -> 
    B = accounts:balance(block_tree:account(ID)),
    [{integer, B}|Stack];%id to balance
run_helper(57, [X|Stack]) -> 
    Y = X == [],
    [Y|[X|Stack]];% dupnil==
run_helper(58, [X|Stack]) -> 
    Y = testnet_sign:pubkey2address(X),
    [Y|Stack];% pub2addr

run_helper({f, T, B}, Stack) -> 
    [{f, T, B}|Stack];%load fraction into stack.
run_helper(B, Stack) when is_binary(B)-> 
    true = size(B) < 300,
    [B|Stack];%load binary into stack.
run_helper({integer, I}, Stack) -> 
    %we should probably have a maximum size.
    [(I rem ?max_integer)|Stack];
run_helper(true, Stack) -> [true|Stack];
run_helper(false, Stack) -> [false|Stack].
assemble(Code) -> assemble(Code, []).
assemble([], Out) -> flip(Out);
assemble([Word|C], Out) ->
    X = if
	    is_atom(Word) -> atom2op(Word);
	    is_integer(Word) -> {integer, Word};
	    true -> Word
	end,
    assemble(C, [X|Out]).
atom2op(hash) -> 0;%( X -- <<Bytes:256>> )
atom2op(verify_sig) -> 1;%( Sig Data Pub -- true/false )
atom2op(plus) -> 2;%( X Y -- Z )
atom2op(minus) -> 3;%( X Y -- Z )
atom2op(multiply) -> 4;%( X Y -- Z )
atom2op(divide) -> 5;%( X Y -- Z )
atom2op(gt) -> 6;%( X Y -- true/false )
atom2op(lt) -> 7;%( X Y -- true/false )
atom2op(eq_num) -> 8;%( X Y -- true/false )
atom2op(pow) -> 9;%( X Y -- true/false )
atom2op(swap) -> 53; %( A B -- B A )
atom2op(drop) -> 10;%( X -- )
atom2op(dup) -> 11;%( X -- X X )
atom2op(rot) -> 12;%( a b c -- c a b ) 
atom2op(tor) -> 13;%( a b c -- b c a )
atom2op(ddup) -> 14;%( a b -- a b a b )
atom2op(tuckn) -> 15;%( X N -- ) inserts X N-deeper into stack.
atom2op(pickn) -> 16;%( N -- X )
% true switch <<"executed">> else <<"ignored">> then 
% false switch <<"ignored">> else <<"executed">> then 
atom2op(switch) -> 17;% conditional statement
atom2op(else) -> 18;% part of an switch conditional statement
atom2op(then) -> 19;%part of switch conditional statement.
atom2op(both) -> 20;%( true/false true/false -- true/false )
atom2op(either) -> 21;%( true/false true/false -- true/false )
atom2op(only_one) -> 22;%( true/false true/false -- true/false )
atom2op(invert) -> 23;%( true/false -- false/true )
atom2op(append) -> 24;%( <<Binary1/binary>> <<Binary2/binary>> -- <<Binary1/binary, Binary2/binary>> )
atom2op(stripr) -> 25;%( <<Binary/binary>> -- <<ShorterBinary/binary>> )
atom2op(stripl) -> 26;%( <<Binary/binary>> -- <<ShorterBinary/binary>> )
atom2op(flip) -> 27;%entire stack is flipped.
atom2op(crash) -> 28;%code stops execution here. Neither person gets the money.
atom2op(f2i) -> 29; %( F -- I ) fraction to integer
atom2op(i2f) -> 30; %( I -- F ) integer to fraction
atom2op(total_coins) -> 31; %( -- TotalCoins )
atom2op(height) -> 32; %( -- Height )
atom2op(stack_size) -> 33; %( -- Size )
atom2op(slash) -> 34; %( -- true/false)
atom2op(eq) -> 35; %( X Y -- true/false )
atom2op(define) -> 36; % make a new word out of all the opcodes between here and the next 37.
atom2op(stop) -> 37; %crash
atom2op(call) -> 38; %Use the binary at the top of the stack to look in our hashtable of defined words. If it exists call the code, otherwise crash.
atom2op(to_r) -> 39; %( V -- )
atom2op(from_r) -> 40; %( -- V )
atom2op(recurse) -> 41; %crash. this word should only be used in the definition of a word.
atom2op(match) -> 42; % Use the binary to look up a defined word. Make sure the word matches the code that follows 'match', otherwise crash.
atom2op(remainder) -> 43; % (A B -- C) only works for integers.
atom2op(store) -> 44; % ( X Y -- )
atom2op(fetch) -> 45; % ( Y -- X )
atom2op(print) -> 46; % ( Y -- X )
atom2op(gas) -> 47; % ( Y -- X )
atom2op(cons) -> 48; % ( X Y -- [X|Y] )
atom2op(car) -> 49; % ( [X|Y] --  X )
atom2op(id2pub) -> 50; % ( [X|Y] --  Y )
atom2op(nil) -> 51; % ( -- [] )
atom2op(flip_list) -> 52; % ( F -- G )
atom2op(append_list) -> 54;
atom2op(check_match) -> 55;
atom2op(true) -> true; %( -- true )
atom2op(false) -> false. %( -- false )
cost(0) -> 20;
cost(1) -> 20;
cost(36) -> 40;
cost(38) -> 20;
cost(44) -> 20;
cost(56) -> 20;%looking the account up takes some time.
cost(50) -> 1306;%15*size of pubkey - 15 for deleting integer + 1
cost(45) -> 16;%fetch
cost(10) -> -15; %drop
cost(11) -> 16; %dup
cost(14) -> 16; %2dup
%cost(16) -> 16; %pickn 
cost(31) -> 16; %total_coins
cost(32) -> 16; %height
cost(33) -> 16; %stack_size
cost(34) -> 16; %stack_size
cost(47) -> 16; %gas
cost(51) -> 16; %nil, list root.
cost(43) -> -14; %remainder 
cost(35) -> -14; %eq
cost(22) -> -14; %xor
cost(21) -> -14; %or
cost(20) -> -14; %and
cost(9) -> -14; %pow
cost(8) -> -14; %eq num
cost(7) -> -14; %less than
cost(6) -> -14; %greater than
cost(5) -> -14; %divide
cost(4) -> -14; %multiply
cost(3) -> -14; %subtract
cost(2) -> -14; %add
cost(X) when is_integer(X) -> 1;
cost({f, _, _}) -> 16;
cost(B) when is_binary(B) -> size(B) + 15;
cost({integer, _}) -> 16;
cost(true) -> 16;
cost(false) -> 16.
replace(A, B, C) -> replace(A, B, C, []).
replace(_, _, [], Out) -> flip(Out);
replace(A, B, [A|T], Out) -> 
    replace(A, B, T, [B|Out]);
replace(A, B, [H|T], Out) -> 
    replace(A, B, T, [H|Out]).
list_length(X) -> list_length(X, 0).
list_length(B, N) when is_binary(B)-> N+size(B);
list_length([], N) -> N;
list_length([H|T], N) -> 
    M = list_length(H),
    list_length(T, M+N);
list_length(_, N) -> N+1.
hashlock(SecretHash) ->
    assemble([hash, SecretHash, eq, switch, {f, 0, 1}, {f, 1, 1}, 2, else, {f, 0, 1}, {f, 0, 1}, 1, then]).
valid_secret(Secret, Script) -> 
   hd(tl(run([Secret] ++ Script, constants:gas_limit()))).
extract_sh(Code) -> hd(tl(Code)).
run_script(Code, Gas) ->
    case run(Code, Gas) of
	[delete] ->
	    {0,{f,0,1},{f,1,1}};
	Out ->
   %{nonce, Amount to transfer, Amount to delete}
   % the highest nonced scriptsig is the only valid scriptsig.
	    {hd(Out), hd(tl(Out)), hd(tl(tl(Out)))}
    end.
    
    
test() ->    
    true = run(assemble([10, 2, plus]), 100) == [12],
    true = run(assemble([{f, 10, 11}, 5, plus]), 100) == [{f, 65, 11}],
    true = run(assemble([false, switch, 100, else, 27, then, 3]), 100) == [3, 27],%if
    true = run(assemble([true, switch, 100, else, 27, then, 2]), 50) == [2, 100],%if
    true = run(assemble([true, switch, 100, false, switch, else, then, else, 27, then, 2]), 100) == [2, 100],%if %err
    true = run(assemble([true, switch, 100, else, 27, true, switch, else, then, then, 2]), 100) == [2, 100],%if
    %{Addr, Pub, Priv} = testnet_sign:hard_new_key(),
    {_Addr, Pub, Priv} = {<<"UrNC5nqd7uERs6m">>, <<"BIDUw/Dagrmh3R4akgRfCwH1/EIoCIAKrfMwPAKSoCn0h2iyjUb/lq8ZrngfARRlAdOsNSVp1gW0DQ8Xp+fz910=">>, <<"P1h6YBH65iQy/4lzvNhhPecYJrvoMoqeX+IR912bIjM=">>},
    Data = <<"example">>,
    Sig = testnet_sign:sign(Data, Priv),
    true = testnet_sign:verify_sig(Data, Sig, Pub),
    true = run(assemble([Sig] ++ [Data, Pub, verify_sig]), 500) == [true],%3rd party signature
    B = hash:doit(1),
    true = run(assemble([1] ++ [hash, B, eq]), 100) == [true],%normal hashlock
    true = run(assemble([{f, 1, 2}, to_r, from_r]), 100) == [{f, 1, 2}],
    H = [{f, 1, 2}, {integer, 500}, plus],
    HASH = hash:doit(assemble(H)),
    Code = [define]++H++[stop, HASH, call],
    true = [{f,1001,2}] == run(assemble(Code), 200),
    H20 = [dup, rot, plus],
    Hash20 = hash:doit(assemble(H20)),
    H2 = [tor, dup, {integer, 0}, gt, switch, {integer, 1}, minus, rot, Hash20, call, recurse, call, else, then],
    Hash2 = hash:doit(assemble(H2)),
    Code1 = [define] ++ H20 ++ [stop, 
	     define] ++ H2 ++ [stop, 
	     {integer, 5}, {integer, 0}, {integer, 1}, Hash2, call],
    true = [0, 8, 5] == run(assemble(Code1), 1000),
    Two = hash:doit(2),
    HTwo = hash:doit(Two),
    [2,{f,1,1},{f,0,1}] = run([Two] ++ hashlock(HTwo), 1000),
    [1,{f,0,1},{f,0,1}] = run([hash:doit(3)] ++ hashlock(HTwo), 1000),
    %H30 = [dup, dup, {integer, 5}, plus, plus, plus],
    %Hash3 = hash:doit(assemble(H30)),
    %Code3 = [define] ++ H30 ++ [stop,
    %Hash3, {integer, 6}, match, dup, dup, {integer, 5}, plus, {integer, -12}, plus], 
    %[true] = language:run(assemble(Code3), 1000),
    success.
%assemble([H]) ++ hashlock(HASH),
 %   success.
