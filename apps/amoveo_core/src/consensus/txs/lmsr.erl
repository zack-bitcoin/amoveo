-module(lmsr).
-export([cf2f/2, log_table/1, to_float/1, 
         exp/1, ln/1, make_rat/2, e/0, pow/2,
         accuracy/1, speed/0, ln2/0, 
         inverse/1, add/2, price/3,
         veo_in_market/3, change_in_market/5,
         q2/3, q2b/3, ac/2, max_buy/4,
         test_max_buy/1,
         test_price_q2/0]).

%==========TLDR, limits of this software=======
% don't match trades where the odds are steeper than 1:1000. only provide liquidity inside the bounds.
%given a market with Q1 and Q2 shares, and a beta value of B you can look up the current amount of veo in that market like this: 
% `veo_in_market(B, Q1, Q2).`
%================================================

%%%%%%%%%_____Long Docs_____%%%%%%%%%%%

%logrithm algorithm based on this page: https://love2d.org/forums/viewtopic.php?p=231486

%about the lmsr.
%how much money is in an lmsr market that sold q1 of shares of type 1, and q2 of shares of type 2. B is a liquidity constant based on how much was paid to make the market.
%`C = B * ln(e^(q1/B) + e^(q2/B))`

%instantaneous price 
%`P = e^(q1/B) / (e^(q1/B) + e^(q2/B))`




-record(rat, {t, b}).
-define(limit, %4294967296). %2^32
%        16777216).
        1048576). %2^20
%        65536).

%highest value we can represent is approximately 

make_rat(T, B) -> simplify(#rat{t = T, b = B}).
to_float(Z) -> Z#rat.t / Z#rat.b.
to_int(Z) -> Z#rat.t div Z#rat.b.
zero(X) -> X#rat.t == 0.
is_positive(#rat{t = T, b = B}) -> (T*B) > 0.
gcf(X, Y) when (abs(Y) > abs(X)) -> 
    gcf(Y, X);
gcf(X, 0) -> X;
gcf(X, Y) -> gcf(Y, X rem Y).
%simplify(R) -> R;%don't simplify
simplify(R = #rat{t = T, b = B}) ->
    %todo. we probably need to limit the number of digits somehow.
    G = gcf(T, B),
    R2 = case G of
             0 -> R;
             _ ->
                 #rat{t = (T div G), b = (B div G)}
         end,
    #rat{t = T2, b = B2} = R2,
    if
        (T2 > ?limit) or (B2 > ?limit) ->
            L = max(T2 div ?limit, B2 div ?limit),
            #rat{t = T2 div L, b = B2 div L};
        true -> R2
    end.
mul(R, X) when is_integer(X) ->
    mul(R, #rat{t = X, b = 1});
mul(X, R) when is_integer(X) ->
    mul(#rat{t = X, b = 1}, R);
mul(#rat{t = T1, b = B1}, 
    #rat{t = T2, b = B2}) ->
    simplify(#rat{t = T1*T2, b = B1 * B2}).
mul([A]) -> A;
mul([A,B|T]) -> 
    mul([mul(A, B)|T]).
square(X) -> mul(X, X).
mul_to_int(I, {rat, T, B}) ->
    I * T div B.
sub(A, B) -> add(A, negative(B)).
add(A, B) when is_integer(B) ->
    add(A, #rat{t = B, b = 1});
add(A, B) when is_integer(A) ->
    add(B, #rat{t = A, b = 1});
add(#rat{t = T1, b = B1}, 
    #rat{t = T2, b = B2}) -> 
    simplify(#rat{t = (T1*B2) + (T2 * B1),
                  b = (B1 * B2)}).
add([A]) -> A;
add([A, B|T]) -> 
    add([add(A, B)|T]).
divide(A, B) -> mul(A, inverse(B)).
inverse(X) when is_integer(X) ->
    #rat{t = 1, b = X};
inverse(#rat{t = T, b = B}) ->
    #rat{t = B, b = T}.
negative(N = #rat{t = T}) ->
    N#rat{t = -T};
negative(X) when is_integer(X) -> -X.
less_than(#rat{t = T1, b = B1},
          #rat{t = T2, b = B2}) ->
    (T1 * B2) < (T2 * B1).
bigger(X, Y) ->
    B = less_than(X, Y),
    if
        B -> Y;
        true -> X
    end.
equal(#rat{t = T1, b = B1},
      #rat{t = T2, b = B2}) ->
    (T1 * B2) == (T2 * B1).
%exponentiation by squaring.
pow(R = #rat{}, 1) -> R;
pow(R = #rat{}, 0) -> #rat{t = 1, b = 1};
pow(R = #rat{}, N) when (N < 0) ->
    pow(inverse(R), -N);
pow(R = #rat{},
    I) when (is_integer(I) and ((I rem 2) == 0)) ->
    pow(square(R), I div 2);
pow(R = #rat{t = T, b = B},
    I) when is_integer(I) ->
    mul(R, pow(R, I-1)).


e() -> 
    #rat{t = 1021321,b = 375723}.
   %#rat{t = 5279458664,b =1942204303}.
%    cf2f(2, [1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, 1, 10, 1, 1, 12, 1, 1, 14, 1, 1, 16, 1, 1, 18, 1, 1]).
    
zero() ->
    #rat{t = 0, b = 1}.
one() ->
    #rat{t = 1, b = 1}.
two() ->
    #rat{t = 2, b = 1}.
half() ->
    #rat{t = 1, b = 2}.
ln2() ->
    #rat{t = 666176,b = 961089}.
%    #rat{t = 3342617437,b = 4822377600}.
%    cf2f(0, [1, 2, 3, 1, 6, 3, 1, 1, 2, 1, 1, 1, 1, 3, 10, 1, 1, 1, 2, 1, 1, 1, 1, 3, 2, 3, 1, 13, 7, 4]).
exp(#rat{t = T, b = B}) ->
    %e^(T / B) = 
    %e^(T div B) * e^((T rem B)/B)
    Xfract = #rat{t = T rem B, b = B},
    Rint = pow(e(), T div B),%e^(T div B)
    Xfract2 = mul(Xfract, Xfract),
    Xfract3 = mul(Xfract, Xfract2),
    Xfract4 = mul(Xfract2, Xfract2),
    Xfract5 = mul(Xfract2, Xfract3),
    Xfract6 = mul(Xfract3, Xfract3),
    Xfract7 = mul(Xfract4, Xfract3),
%    Xfract8 = mul(Xfract4, Xfract4),
    % for n=0,4 sum xfract^n / n!
    %maclaurin series for e^(Xfract)
    %https://en.wikipedia.org/wiki/Taylor_series
    Rfract = add(
               [one(),
               Xfract,
               mul(Xfract2, #rat{t = 1, b = 2}),
               mul(Xfract3, #rat{t = 1, b = 6}),
               mul(Xfract4, #rat{t = 1, b = 24}),
               mul(Xfract5, #rat{t = 1, b = 120}),
               mul(Xfract6, #rat{t = 1, b = 720}),
                mul(Xfract7, #rat{t = 1, b = 5040})%,
%                mul(Xfract8, #rat{t = 1, b = 40320})
                
               ]),
    mul(Rint, Rfract).
%-define(powerTable, {%i + 2^(-i)
%          #rat{3, 2}, #rat{5, 4}, #rat{9, 8}), ...
power_table(I) when (I > 0) ->
    %1 + (1/2)^i
    B = round(math:pow(2, I)),
    #rat{t = B+1, b = B}.

%continued fraction -> rational
%used for generating the constant rationals on this page.
cf2f(F, L) -> 
    add(#rat{t = F, b = 1},
    cf2f2(L)).
cf2f2([]) -> #rat{t = 0, b = 1};
cf2f2([H|T]) -> 
    %1 / (H + cf2f2(T))
    inverse(add(#rat{t = H, b = 1}, cf2f2(T))).

%like frexp, but we already know R is <1/2
frexp_lt(R) ->
    LTH = less_than(R, half()),
    if
        LTH ->
            {M1, X1} = frexp_lt(mul(R, two())),
            {M1, X1 - 1};
        true ->
            {R, 0}
    end.

%like frexp, but we already know R is >1
frexp_gt(R) ->
    GTO = less_than(one(), R),
    if
        GTO ->
            {M1, X1} = frexp_gt(mul(R, half())),
            {M1, X1 + 1};
        true ->
            {R, 0}
    end.

frexp(#rat{t = 0}) ->
    io:fwrite("log(0) is undefined"),
    1=2;
frexp(R = #rat{}) ->
    %-> {M, X} s.t. 1 >= |M| >= 0.5 and M*(2^X) == R
    NP = not(is_positive(R)),
    if
        NP -> 
            {M1, X1} = frexp(negative(R)),
            {negative(M1), X1};
        true ->
            LTH = less_than(R, half()),
            if
                LTH ->
                    {M1, X1} = frexp_lt(mul(R, two())),
                    {M1, X1 - 1};
                true ->
                    GTO = less_than(one(), R),
                    if
                        GTO ->
                            {M1, X1} = frexp_gt(mul(R, half())),
                            {M1, X1 + 1};
                        true ->
                            {R, 0}
                    end
            end
    end.

%accurate to 3 decimal points for inputs less than 20 million.
% 6 decimal points for inputs less than 9 million.
% accurate to 3 decimals for input bigger than 1.01
ln({rat, _, 0}) ->
    1=2,
    log_infinity_fail;
ln(R = #rat{}) ->           
    {M, X} = frexp(R),
    X2 = X - 1,
    LN2_X2 = mul(ln2(), #rat{t = X2, b = 1}),
    E = equal(M, half()),
    if 
        E -> LN2_X2;
        true ->
            Arg = mul(M, two()),
            Sum = ln_loop(Arg, one(), zero(), 1),
            add(Sum, LN2_X2)
    end.
ln_loop(_Arg, _Prod, Sum, 20) -> Sum;
ln_loop(Arg, Prod, Sum, N) ->
    Prod2 = mul(Prod, power_table(N)),
    LT = less_than(Prod2, Arg),
    if
        LT ->
            ln_loop(Arg, Prod2, add(Sum, log_table(N)), N+1);
        true ->
            ln_loop(Arg, Prod, Sum, N+1)
    end.

%log_table(i) = ln(1+(1/2)^i)
log_table(1) ->
    #rat{t = 239194,b = 589925};
%    #rat{t = 551104173,b = 1359190130};
%    cf2f(0, [2,2,6,1,11,2,1,2,2,1,4,3,1,1,7,2,1,1,4,1,2,1,2,1]);
log_table(2) ->
    #rat{t = 252475,b = 1131446};
%     #rat{t = 1497134811,b = 6709290061};
%    cf2f(0, [4,2,12,1,21,2,2,1,89,1,5,3,1,76,1,7,60,2,2,2,22,6]);
log_table(3) ->
    #rat{t = 138842,b = 1178797};
%    #rat{t = 283987567,b = 2411107556};
%    cf2f(0, [8, 2, 24, 1, 41, 2, 6, 9, 2, 83, 8, 5, 1, 2, 20, 3, 1, 2, 4, 3, 7, 2, 1, 4, 1, 4, 1, 1, 1]);
log_table(4) ->
    {rat,33178,547271};
%    #rat{t = 264413820,b = 4361492281};
    %cf2f(0, [16, 2, 48, 1, 81, 2, 12, 3, 36, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 6, 1, 2, 1, 12]);
log_table(5) ->
    {rat,496,16119};
    %#rat{t = 133446201,b = 4336659364};
    %cf2f(0, [32, 2, 96, 1, 161, 2, 24, 1, 3, 2, 7, 1, 2, 7, 1, 22, 105, 1, 1, 3, 7, 4, 7, 1, 3, 2, 1]);
log_table(6) ->
    {rat,16463,1061875};
      %#rat{t = 67586257,b = 4359226273};
%    cf2f(0, [64, 2, 192, 1, 321, 2, 49, 1, 1, 1, 144, 2, 5, 5, 32, 1, 7, 2, 2, 2, 3, 2, 5, 6, 2, 1]);
log_table(7) ->
    {rat,1753,225269};
%    #rat{t = 11201457,b = 1439379995};
     %cf2f(0, [128, 2, 384, 1, 641, 2, 99, 2, 3, 1, 31, 2, 1, 7, 2, 2, 8, 11, 16, 1, 1, 1, 1, 2, 12]);
log_table(8) ->
    {rat,2051,526100};
%    #rat{t = 16794959,b = 4307901582};
%    cf2f(0, [256, 2, 768, 1, 1281, 2, 198, 1, 5193, 8, 3, 100, 33, 2, 1, 67, 3, 2, 1, 5, 2, 1, 1]);
log_table(9) ->
    {rat,2049,1050221};
    %#rat{t = 8491613,b = 4351950448};
%    cf2f(0, [512, 2, 1536, 1, 2561, 2, 398, 9, 128, 72, 6, 4, 41640, 2, 1, 2, 1, 5, 6, 3, 277]);
log_table(10) ->
    {rat,128,131195};
    %#rat{t = 4192361,b = 4295074331};
%    cf2f(0, [1024, 2, 3072, 1, 5121, 2, 796, 3, 2304, 1, 3, 1, 3, 1, 11, 1, 2, 1, 2, 2, 1, 2, 13]);
log_table(11) ->
    {rat,511,1048721};
    %#rat{t = 2097520,b = 4296771473};
%    cf2f(0, [2048, 2, 6144, 1, 10241, 2, 1592, 1, 3, 2, 511, 1, 2, 7, 1, 2, 24, 1, 2, 4, 2, 3, 2]);
log_table(12) ->
    {rat,128,524487};
    %#rat{t = 1048669,b = 4295876303};
    %cf2f(0, [4096, 2, 12288, 1, 20481, 2, 3185, 1, 1, 1, 9216, 2, 5, 2, 49, 1, 1, 1, 1, 3, 2, 2]);
log_table(13) ->
    {rat,8,65543};
    %#rat{t = 131086,b = 1073923321};
%    cf2f(0, [8192, 2, 24576, 1, 40961, 2, 6371, 2, 3, 1, 2047, 2, 1, 7, 2, 1, 99, 2, 4, 2, 266]);
log_table(14) ->
    {rat,32,524305};
    %#rat{t = 262150,b = 4295196831};
%    cf2f(0, [16384, 2, 49152, 1, 81921, 2, 12742, 1, 331785, 8, 200, 7, 1, 2, 3, 1, 2129, 4]);
log_table(15) ->
    {rat,16,524305};
%    #rat{t = 2147542565,b = 70371748535741}.
%    cf2f(0, [32768, 2, 98304, 1, 163841, 2, 25486, 9, 8192, 72, 400, 2, 24, 2, 4259, 2, 2]);
log_table(16) ->
    {rat,16,1048605};
    %#rat{t = 32768,b = 2147526069};
%    cf2f(0, [65536, 2, 196608, 1, 327681, 2, 50972, 3, 147456, 1, 3, 1, 3, 1, 800, 4, 1, 3]);
log_table(17) ->
    {rat,4,524291};
    %#rat{t = 4096,b = 536874717};
%    cf2f(0, [131072, 2, 393216, 1, 655361, 2, 101944, 1, 3, 2, 32767, 1, 2, 7, 1, 2, 1601]);
log_table(18) ->
    {rat,3,1048577};
    %#rat{t = 16383,b = 4294970971};
%    cf2f(0, [262144, 2, 786432, 1, 1310721, 2, 203889, 1, 1, 1, 589824, 2, 5, 2, 3203, 1]);
log_table(19) ->
    {rat,1,1048576};
    %#rat{t = 8191,b = 4294969751};
%    cf2f(0, [524288, 2, 1572864, 1, 2621441, 2, 407779, 2, 3, 1, 131071, 2, 1, 7, 2, 1, 64]);
log_table(20) ->
    {rat,1,1048576};
    %#rat{t = 2048,b = 2147485607};
%    cf2f(0, [1048576, 2, 3145728, 1, 5242881, 2, 815558, 1, 21233673, 8, 12815, 1, 4, 3]);
log_table(21) ->
    {rat,0,1}.
    %#rat{t = 2048,b = 4294969197}.
%    cf2f(0, [2097152, 2, 6291456, 1, 10485761, 2, 1631118, 9, 524288, 72, 25631, 1, 5]).

price(B, Q1, Q2) -> 
    %result is in shares/veo. a value from 0 to 1.
    %1/(1 + e^((Q2 - Q1)/B))
    inverse(add(exp(#rat{t = Q2 - Q1, b = B}), 1)).

q2({rat, 0, _}, _B, _Q1) ->
    impossible;
q2(P = {rat, _, _}, B, Q1) ->
%given a price, Q1, and B, solve for Q2.
%so like, if the current price is P, and we want to match a trade at some higher price Q, you could use this function to see how much liquidity the market maker is going to give you as you move the price from P to Q.
    %the pure logrithm version fails when Q1 is much bigger than B. 
    %pure log version: Q1 + B*ln((1/P) - 1) = Q2
    %so we use a slide-rule trick to move what we are looking for into the more accurate range of our measuring stick.
    %Q2b = q2b(P, B, {rat, 0, 1}),
    %to_int(Q2b) + Q1.

    true = is_positive(P),
    true = less_than(P, {rat, 1, 1}),
    %another issue is that if Q2 is bigger than ?limit, it isn't possible for this library to return that value. So lets scale big values as well.
    StartSize = max(B, Q1),
    %MaxSize = 1024,
    MaxSize = 1,
    %StartSize = 50000,
    %Scale = bigger({rat, 1, 1}, {rat, MaxSize, StartSize}),
%    Bb = B * MaxSize div StartSize,
%    Bb = mul(B, Scale),
    Bb = simplify({rat, B * MaxSize, StartSize}),

    %io:fwrite({P, B, Scale}),
    Q2bb = q2b(P, Bb, {rat, 0, 1}),
    #rat{t = Q2bbt, b = Q2bbb} = Q2bb,
    Q2b = (Q2bbt * StartSize) div (Q2bbb * MaxSize),
%    Q2b = to_int(Q2bb) * StartSize div MaxSize,
    Q2b + Q1.
    
%    to_int(Q2b) + Q1.
q2b(P, B, Q1) ->
    %todo maybe we should use floating point here for speed?

    %derivation:
    %P = 1/(1 + e^((Q2 - Q1)/B))
    %1/P = (1 + e^((Q2 - Q1)/B))
    %ln((1/P) - 1) = (Q2 - Q1)/B
    %Q1 + B*ln((1/P) - 1) = Q2

    %sanity check on bounds.

    add(Q1, mul(B, ln(sub(inverse(P), 1)))).
    
  
%if you have X veo, how many shares of Y can you buy? 
max_buy(B, Y0, N0, X) -> 
    %C1 = B*ln(e^(Y0/B) + e^(N0/B)),
    %C1 + X = B*ln(e^((Y0+?)/B) + e^(N0/B)),

    %X + B*ln(e^(Y0/B) + e^(N0/B)) 
    %=  B*ln(e^((Y0+?)/B) + e^(N0/B)),

    % X/B + ln(e^(Y0/B) + e^(N0/B))
    %=  ln(e^((Y0+?)/B) + e^(N0/B))

    % e^(X/B)*(e^(Y0/B) + e^(N0/B))
    %= e^((Y0+?)/B) + e^(N0/B)

    % e^(X/B)*e^(Y0/B) + (e^(X/B) - 1)*(e^(N0/B))
    %= e^((Y0+?)/B) 

    % ln(e^(X/B)*e^(Y0/B) + (e^(X/B) - 1)*(e^(N0/B)))
    %= (Y0+?)/B
    
    %this is the version we are using.
    % B*ln(e^(X/B)*e^(Y0/B) + (e^(X/B) - 1)*(e^(N0/B))) - Y0
    %= ?

    % B*ln(e^((X+Y0)/B) + (e^(X/B) - 1)*(e^(N0/B))) - Y0
    %= ?

    %= B*ln(e^((X+Y0)/B)(1 + (e^(X/B) - 1)*(e^(N0/B))*(e^((-X-Y0)/B)))) - Y0
    %= B*(((X+Y0)/B) + ln((1 + (e^(X/B) - 1)*(e^(N0/B))*(e^(-X-Y0))))) - Y0
    %= B*(((X+Y0)/B) + ln((1 + (e^(X/B) - 1)*(e^(N0-X-Y0/B))))) - Y0
    %= B*(((X+Y0)/B) + ln((1 + (e^((N0-Y0)/B) - e^((N0-X-Y0)/B))))) - Y0

    %this version could be a little faster.
    %= X + B*(ln(1 + e^((N0-Y0)/B) - e^((N0-X-Y0)/B)))
    %= X + B*(ln(e^((N0-Y0)/B)*(e^((Y0-N0)/B) + 1 - e^(-X/B)))
    %= X + N0 - Y0 + B*(ln(e^((Y0-N0)/B) + 1 - e^(-X/B))

    EXB = exp({rat, X, B}),
    EYB = exp({rat, Y0, B}),
    ENB = exp({rat, N0, B}),

    mul_to_int(
      B, ln(add(mul(EXB, EYB),
                mul(sub(EXB, 1), 
                    ENB)))) 
        - Y0.

   
    

change_in_market(B, Y0, N0, Y1, N1) ->
    %C1 = B*ln(e^(Y0/B) + e^(N0/B)),
    %C2 = B*ln(e^(Y1/B) + e^(N1/B)),
    %R = C2 - C1
    %R = B*(ln(e^(Y0/B) + e^(N0/B)) -
    %       ln(e^(Y1/B) + e^(N1/B)))
    %R = B*(ln((e^(Y0/B) + e^(N0/B)) /
    %          (e^(Y1/B) + e^(N1/B))))
    % let M = minimum(Y0, N0, Y1, N1)
    %R = B*(ln((e^(Y0/B) + e^(N0/B)) * e^(-M/B)/
    %          ((e^(Y1/B) + e^(N1/B)) * e^(-M/B))))
    %R = B*(ln((e^((Y0 - M)/B) + e^((N0-M)/B)) /
    %          ((e^((Y1 - M)/B) + e^((N1-M)/B)))))
    
    Result = veo_in_market(B, Y1, N1) -
        veo_in_market(B, Y0, N0),

%    Ma = max(max(Y0, N0), max(Y1, N1)),
%    Mi = min(min(Y0, N0), min(Y1, N1)),
%    M = (Ma + Mi) div 2,
%    R=ln(divide(
%           add(exp(make_rat(Y1 - M, B)),
%               exp(make_rat(N1 - M, B))),
%           add(exp(make_rat(Y0 - M, B)),
%               exp(make_rat(N0 - M, B))))),
%    Result2 = B * R#rat.t div R#rat.b,

%    F = ln(divide(add(1, exp(#rat{t= N1-Y1, b= B})),
%                  add(1, exp(#rat{t= N0-Y0, b= B})))),
%    Result3 = Y1 - Y0 + ((B * F#rat.t) div F#rat.b),
    
    Result.
    
veo_in_market(0, Y, N) when (is_integer(Y) and is_integer(N)) ->
    Y+N;
veo_in_market(B, Y, N) when (is_integer(Y) and is_integer(N)) ->
    %C = B * ln(e^(Y/B) + e^(N/B))
    % = B * (ln((Y/B)*(1 + e^((N-Y)/B))))
    % = B * (Y/B + ln(1 + e^((N-Y)/B)))
    % Y + B*ln(1 + e^((N-Y/B)))
    F = ln(add(1, exp(#rat{t = N-Y, b = B}))),
    Y + mul_to_int(B, F);
veo_in_market(B, Y, N) ->
    F = ln(add(1, exp(divide(sub(N, Y), B)))),
    to_int(Y) + mul_to_int(B, F).

ac(A, B) ->
    (A - B)*2 / (A + B).
   
there_and_back(T, B) -> 
    R = #rat{t = T, b = B},
    {{relative, ac(to_float(R), to_float(ln(exp(R))))},
     {absolute, to_float(sub(R, ln(exp(R))))}}.

accuracy(0) -> 
    1=2,
    {exp,
     {ac(math:exp(0.00000002), 
      to_float(
        exp(
          make_rat(1, 50000000))))},
     {ac(math:exp(1.000002), 
      to_float(
        exp(
          make_rat(1000002, 1000000))))},
     {ac(math:exp(6.9),
      to_float(
        exp(
          make_rat(69, 10))))},
     {ac(math:exp(7.0),
      to_float(
        exp(
          make_rat(70, 10))))},
     {ac(math:exp(6.6),
      to_float(
        exp(
          make_rat(66, 10))))},
     ln,
     {ac(math:log(0.0000001),
      to_float(
        ln(
          make_rat(1, 10000000))))},
     {ac(math:log(0.011),
      to_float(
        ln(
          make_rat(11, 1000))))},
     {ac(math:log(1000),
      to_float(
        ln(
          make_rat(10000, 10))))},
    there_and_back,
    {
      there_and_back(1, 100000),%fail
      there_and_back(1, 10000),% 0.09
      there_and_back(1, 1000),% 0.024
      there_and_back(1, 100),% 0.002
      there_and_back(1, 10),% 0.0003
      there_and_back(1, 1),% 0.00002
     there_and_back(69, 10)%0.000001
    }};
accuracy(1) -> 
    io:fwrite("accuracy of change_in_market\n"),
    A = change_in_market( 
                 500000000,
                 11000000000,
                 10100000000,
                 11000000000,
                 12000000000),
    B = 5,
    A2 = 100000000 * B * 
        (math:log(math:exp((110)/B)
                  + math:exp((101)/B))
         - math:log(math:exp((110)/B) 
                    + math:exp((120)/B))),
    {A, abs(A2), 
     (A - abs(A2))*2/(A+abs(A2))};
accuracy(2) -> 
    io:fwrite("accuracy of veo_in_market\n"),
    A = veo_in_market(500000000,
                      10700000000,
                      11400000000),
    A2 = 500000000 * math:log(math:exp((107)/5) +
                                  math:exp((114)/5)),
%    {%A, A2, 
    {abs(A - A2)*2/(A + A2)}.
    
    
    
      
range(N, N) -> [N];
range(A, B) when (A < B) -> 
    [A|range(A+1, B)].
speed() ->
    Many = 10000,
    R = range(0, Many),
    Rat = make_rat(1234567890, 321456789),
    T1 = erlang:timestamp(),
    lists:foldl(fun(_, _) ->
                        ln(Rat)
                end, 0, R),
    T2 = erlang:timestamp(),
    lists:foldl(fun(_, _) ->
                        exp(Rat)
                end, 0, R),
    T3 = erlang:timestamp(),
    {{ln, 1000000/(timer:now_diff(T2, T1)/Many)},
     {exp, 1000000/(timer:now_diff(T3, T2)/Many)},
    per_second}.


test_price_q2() ->
    Q1 =  1000000000,
    B =   1000000000,
    Q2 =  6000000000,
    P = price(B, Q1, Q2),
    Q2b = q2(P, B, Q1),
    {Q2b, Q2, to_float(inverse(P))}.

test_max_buy(0) ->
    B = 12345,
    Y = 0,
    N = 5555,
    X = 12300,
    S = max_buy(B, Y, N, X),
    X2 = change_in_market(B, Y, N, to_int(add(Y, S)), N),
    {X, X2, S};
test_max_buy(1) ->
    B = 500000000000,
    Y = 0,
    N = B*1,
    X = B*2,
    io:fwrite("max buy 1 0\n"),
    S = max_buy(B, Y, N, X),%big integer.
    io:fwrite("max buy 1 1\n"),
    Y2 = Y+S,
    X2 = change_in_market(B, Y, N, Y2, N),
    io:fwrite("max buy 1 2\n"),
    {X, X2, S}.

