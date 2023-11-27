-module(futarchy_batch).
-export([cf2f/2, log_table/1, to_float/1, 
         exp/1, ln/1, make_rat/2, e/0, pow/2,
         accuracy/0, speed/0, ln2/0,
         veo_in_market/3, change_in_market/5, unit/0]).


%==========TLDR, limits of this software=======
% set unit size to 1 veo for now. we can increase or decrease later via hard fork if the market cap changes much.
% don't match trades where the odds are steeper than 1:1000. only provide liquidity inside the bounds.
% tx burn fee must be bigger than (unit)*(0.00003).
% do not match limit orders if they are betting less than (unit)/10000.
%================================================

%%%%%%%%%_____Long Docs_____%%%%%%%%%%%

%logrithm algorithm based on this page: https://love2d.org/forums/viewtopic.php?p=231486

%in this rational numbers system, the numerator and denominator of the rational each need to be between ?limit and -?limit.
%so, if you try to represent numbers very near to ?limit, then the accuracy starts decreasing.
% accuracy = exp(N) / ?limit
%so ideally we shouldn't return a value bigger than `1 200 000`. Which means we shouldn't have an input bigger than 14.
% so use a scaling factor to move the numbers we care about into the region that is accurate. Instead of calculating the total number of coins in the market, calculate something 1000x smaller than that.
%tx fees are minimum 0.00151118 satoshis.

%This is going to be used to implement the LMSR, so this limitation is workable.

%LMSR
% money in market 
%C = B*ln(e^(q1/B) + e^(q2/B))
%
%instantaneous price 
%price = e^(q1/B) / (e^(q1/B) + e^(q2/B))

%The thing about LMSR prices is that it only depends on the difference between Q1 and Q2. Not their absolute values. Because we only want to calculate the difference between C1, and C2. not their absolute values.

%for example, if Q1 is bigger, and someone is buying Q1b of the Q1 shares
%C1 = B * ln(e^((Q1 -  Q2)/B) + e^(0/B))
%C2 = B * ln(e^((Q1 + Q1b - Q2)/B) + e^(0/B))
%C2 - C1 is how much you need to pay to buy Q1b shares
% = B* (ln(e^((Q1+Q2b -Q2)/B) + 1) - ln(e^((Q1-Q2)/B) + 1)

%If someone is buying Q2b of the Q2 shares
%C1 = B * ln(e^((Q1 - Q2)/B) + 1)
%C2 = B * ln(e^((Q1 - Q2)/B) + e^(Q2b/B))

%as long as ((Q1 + Q1b - Q2)/B) is less than 14, then it is accurate to 0.1%.
%if `(Q1 + Q1b - Q2) / B) > 14`, that means the odds = `e^(q2/B) / (e^(Q1/B) + e^(Q2/B))` must be a very small value.
%again, taking advantage of symmetry to rewrite the odds equation
% `e^(1/B) / (e^(1/B) + e^((Q1 - Q2 + 1)/B)
%now plugging in 14 for `(Q1 - Q2 + 1)/B`
% so the price is `e^(1/B) / (e^(1/B) + e^14) ~= 1/(1+1202604)`, which is around `0.0000008` . practically zero. 
%this implies betting odds of 1:1 200 000, which is very far from the 50:50 odds that we would want in order for the prediction market to be accurate.
% So, we can just shut off the LMSR when it is outside of certain price bounds, like 1:1000, and provide zero liquidity out of those bounds. Limit orders could still be matched.
%this would prevent rounding errors attacks.

%if our price limit is 1:1000, that implies that the biggest difference between Q1 an Q2 is 6.9 * B.
%so, the biggest number we need to take the log of is around 1000. and the biggest number we need to take the exponent of is 6.9.
%within these bounds, the formula are accurate to around 0.00003, if you do ln(exp(X)) for example.
 %so, we would need tx fees to be bigger than this, to make it impossible to print money from the mechanism.
 % if the formula is counting in units of 1000 veo, then 0.00003 is 0.03 veo. 
%also, the minimum bet size we can allow is 0.00003. 


-record(rat, {t, b}).
-define(limit, 4294967296). %2^32

unit() -> 100.%we count in units of this many satoshis.
%highest value we can represent is approximately 

make_rat(T, B) -> simplify(#rat{t = T, b = B}).
to_float(Z) -> Z#rat.t / Z#rat.b.
to_int(Z) -> Z#rat.t div Z#rat.b.
zero(X) -> X#rat.t == 0.
positive(#rat{t = T, b = B}) -> (T*B) > 0.
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
mul(X, R) when is_integer(X) ->
    mul(R, #rat{t = X, b = 1});
mul(#rat{t = T1, b = B1}, 
    #rat{t = T2, b = B2}) ->
    simplify(#rat{t = T1*T2, b = B1 * B2}).
mul([A]) -> A;
mul([A,B|T]) -> 
    mul([mul(A, B)|T]).
square(X) -> mul(X, X).
sub(#rat{t = T1, b = B1}, 
    #rat{t = T2, b = B2}) -> 
    simplify(#rat{t = (T1*B2) - (T2*B1), 
                     b = B1*B2}).
add(A, B) when is_integer(A) ->
    add(B, #rat{t = A, b = 1});
add(A, B) -> sub(A, negative(B)).
add([A]) -> A;
add([A, B|T]) -> 
    add([add(A, B)|T]).
divide(A, B) -> mul(A, inverse(B)).
inverse(X) when is_integer(X) ->
    #rat{t = 1, b = X};
inverse(#rat{t = T, b = B}) ->
    #rat{t = B, b = T}.
negative(N = #rat{t = T}) ->
    N#rat{t = -T}.
less_than(#rat{t = T1, b = B1},
          #rat{t = T2, b = B2}) ->
    (T1 * B2) < (T2 * B1).
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


e() -> #rat{t = 5279458664,b =1942204303}.
    %cf2f(2, [1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, 1, 10, 1, 1, 12, 1, 1, 14, 1, 1, 16, 1, 1, 18, 1, 1]).
    
zero() ->
    #rat{t = 0, b = 1}.
one() ->
    #rat{t = 1, b = 1}.
two() ->
    #rat{t = 2, b = 1}.
half() ->
    #rat{t = 1, b = 2}.
ln2() ->
    #rat{t = 3342617437,b = 4822377600}.
%    cf2f(0, [1, 2, 3, 1, 6, 3, 1, 1, 2, 1, 1, 1, 1, 3, 10, 1, 1, 1, 2, 1, 1, 1, 1, 3, 2, 3, 1, 13, 7, 4]).
exp(#rat{t = T, b = B}) ->
    %e^(T / B) = 
    %e^(T div B) * e^((T rem B)/B)
    Xint = T div B,
    Xfract = #rat{t = T rem B, b = B},
    Rint = pow(e(), Xint),%e^(T div B)
    Xfract2 = mul(Xfract, Xfract),
    Xfract3 = mul(Xfract, Xfract2),
    Xfract4 = mul(Xfract2, Xfract2),
    Xfract5 = mul(Xfract2, Xfract3),
    Xfract6 = mul(Xfract3, Xfract3),
    Xfract7 = mul(Xfract4, Xfract3),
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
               mul(Xfract7, #rat{t = 1, b = 5040})
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

frexp(R = #rat{}) ->
    %-> {M, X} s.t. 1 >= |M| >= 0.5 and M*(2^X) == T/B
    NP = not(positive(R)),
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
ln_loop(_Arg, _Prod, Sum, 16) -> Sum;
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
    #rat{t = 551104173,b = 1359190130};
%    cf2f(0, [2,2,6,1,11,2,1,2,2,1,4,3,1,1,7,2,1,1,4,1,2,1,2,1]);
log_table(2) ->
     #rat{t = 1497134811,b = 6709290061};
%    cf2f(0, [4,2,12,1,21,2,2,1,89,1,5,3,1,76,1,7,60,2,2,2,22,6]);
log_table(3) ->
    #rat{t = 283987567,b = 2411107556};
%    cf2f(0, [8, 2, 24, 1, 41, 2, 6, 9, 2, 83, 8, 5, 1, 2, 20, 3, 1, 2, 4, 3, 7, 2, 1, 4, 1, 4, 1, 1, 1]);
log_table(4) ->
    #rat{t = 264413820,b = 4361492281};
    %cf2f(0, [16, 2, 48, 1, 81, 2, 12, 3, 36, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 6, 1, 2, 1, 12]);
log_table(5) ->
    #rat{t = 133446201,b = 4336659364};
    %cf2f(0, [32, 2, 96, 1, 161, 2, 24, 1, 3, 2, 7, 1, 2, 7, 1, 22, 105, 1, 1, 3, 7, 4, 7, 1, 3, 2, 1]);
log_table(6) ->
      #rat{t = 67586257,b = 4359226273};
%    cf2f(0, [64, 2, 192, 1, 321, 2, 49, 1, 1, 1, 144, 2, 5, 5, 32, 1, 7, 2, 2, 2, 3, 2, 5, 6, 2, 1]);
log_table(7) ->
    #rat{t = 11201457,b = 1439379995};
    %cf2f(0, [128, 2, 384, 1, 641, 2, 99, 2, 3, 1, 31, 2, 1, 7, 2, 2, 8, 11, 16, 1, 1, 1, 1, 2, 12]);
log_table(8) ->
    #rat{t = 16794959,b = 4307901582};
    %cf2f(0, [256, 2, 768, 1, 1281, 2, 198, 1, 5193, 8, 3, 100, 33, 2, 1, 67, 3, 2, 1, 5, 2, 1, 1]);
log_table(9) ->
    #rat{t = 8491613,b = 4351950448};
    %cf2f(0, [512, 2, 1536, 1, 2561, 2, 398, 9, 128, 72, 6, 4, 41640, 2, 1, 2, 1, 5, 6, 3, 277]);
log_table(10) ->
    #rat{t = 4192361,b = 4295074331};
    %cf2f(0, [1024, 2, 3072, 1, 5121, 2, 796, 3, 2304, 1, 3, 1, 3, 1, 11, 1, 2, 1, 2, 2, 1, 2, 13]);
log_table(11) ->
    #rat{t = 2097520,b = 4296771473};
    %cf2f(0, [2048, 2, 6144, 1, 10241, 2, 1592, 1, 3, 2, 511, 1, 2, 7, 1, 2, 24, 1, 2, 4, 2, 3, 2]);
log_table(12) ->
    #rat{t = 1048669,b = 4295876303};
    %cf2f(0, [4096, 2, 12288, 1, 20481, 2, 3185, 1, 1, 1, 9216, 2, 5, 2, 49, 1, 1, 1, 1, 3, 2, 2]);
log_table(13) ->
    #rat{t = 131086,b = 1073923321};
    %cf2f(0, [8192, 2, 24576, 1, 40961, 2, 6371, 2, 3, 1, 2047, 2, 1, 7, 2, 1, 99, 2, 4, 2, 266]);
log_table(14) ->
%    #rat{t = 2147532879,b = 35186252445053};
    #rat{t = 262150,b = 4295196831};
    %cf2f(0, [16384, 2, 49152, 1, 81921, 2, 12742, 1, 331785, 8, 200, 7, 1, 2, 3, 1, 2129, 4]);
log_table(15) ->
%    #rat{t = 2147542565,b = 70371748535741}.
    #rat{t = 65541,b = 2147695010}.
    %cf2f(0, [32768, 2, 98304, 1, 163841, 2, 25486, 9, 8192, 72, 400, 2, 24, 2, 4259, 2, 2]).

change_in_market(Ba, Y0a, N0a, Y1a, N1a) ->
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

    B = Ba div unit(),
    Y0 = Y0a div unit(),
    N0 = N0a div unit(),
    Y1 = Y1a div unit(),
    N1 = N1a div unit(),
    Ma = max(max(Y0, N0), max(Y1, N1)),
    Mi = min(min(Y0, N0), min(Y1, N1)),
    M = (Ma + Mi) div 2,
    R=ln(divide(
           add(exp(make_rat(Y1 - M, B)),
               exp(make_rat(N1 - M, B))),
           add(exp(make_rat(Y0 - M, B)),
               exp(make_rat(N0 - M, B))))),
    to_int(mul(R, B)) * unit().
    
veo_in_market(B0, Y0, N0) ->
    %C = B * ln(e^(Y/B) + e^(N/B))
    %this can be written like
    %C = B * ln(e^(I0 + D0) + e^(I1 + D1))
    %C = B * ln(e^(I0)*e^(D0) + e^(I1)*e^(D1))

    B = B0 div unit(),
    Y = Y0 div unit(),
    N = N0 div unit(),

    I0 = Y div B,
    I1 = N div B,
    D0 = #rat{t = Y rem B, b = B},
    D1 = #rat{t = N rem B, b = B},
    F = if
            (I0 =< I1) ->
      %Assuming I0 is =< I1.
    %C = B * ln(e^(I0)(e^(D0) + e^(I1-I0)*e^(D1)))
    %C = B*(I0 + ln(e^(D0) + e^(D1 + I1 - I0))
                add(I0,
                    ln(add(exp(D0),
                           exp(add(I1 - I0,
                                   D1)))));
            true ->
    %C = B*(I1 + ln(e^(D1) + e^(D0 + I0 - I1))
                add(I1,
                    ln(add(exp(D1),
                           exp(add(I0 - I1,
                                   D0)))))
        end,
    to_int(mul(B, F)) * unit().

ac(A, B) ->
    (A - B)*2 / (A + B).
   
there_and_back(T, B) -> 
    R = #rat{t = T, b = B},
    {{relative, ac(to_float(R), to_float(ln(exp(R))))},
     {absolute, to_float(sub(R, ln(exp(R))))}}.

accuracy() -> 
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
    }}.
      
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