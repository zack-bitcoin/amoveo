-module(fractions).
-export([new/2,negate/1,add/2,subtract/2,multiply/2,divide/2,to_int/1,test/0, multiply_int/2, exponent/2, less_than/2, equal/2, is_fraction/1,sqrt/1]).
-record(f, {top = 0, bottom = 0}).
is_fraction(X) when not is_record(X, f) -> false;
is_fraction({f, _, Y}) when not is_integer(Y) -> false;
is_fraction({f, Y, _}) when not is_integer(Y) -> false;
is_fraction({f, _, Y}) when Y == 0 -> false;
is_fraction(_) -> true.
sqrt({f, A, B}) ->
    sqrt_helper({f, A, B}, {f, 1, 2}).
sqrt_helper(A, Guess) ->
    B = subtract(A, multiply(Guess, Guess)),
    Bool = (less_than(B, {f, 1, 1000}) and (not less_than(B, {f, -1, 1000}))), %correct to 8 decimal places.
    if
	Bool -> Guess;
	true -> 
	    Sum = add(Guess, divide(A, Guess)),
	    Improved = divide(Sum, {f, 2, 1}),
	    sqrt_helper(A, Improved)
    end.
equal(A, B) ->
    A#f.top * B#f.bottom == B#f.top * A#f.bottom.
less_than(A, B) ->
    A#f.top * B#f.bottom < B#f.top * A#f.bottom.
new(T,B) -> #f{top = T, bottom = B}.
negate(A) -> #f{top = -A#f.top, bottom = A#f.bottom}.
subtract(A, B) -> add(A, negate(B)).
add(A, B) -> simplify(#f{top = (A#f.top * B#f.bottom) + (A#f.bottom * B#f.top) , bottom = A#f.bottom * B#f.bottom}).
multiply(A, B) -> simplify(#f{top = A#f.top * B#f.top, bottom = A#f.bottom * B#f.bottom}).
divide(A, B) -> simplify(#f{top = A#f.top * B#f.bottom, bottom = A#f.bottom * B#f.top}).
to_int(A) -> A#f.top div A#f.bottom.
multiply_int(F, I) -> F#f.top * I div F#f.bottom.
simplify(F) -> simplify_lcd(simplify_size(F)).
simplify_lcd(F) ->
    L = lcd(F#f.top, F#f.bottom),
    #f{top = F#f.top div L, bottom = F#f.bottom div L}.
simplify_size(F) ->
    IC = 281474976710656,
    %X = F#f.bottom div IC,
    %Y = F#f.top div IC,
    Z = if 
	((F#f.bottom > IC) and (F#f.top > IC)) -> IC; 
	true -> 1 
    end,
    #f{top = F#f.top div Z, bottom = F#f.bottom div Z}.
exponent(_, 0) -> #f{top = 1, bottom = 1};
exponent(F, 1) -> F;
exponent(F, N) when N rem 2 == 0 ->
    exponent(multiply(F, F), N div 2);
exponent(F, N) -> multiply(F, exponent(F, N - 1)).
lcd(A, 0) -> A;
lcd(A, B) -> lcd(B, A rem B).
test() ->
    A = new(1, 3),
    B = new(2, 5),
    C = multiply(A, B),
    C = new(2, 15),
    B = divide(C, A),
    9 = lcd(27, 9),
    5 = lcd(25, 15),
    success.
    
