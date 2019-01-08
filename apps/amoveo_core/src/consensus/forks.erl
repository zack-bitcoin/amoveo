-module(forks).
-export([get/1]).

common(A, B) ->
    case application:get_env(amoveo_core, kind) of
	{ok, "production"} -> A;
	_ -> B
    end.
test_height() -> 0.
    %50.
get(1) -> common(0, test_height());
get(2) -> common(0, test_height());
get(3) -> common(0, test_height());
get(4) -> common(constants:retarget_frequency(), max(test_height(), constants:retarget_frequency()));
get(5) -> common(1, max(test_height(), 1));
get(6) -> common(0, test_height());
get(7) -> common(40, 40);%test_height()).
get(8) -> common(0, test_height());
get(9) -> common(0, test_height());
get(10) -> common(1, 1).
