-module(forks).
-export([get/1]).

common(A, B) ->
    case application:get_env(amoveo_core, kind) of
	{ok, "production"} -> A;
	_ -> B
    end.
test_height() -> 0.
    %50.

get(1) -> common(4200, test_height());
get(2) -> common(9000, test_height());
get(3) -> common(9900, test_height());
get(4) -> common(26900, max(test_height(), constants:retarget_frequency()));
get(5) -> common(27500, max(test_height(), 1));
get(6) -> common(27700, test_height());
get(7) -> common(28135, 40);%test_height()).
get(8) -> common(36120, test_height());
get(9) -> common(39500, test_height());
get(10) -> common(47043, 1);
get(11) -> common(57000, test_height()).
