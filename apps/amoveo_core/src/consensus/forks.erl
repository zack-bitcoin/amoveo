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
get(4) -> common(26900, test_height());
get(5) -> common(27500, 1).
    
    
