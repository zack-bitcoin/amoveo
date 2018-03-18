-module(forks).
-export([get/1]).

common(A, B) ->
    case application:get_env(amoveo_core, kind) of
	{ok, "production"} -> A;
	_ -> B
    end.
get(1) -> common(4200, 0);
get(2) -> common(9000, 0);
get(3) -> common(9900, 0).
    
