-module(forks).
-export([get/1]).

get(1) ->
    case application:get_env(amoveo_core, kind) of
	{ok, "production"} -> 4200;
	_ -> 0
    end;
get(2) ->
    case application:get_env(amoveo_core, kind) of
	{ok, "production"} -> 9000;
	_ -> 50
    end.
    
