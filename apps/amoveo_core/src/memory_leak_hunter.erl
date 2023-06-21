-module(memory_leak_hunter).
-export([take_picture/0]).

take_picture() ->
    PI = [{erlang:process_info(Pid, memory), erlang:process_info(Pid)}||Pid<-erlang:processes()],
    PI2 = lists:sort(fun({{memory, X}, _}, {{memory, Y}, _}) -> X > Y end, PI),
    PI2.
    
