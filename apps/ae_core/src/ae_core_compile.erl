-module(ae_core_compile).

-export([doit/2]).

doit(X, Y) when is_list(Y) ->
    doit(X, list_to_binary(Y));
doit(F, Front) ->
    %for satoshi dice for a pair of users each betting 1000, Front is <<"Amount 1000">>
    %and F is "src/bets/dice.fs - INFO: it got moved to /priv dir !!!"
    {ok, Text} = file:read_file(F),
    compiler_chalang:doit(<<Front/binary, Text/binary>>).
    
