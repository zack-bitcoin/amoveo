-module(compile).

-export([doit/1, doit/2]).

doit(F, Front) ->
    %for satoshi dice for a pair of users each betting 1000, Front is <<"Amount 1000">>
    %and F is "src/bets/dice.fs"
    {ok, Text} = file:read_file(F),
    compiler_chalang:doit(<<Front/binary, Text/binary>>).
    
