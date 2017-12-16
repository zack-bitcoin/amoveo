-module(tree_test).
-export([test/0]).

test() ->
    S = success,
    io:fwrite("tree tests\n"),
    io:fwrite("account test\n"),
    S = accounts:test(),
    io:fwrite("channel test\n"),
    S = channels:test(),
    io:fwrite("existence test\n"),
    S = existence:test(),
    io:fwrite("oracles test\n"),
    S = oracles:test(),
    io:fwrite("oracle_bets test\n"),
    S = oracle_bets:test(),
    io:fwrite("orders test\n"),
    S = orders:test(),
    io:fwrite("governance test\n"),
    S = governance:test(),
    S.
    
