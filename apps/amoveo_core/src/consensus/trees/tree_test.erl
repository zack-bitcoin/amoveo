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
    io:fwrite("matched test\n"),
    S = matched:test(),
    io:fwrite("unmatched test\n"),
    S = unmatched:test(),
    io:fwrite("sub_accounts test\n"),
    S = sub_accounts:test(),
    io:fwrite("contracts test\n"),
    S = contracts:test(),
    io:fwrite("trades test\n"),
    S = trades:test(),
    io:fwrite("markets test\n"),
    S = markets:test(),
    io:fwrite("stablecoins test\n"),
    S = stablecoins:test(),

    io:fwrite("tree tests done\n"),
    S.
    
