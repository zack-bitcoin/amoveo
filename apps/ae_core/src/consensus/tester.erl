-module(tester).
-export([test/0, oracle_test/0]).
test() ->
    lager:info("You need to clean the state of the node before running this test. Make sure you don't download anything from peers before running this test.~n"
               "You need to clean the state of the node after running this test, before you can run an Aeternity node."),
    case keys:status() of
	unlocked -> test1();
	_ -> "you need to unlock with keys:unlock(""password"") first"
    end.

test1() ->
    timer:sleep(2000),
    S = success,
    io:fwrite("db test\n"),
    S = db:test(),
    io:fwrite("sign test\n"),
    S = testnet_sign:test(),
    io:fwrite("packer test\n"),
    S = packer:test(),
    io:fwrite("encryption test\n"),
    S = encryption:test(),
    io:fwrite("fractions test\n"),
    S = ae_core_fractions:test(),
    io:fwrite("merkel tree tests\n"),
    S = tree_test:test(),
    io:fwrite("block hashes test\n"),
    S = block_hashes:test(),
    io:fwrite("block test\n"),
    S = block:test(),
    io:fwrite("spk test\n"),
    S = spk:test(),
    io:fwrite("txs test\n"),
    S = test_txs:test(),
    io:fwrite("existence test\n"),
    S = existence:test(),
    io:fwrite("order_book test\n"),
    S = order_book:test(),
    io:fwrite("proofs test\n"),
    S = proofs:test(),
    io:fwrite("market test\n"),
    S = market:test(), %% This test has side effects i.e. it absorbs transactions.
    %io:fwrite("chalang test\n"),
    %S = test_chalang:test(), %% Module test_chalang needs review as even passing folder it runs other tests with hardcoded folder.
    %io:fwrite("block header test\n"),
    %S = headers:test(), %% This test has side effects i.e. it absorbs header.
    %io:fwrite("keys test\n"),
    %S = keys:test(), %% Fails with `{error,<<"cannot sign">>}`.
    S.

oracle_test() ->
    N = 2,
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    {_, Acc, _} = accounts:get(keys:pubkey(), Accounts),
    OBTree = accounts:bets(Acc),
    {_, OB, _} = oracle_bets:get(N, OBTree),
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(N, Oracles),
    OrdersTree = oracles:orders(Oracle),
    {_, Order, _} = orders:get(keys:pubkey(), OrdersTree),
    {OB, Order}.
    
