-module(tester).
-export([test/0]).
test() ->
    %this tests modules individually. To test all of them together, use block_tree:test() which adds some test blocks to the blocktree.
    %you need to run clean.sh to empty out the databases before running this test. Make sure you don't download anything from peers before running this test.
    %you need to run clean.sh after running this test, before you can run a Flying Fox node.
    case keys:status() of
	unlocked -> test1();
	_ -> "you need to unlock with keys:unlock(""password"") first"
    end.

test1() ->
    S = success,
    io:fwrite("constants test\n"),
    S = constants:test(),
    S = db:test(),
    io:fwrite("sign test\n"),
    S = testnet_sign:test(),
    S = packer:test(),
    io:fwrite("encryption test\n"),
    S = encryption:test(),
    %io:fwrite("fractions test\n"),
    %S = ae_core_fractions:test(),
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
    %io:fwrite("mine test\n"),
    %S = block:mine_test(),
    %io:fwrite("market smart contract test\n"),
    io:fwrite("market test\n"),
    S = market:test(),
    io:fwrite("order_book test\n"),
    S = order_book:test(),
    %S = inbox:test(),
    %io:fwrite("chalang test\n"),
    %S = test_chalang:test("deps/chalang/examples/"),
    %S = channel_manager:test(),
    %S = arbitrage:test(),
    %S = mail:test(),
    S.
    
