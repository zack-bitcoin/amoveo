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
    S = block_tree:test(),
    S = channels:test(),
    S = to_channel_tx:test(),
    S = language:test(),
    S = all_secrets:test(),
    S = hash:test(),
    S = block_dump:test(),
    S = block_pointers:test(),
    S = block_finality:test(),
    S = packer:test(),
    S = accounts:test(),
    S = encryption:test(),
    S = db:test(),
    S = testnet_sign:test(),
    S = accounts:test(),
    S = secrets:test(),
    S = constants:test(),
    S = fractions:test(),
    S = inbox:test(),
    S = mail:test(),
    S = arbitrage:test(),
    S = channel_manager:test(),
    S = test_fff:test(),
    S.
    
