-module(tester).
-export([test/0]).
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
    lager:info("db test"),
    S = db:test(),
    lager:info("sign test"),
    S = testnet_sign:test(),
    lager:info("packer test"),
    S = packer:test(),
    lager:info("encryption test"),
    S = encryption:test(),
    lager:info("fractions test"),
    S = ae_core_fractions:test(),
    lager:info("merkel tree tests"),
    S = tree_test:test(),
    lager:info("block hashes test"),
    S = block_hashes:test(),
    lager:info("block test"),
    S = block:test(),
    lager:info("spk test"),
    S = spk:test(),
    lager:info("txs test"),
    S = test_txs:test(),
    lager:info("existence test"),
    S = existence:test(),
    lager:info("order_book test"),
    S = order_book:test(),
    lager:info("proofs test"),
    S = proofs:test(),
    %lager:info("market test"),
    %S = market:test(), %% This test has side effects i.e. it absorbs transactions.
    %lager:info("chalang test"),
    %S = test_chalang:test(), %% Module test_chalang needs review as even passing folder it runs other tests with hardcoded folder.
    %lager:info("block header test"),
    %S = headers:test(), %% This test has side effects i.e. it absorbs header.
    %lager:info("keys test"),
    %S = keys:test(), %% Fails with `{error,<<"cannot sign">>}`.
    S.
