-module(tester).
-export([test/0]).
test() ->
    case keys:status() of
	unlocked -> test1();
	_ -> "you need to unlock with keys:unlock(""password"") first"
    end.
test_helper([]) -> success;
test_helper([A|B]) ->
    io:fwrite(atom_to_list(A) ++ " test\n"),
    success = A:test(),
    test_helper(B).
test1() ->
    timer:sleep(2000),
    S = success,
    Tests = [db, testnet_sign, packer, encryption, tree_test, block_hashes, block, spk, test_txs, existence, order_book, proofs, market], %headers, keys],
    S = test_helper(Tests).

    
