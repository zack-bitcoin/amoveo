-module(tester).
-export([test/0, test_helper/1, encryption_test/0, coverage_test/0]).
test() ->
    case keys:status() of
	unlocked -> test1();
	_ -> "you need to unlock with keys:unlock(""password"") first"
    end.
test_helper([]) -> success;
test_helper([A|B]) ->
    io:fwrite(atom_to_list(A) ++ " test\n"),
    success = A:test(),
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    test_helper(B).
test1() ->
    %timer:sleep(2000),
    S = success,
    headers:dump(),
    block:initialize_chain(),
    tx_pool:dump(),
    Tests = [secrets, db, signing, packer, tree_test, block_hashes, block, spk, test_txs, existence, %order_book, 
proofs], %, lisp_market2, lisp_scalar], %headers, keys],
    S = test_helper(Tests).

    
encryption_test() ->
    %EM=encryption:send_msg(Message, base64:encode(Pubkey), base64:encode(R#f.pub), base64:encode(R#f.priv)),
    Pub1_64 = "BNFsD42eXjwHd4PyP4lODu+aybYjmVJF0bA0UYNcJ2/cELBl5Z6IA639jUl1km+8YAD3aL3of+SqLI8emuhsa2c=",
    Priv1_64 = "wruxx99+2hK4cT+j1SkJhV6VzBxgbl11iHxnq6ghASA=",
    Pub2_64 = "BA7HtKYPIvUwQjmUqNQ0UMsxHu+KtISveg45Jl+tl/y6OMgtyC3rE4/YrEHLtTprCPsxcus5CbhmlWo9IDKfnzo=",
    Priv2_64 = "4a6E2IK3hhhP2dK8xGYaUqg23Fk/n/Ms2VuORKC5Xvo=",
    EM = encryption:send_msg([1,2,3], Pub2_64, Pub1_64, Priv1_64),%msg, to, from
    io:fwrite("encryption_test\n"),
    io:fwrite(packer:pack(EM)),
    ok.
    

coverage_test() ->
    Start = 161191,
    End = 251202,
    %End = 162000,
    coverage_test3(Start, End, dict:new()).

coverage_test3(Start, End, D) 
  when not(End > Start) ->
    D;
coverage_test3(Start, End, D) ->
    1=2,
    io:fwrite("coverage test height: "),
    io:fwrite(integer_to_list(Start)),
    io:fwrite("\n"),
    %Bs0 = block_db:read_by_height(Start),
    %Bs = block_db:uncompress(Bs0),
    Bs = block_db3:read(Start, Start),
    %Bs is a dictionary storing blocks by blockhash.
    Keys = dict:fetch_keys(Bs),
    D2 = lists:foldl(
            fun(K, D1) -> 
                    Block = dict:fetch(K, Bs),
                    Txs = tl(element(11, Block)),
                    Txs2 = lists:map(
                             fun(X) -> 
                                     element(2, X) 
                             end, Txs),
                    coverage_txs(Txs2, D1)
            end, D, Keys),
    coverage_test3(Start + length(Keys), End, D2).
%Bs.
    

coverage_test2(A, B, D) when (A > B) ->
    D;
coverage_test2(A, B, D) ->
    if
        ((A rem 100) == 0) ->
            io:fwrite("tester coverage test height: "),
            io:fwrite(integer_to_list(A)),
            io:fwrite("\n");
        true -> ok
    end,
    Block = block:get_by_height(A),
    Txs = tl(element(11, Block)),
    Txs2 = lists:map(
             fun(X) -> element(2, X) end, Txs),
    D2 = coverage_txs(Txs2, D),
    coverage_test2(A+1, B, D2).

coverage_txs([], D) -> D;
coverage_txs([Tx|T], D) -> 
    if
        is_binary(Tx) -> io:fwrite({Tx});
        true -> ok
    end,
    Type = element(1, Tx),
    D2 = case Type of
             multi_tx -> 
                 Txs = element(5, Tx),
                 coverage_txs(Txs, D);
             _ ->
                 cov_inc(Type, D)
         end,
    coverage_txs(T, D2).

cov_inc(X, D) ->
    case dict:find(X, D) of
        error -> dict:store(X, 1, D);
        {ok, N} -> dict:store(X, N+1, D)
    end.
            
