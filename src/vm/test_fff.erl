-module(test_fff).
-export([doit/2, test/0]).
-define(loc, "src/vm/fff/").
doit(X, Gas) ->
    {ok, A} = file:read_file(?loc ++ X ++ ".fs"),
    B = compiler:compile(<<A/binary, <<" test ">>/binary >>),
    language:run(B, Gas).

test() ->
    T = [true],
    T = doit("hashlock", 1000),
    T = doit("recursion", 1000),
    T = doit("function", 1000),
    T = doit("macro", 1000),
    T = doit("variable", 1000),
    T = doit("growing_database", 1000),
    T = doit("sidechain", 1000),
    T = doit("map", 4000),
    T = doit("reduce", 4000),
    T = doit("sort", 10000),
    T = doit("check", 1000),
    X = oracle:test(),
    X = doit("oracle", 180000),
    T = doit("2of3multisig", 2000),
    T = doit("weighted_multisig", 2000),
    T = doit("commit_reveal", 2000),
    T = doit("double_sign_slash", 8000),
    T = doit("weighted_multisig_big", 3000),
    T = doit("weighted_commit_reveal", 80000),
    success.
    
