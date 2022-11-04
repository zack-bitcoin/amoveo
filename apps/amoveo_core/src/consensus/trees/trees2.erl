-module(trees2).
-export([test/1]).

range(N, N) ->
    [N];
range(N, M) when N < M ->
    [N|range(N+1, M)].

test(0) ->
    CFG = tree:cfg(amoveo),
    Loc = 1,
    Pairs = 
        lists:map(
          fun(N) ->
                  Key = crypto:strong_rand_bytes(32),
                  Val = crypto:strong_rand_bytes(32),
                  Meta = crypto:strong_rand_bytes(8),
                  {Key, Val, Meta}
          end, range(1, 32)),
    Leaves = lists:map(
               fun({Key, Val, Meta}) ->
                       leaf_verkle:new(
                         Key, Val, Meta, CFG)
               end, Pairs), 
    Keys = lists:map(
             fun({Key, _, _}) ->
                     Key
             end, Pairs),
    %io:fwrite(hd(Leaves)),
    %
    %io:fwrite(cfg_verkle:meta(CFG)),
    {Loc2, stem, _} = store_verkle:batch(
                        Leaves, Loc, CFG),
    {Proof, _} = get_verkle:batch(Keys, Loc2, CFG),
    {FastProof, _} = 
        get_verkle:batch(Keys, Loc2, CFG, fast),
    Root = stem_verkle:root(
             stem_verkle:get(Loc2, CFG)),
    {true, Leaves2, _} = 
        verify_verkle:proof(Proof, CFG),
    {true, Leaves2, _} = 
        verify_verkle:proof(FastProof, CFG),
    success.
