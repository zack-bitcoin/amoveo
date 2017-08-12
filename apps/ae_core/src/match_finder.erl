-module(match_finder).
-export([doit/0]).

doit() ->
    doit2(298, 63).
doit2(N, M) ->
    <<X, _/binary>> = accounts:ensure_decoded_hashed(<<N:520>>),
    case X of
        M -> N;
        _ -> doit2(N+1, M)
    end.
            
    %744, 298
