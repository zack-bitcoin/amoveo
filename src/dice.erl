-module(dice).
-export([make_ss/2, resolve_ss/2]).

make_ss(SPK, Secret) ->
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    N = case MyID of
	    Acc1 -> 1;
	    Acc2 -> 2;
	    X -> X = Acc1
	end,
    chalang_compiler:doit("binary 12 " ++ Secret ++ " int " ++ integer_to_list(N) + " ").
    
resolve_ss(Secret, ThereSecret) ->
    S = " binary 12 " ++Secret ++ " binary 12 " ++ TheirSecret ++ " int 3 ",
    compiler_chalang:doit(S).
