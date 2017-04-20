-module(dice).
-export([make_ss/2, resolve_ss/3]).

make_ss(SPK, Secret) ->
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    MyID = keys:id(),
    N = case MyID of
	    Acc1 -> 1;
	    Acc2 -> 2;
	    X -> X = Acc1
	end,
    S = size(Secret),
    compiler_chalang:doit("binary " ++ integer_to_list(S) ++ " " ++ binary_to_list(base64:encode(Secret)) ++ " int " ++ integer_to_list(N) ++ " ").

resolve_ss(SPK, Secret, TheirSecret) ->
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    MyID = keys:id(),
    {S1, S2} = case MyID of
	    Acc1 -> {Secret, TheirSecret};
	    Acc2 -> {TheirSecret, Secret};
	    X -> X = Acc1
	end,
    T1 = integer_to_list(size(S1)),
    T2 = integer_to_list(size(S2)),
    S1s = binary_to_list(base64:encode(S1)),
    S2s = binary_to_list(base64:encode(S2)),
    S = " binary " ++ T1 ++ " " ++ S1s ++ " binary " ++ T2 ++ " "++ S2s ++ " int 3 ",
    compiler_chalang:doit(S).
