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
    true = is_binary(Secret),
    chalang_compiler:doit("binary " ++ integer_to_list(S) ++ " " ++ Secret ++ " int " ++ integer_to_list(N) + " ").
    
resolve_ss(SPK, Secret, TheirSecret) ->
    Acc1 = spk:acc1(SPK),
    Acc2 = spk:acc2(SPK),
    MyID = keys:id(),
    {S1, S2} = case MyID of
	    Acc1 -> {Secret, TheirSecret};
	    Acc2 -> {TheirSecret, Secret};
	    X -> X = Acc1
	end,
    
    S = " binary 32 " ++S1 ++ " binary 32 " ++ S2 ++ " int 3 ",
    compiler_chalang:doit(S).
