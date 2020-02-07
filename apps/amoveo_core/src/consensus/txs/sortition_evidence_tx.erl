-module(sortition_evidence_tx).
-export([test/0]).
-include("../../records.hrl").
test() ->
    Ownership = ok,
    Contract = ok,
    NewHeight = ok,
    Evidence = ok,
    CH = ownership:contract(Ownership),
    CH = hash:doit(Contract),
    OpGas = 10000,
    RamGas = 10000,
    Vars = 1000,
    Funs = 1000,
    State = chalang:new_state(NewHeight, 0, 0),
    Data = chalang:data_maker(OpGas, RamGas, Vars, Funs, Evidence, Contract, State, constants:hash_size(), 2, false),
    Data2 = chalang:run5(Evidence, Data),
    Data3 = chalang:run5(Contract, Data2),
    [<<1:32>>|_] = chalang:stack(Data3),
    ok.
