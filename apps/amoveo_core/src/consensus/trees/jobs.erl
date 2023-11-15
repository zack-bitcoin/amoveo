-module(jobs).
-export([key_to_int/1, make_id/2, dict_get/2, salary_update/5, 
         dict_write/2, dict_write/3, dict_get/3, n64/0,
         curve/1, salary_per_block/2, min_balance_ratio_check/4,
         rat_exponent/3, maybe_simplify/2, update/3]).

-include("../../records.hrl").

curve(_Height) -> 1000.
n64() -> 18446744073709551616.

key_to_int(#job{id = <<X:256>>}) ->
    X;
key_to_int(<<X:256>>) ->
    X.


make_id(Worker, Salt) when is_integer(Salt) ->
    make_id(Worker, <<Salt:256>>);
make_id(<<Worker:520>>, Salt) ->
    W = trees2:compress_pub(<<Worker:520>>),
    make_id(W, Salt);
make_id(<<Worker:264>>, <<Salt:256>>) ->
    B = <<Worker:264, Salt:256>>,
    hash:doit(B).
dict_get(ID, Dict, _) ->
    dict_get(ID, Dict).
dict_get(ID, Dict) ->
    case csc:read({jobs, ID}, Dict) of
        error -> error;
        {empty, _, _} -> empty;
        {ok, jobs, Val} -> Val
    end.

dict_write(Job, Dict) ->
    dict_write(Job, 0, Dict).
dict_write(Job, Meta, Dict) ->
    ID = Job#job.id,
    csc:update({jobs, ID}, Job, Dict).

rat_exponent(N, D, 1) ->
    {N, D};
rat_exponent(N, D, E) when (E rem 2 == 0) ->
    N2 = N*N,
    D2 = D*D,
    {N3, D3} = maybe_simplify(N2, D2),
    rat_exponent(N3, D3, E div 2);
rat_exponent(N, D, E) ->
    {N2, D2} = rat_exponent(N, D, E-1),
    N3 = N * N2,
    D3 = D * D2,
    maybe_simplify(N3, D3).

maybe_simplify(N, D) ->
    N32 = 4294967296,
    GCF = gcf(N, D),
    N2 = N div GCF,
    D2 = D div GCF,
    if
        D2 > N32 ->
            F = D2 div N32,
            {N2 div F, D2 div F};
        true -> {N2, D2}
    end.

gcf(A, 0) -> A;
gcf(A, B) when B > A-> 
    gcf(B, A);
gcf(A, B) -> gcf(B, A rem B).

apply_proportion({N, D}, V) ->
    N*V div D.

update(ID, Dict, NewHeight) ->
    % applies the changes that accumulated since last time we updated this job's state. 
    Job = jobs:dict_get(ID, Dict),
    #job{worker = Worker, id = ID, value = Value, salary = Salary, 
         balance = Balance, time = OldHeight} = Job,
    {Value2, Balance2, Payment} = 
        jobs:salary_update(Value, Salary, Balance, OldHeight, NewHeight),
    Job2 = Job#job{value = Value2, balance = Balance2, time = NewHeight},

    BPayment = Payment * 625 div 10000,
    WPayment = Payment - BPayment,

    BurnPub = constants:decompressed_burn(),
    Wacc = accounts:dict_update(Worker, Dict, WPayment, none),
%    Bacc = accounts:dict_update(BurnPub, Dict, BPayment, none),
    Bacc = case accounts:dict_get(BurnPub, Dict) of
               BA = #acc{balance = B} ->
                   BA#acc{balance= B + BPayment};
               _ ->
                   accounts:new(BurnPub, BPayment)
           end,

    %io:fwrite({BPayment, WPayment}),

    Dict2 = jobs:dict_write(Job2, Dict),
    Dict3 = accounts:dict_write(Wacc, Dict2),
    accounts:dict_write(Bacc, Dict3).
   

salary_per_block(Value, Salary) ->
    Value * Salary div n64().
   
salary_update(Value, 
              Salary0, %this is the portion of value that you are paid per block.
              Balance, OldHeight, BlockHeight) ->
    %returns {new_value, new_balance, payment}
    EC = curve(BlockHeight),
    %N64 = 18446744073709551616,
    Blocks = BlockHeight - OldHeight,
    %Salary2 = Value * Salary0 div N64,
    Salary2 = salary_per_block(Value, Salary0),
    %salary2 is how much you are paid per block.
    MaxSalary = Salary2 * Blocks,
    MinReserve = Salary2 * EC,
    %MinReserve = Value * 1000 * Salary0 div N64,
    if
        Balance >= (MaxSalary + MinReserve) ->
            %we are in the normal state. earn salary for this period of time.
            {Value, Balance - MaxSalary, MaxSalary};
        (Balance =< MinReserve) ->
            %we are in the auction state. the price is falling exponentially.
            R = rat_exponent(EC - 1, EC, Blocks),
            Bal2 = apply_proportion(R, Balance),
            Payment = Balance - Bal2,
            
            {apply_proportion(R, Value),
             Bal2,
             Payment};
        true ->
            %we are in a mixed state. we should finish off the money in the normal state, and then switch to auction when we run out.
            S1 = Balance - MinReserve,
            Blocks1 = (S1 div Salary2) + 1,
            Blocks2 = Blocks - Blocks1,
            R = rat_exponent(EC - 1, EC, Blocks2),
            Bal2 = apply_proportion(R, MinReserve),
            Payment = S1 + (MinReserve - Bal2),
            {apply_proportion(R, Value),
             Bal2,
             Payment}
    end.

min_balance_ratio_check(NewPrice, NewBalance, Salary, NewHeight) ->
    %you are required to leave your balance high enough to pay 1000 blocks at the current salary level.
    SalaryPerBlock = jobs:salary_per_block(NewPrice, Salary),
    (NewBalance > (jobs:curve(NewHeight) * SalaryPerBlock)).
    
    %returns {Value, Balance, Time, Payment}


%tax_rate balance price constraint: We must always have enough balance left over to keep paying the current tax for a week.
% -> balance >= tax_rate_per_block * price * 1000.
%If this inequality stops being true, then the price gets reset to:
%price = balance / (tax_rate_per_block * 1000)
%This acts as an auction, value and balance are falling at a rate of 0.1% per block.





    
