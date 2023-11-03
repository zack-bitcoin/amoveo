-module(job_team_adjust_tx).
-export([make_dict/5, go/4, id/1]).

-include("../../records.hrl").
-record(job_team_adjust_tx, {boss, worker, fee, nonce, new_salary, new_price, new_balance, id}).

id(#job_team_adjust_tx{id = ID}) -> ID.



make_dict(ID, NewSalary, NewBalance, NewPrice, Fee) ->
    true = is_integer(NewPrice),
    true = is_integer(NewBalance),
    true = is_integer(NewSalary),
    true = NewPrice > -1,
    true = NewBalance > -1,
    true = NewSalary > -1,
    true = is_binary(ID),
    true = 32 == size(ID),
    true = is_integer(Fee),
    true = Fee > -1,
    N64 = jobs:n64(),
    true = (NewSalary < N64),
    
    Job = trees:get(jobs, ID),
    #job{boss = Boss, worker = Worker} = Job,
    Account = trees:get(accounts, Boss),
    Nonce = Account#acc.nonce+1,
    #job_team_adjust_tx{boss = Boss, worker = Worker, fee = Fee, 
                   nonce = Nonce, new_salary = NewSalary,
                   new_price = NewPrice, new_balance = NewBalance,
                   id = ID}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #job_team_adjust_tx{
    boss = Boss, worker = Worker, nonce = Nonce0, fee = Fee, id = ID,
    new_salary = NewSalary, new_price = NewPrice, 
    new_balance = NewBalance} = Tx,
    true = NewHeight > (forks:get(53)),

    true = NewBalance > -1,
    true = NewPrice > -1,
    true = NewSalary > -1,
    N64 = jobs:n64(),
    true = NewSalary < N64,
    true = jobs:min_balance_ratio_check(
             NewPrice, NewBalance, NewSalary, NewHeight),

    Job = jobs:dict_get(ID, Dict),
    #job{boss = Boss, worker = Worker} = Job,
    
    Dict2 = jobs:update(ID, Dict, NewHeight),

    Job2 = jobs:dict_get(ID, Dict2),
    #job{balance = Balance1} = Job2,
    Job3 = Job#job{value = NewPrice, balance = NewBalance, 
                   salary = NewSalary},
    Dict3 = jobs:dict_write(Job3, Dict2),
    
    Nonce = nonce_check:doit(NonceCheck, Nonce0),
    Bacc = accounts:dict_update(
             Boss, Dict3, Balance1 - NewBalance - Fee, Nonce),
    accounts:dict_write(Bacc, Dict3).
    
