-module(job_receive_salary_tx).
-export([go/4, make_dict/2, worker/1, id/1]).
-include("../../records.hrl").
-record(job_receive_salary_tx, {worker, nonce, fee, id}).

worker(X) -> X#job_receive_salary_tx.worker.
id(X) -> X#job_receive_salary_tx.id.


make_dict(ID, Fee) ->
    Job = trees:get(jobs, ID),
    #job{worker = Worker} = Job,
    Account = trees:get(accounts, Worker),
    Nonce = Account#acc.nonce+1,
    #job_receive_salary_tx{id = ID, fee = Fee, nonce = Nonce, worker = Worker}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    true = NewHeight > (forks:get(53)),
    #job_receive_salary_tx{
    id = ID, worker = Worker, 
    fee = Fee, nonce = Nonce0} = Tx,

    Job = jobs:dict_get(ID, Dict),
    #job{worker = Worker, id = ID} = Job,
    
    Dict2 = jobs:update(ID, Dict, NewHeight),
    Nonce = nonce_check:doit(NonceCheck, Nonce0),
    Wacc = accounts:dict_update(Worker, Dict2, -Fee, Nonce),
    accounts:dict_write(Wacc, Dict2).
    


%    Job = jobs:dict_get(ID, Dict),
%    #job{worker = Worker, id = ID, value = Value0, salary = Salary, 
%         balance = Balance0, time = Height0} = Job,
%
%    {Value, Balance, Payment} = 
%        jobs:salary_update(Value0, Salary, Balance0, Height0, NewHeight),
%    Job2 = Job#job{value = Value, balance = Balance, time = NewHeight},
%    Dict2 = jobs:dict_write(Job2, Dict),
%    Nonce = nonce_check:doit(NonceCheck, Nonce0),
%    Wacc = accounts:dict_update(Worker, Dict2, Payment - Fee, Nonce),

%    accounts:dict_write(Wacc, Dict2).
