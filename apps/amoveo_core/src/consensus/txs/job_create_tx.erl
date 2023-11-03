-module(job_create_tx).
-export([go/4, make_dict/5, worker/1, salt/1, id/1]).
-include("../../records.hrl").
-record(job_create_tx, {worker, nonce, fee, salary, balance, value, id, salt}).

worker(X = #job_create_tx{worker = W}) -> W.
salt(X) -> X#job_create_tx.salt.
id(X) -> X#job_create_tx.id.


make_dict(Worker, Value, Salary, Balance, Fee) ->
%    Worker = trees2:compress_pub(Worker0),
    65 = size(Worker),
    true = is_integer(Salary),
    true = is_integer(Balance),
    true = is_integer(Value),
    true = Balance > -1,
    true = Salary > -1,
    true = Value > -1,
    Salt = crypto:strong_rand_bytes(32),
    Account = trees:get(accounts, Worker),
    ID = jobs:make_id(Worker, Salt),
    #job_create_tx{worker = Worker, 
                   nonce = Account#acc.nonce+1,
                   salary = Salary,
                   balance = Balance, 
                   value = Value,
                   salt = Salt,
                   fee = Fee,
                   id = ID}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    true = NewHeight > (forks:get(53)),
    #job_create_tx{
    worker = Worker, salary = Salary, balance = Balance, salt = Salt,
    fee = Fee, nonce = Nonce0, id = ID, value = Value} = Tx,
    true = is_integer(Salary),
    true = is_integer(Balance),
    true = (Salary > -1),
    N64 = 18446744073709551616,%2^64
    true = (Salary < N64),
    true = (Balance > -1),
    Nonce = nonce_check:doit(
              NonceCheck, 
              Nonce0),
    Wacc = accounts:dict_update(
             Worker, Dict, -Fee-Balance, Nonce),
    Dict2 = accounts:dict_write(Wacc, Dict),
    ID = jobs:make_id(Worker, Salt),
    empty = jobs:dict_get(ID, Dict2),
    Job = #job{id = ID, worker = Worker, boss = Worker, value = Value,
               salary = Salary, balance = Balance, time = NewHeight},
    jobs:dict_write(Job, Dict2).
    
    
