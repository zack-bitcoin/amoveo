-module(job_create_tx).
-export([go/4, make_dict/4]).

-include("../../records.hrl").

-record(job_create_tx, {worker, nonce, salary, balance, salt, fee}).


make_dict(Worker, Salary, Balance, Fee) ->
    33 = size(Worker),
    true = is_integer(Salary),
    true = is_integer(Balance),
    true = Balance > -1,
    true = Salary > -1,
    Salt = crypto:strong_random_bytes(32),
    Account = trees:get(accounts, Worker),
    #job_create_tx{worker = Worker, 
                   nonce = Account#acc.nonce+1,
                   salary = Salary,
                   balance = Balance, 
                   salt = Salt,
                   fee = Fee}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    #job_create_tx{
    worker = Worker, salary = Salary, balance = Balance, salt = Salt,
    fee = Fee} = Tx,
    true = is_integer(Salary),
    true = is_integer(Balance),
    true = (Salary > -1),
    true = (Balance > -1),
    Nonce = nonce_check:doit(
              NonceCheck, 
              Tx#spend.nonce),
    Wacc = accounts:dict_update(
             Worker, Dict, -Fee-Balance, Nonce),
    Dict2 = accounts:dict_write(Wacc, Dict),
    ID = jobs:make_id(Worker, Salt),
    empty = jobs:dict_get(ID, Dict2),
    Job = #job{id = ID},
    jobs:dict_write(Job, Dict2).
    
    
