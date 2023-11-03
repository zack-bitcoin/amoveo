-module(job_adjust_tx).
-export([go/4, make_dict/4, id/1]).

-include("../../records.hrl").
-record(job_adjust_tx, {boss, nonce, fee, id, new_price, new_balance}).

id(#job_adjust_tx{id = ID}) -> ID.

make_dict(ID, NewPrice, NewBalance, Fee) ->
    true = is_integer(NewPrice),
    true = is_integer(NewBalance),
    true = is_binary(ID),
    true = (32 == size(ID)),
    Job = trees:get(jobs, ID),
    #job{boss = Boss} = Job,
    Account = trees:get(accounts, Boss),
    Nonce = Account#acc.nonce+1,
    #job_adjust_tx{boss = Boss, nonce = Nonce, fee = Fee, id = ID,
                  new_price = NewPrice, new_balance = NewBalance}.

go(Tx, Dict, NewHeight, NonceCheck) ->
    true = NewHeight > (forks:get(53)),
    #job_adjust_tx{boss = Boss, nonce = Nonce0, fee = Fee, id = ID,
                   new_price = NewPrice, new_balance = NewBalance} = Tx,
    true = NewBalance > -1,
    true = NewPrice > -1,
    Job0 = jobs:dict_get(ID, Dict),
    #job{boss = Boss, id = ID, salary = Salary} = Job0,

    true = jobs:min_balance_ratio_check(
             NewPrice, NewBalance, Salary, NewHeight),

    Dict2 = jobs:update(ID, Dict, NewHeight),
    
    Job = jobs:dict_get(ID, Dict2),
    #job{balance = Balance1} = Job,
    Job2 = Job#job{value = NewPrice, balance = NewBalance},
    Dict3 = jobs:dict_write(Job2, Dict2),

    Nonce = nonce_check:doit(NonceCheck, Nonce0),
    Bacc = accounts:dict_update(
             Boss, Dict3, Balance1 - NewBalance - Fee, Nonce),
    accounts:dict_write(Bacc, Dict3).
    
    
