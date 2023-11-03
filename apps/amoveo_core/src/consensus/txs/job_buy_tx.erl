-module(job_buy_tx).
-export([go/4, make_dict/4, id/1, pub/1]).
-include("../../records.hrl").
-record(job_buy_tx, {pub, nonce, fee, id, balance}).
%* job_buy - {job_id, balance} (signed by who will be the new boss) old balance goes to old owner, balance is the new balance. doubles the price.

id(#job_buy_tx{id = ID}) -> ID.
pub(#job_buy_tx{pub = Pub}) -> Pub.

make_dict(Pub, ID, Balance, Fee) ->
    true = is_binary(Pub),
    true = is_binary(ID),
    65 = size(Pub),
    32 = size(ID),
    true = is_integer(Balance),
    true = Balance > -1,
    Account = trees:get(accounts, Pub),
    #job_buy_tx{id = ID,
                nonce = Account#acc.nonce+1,
                pub = Pub,
                balance = Balance,
                fee = Fee}.
go(Tx, Dict, NewHeight, NonceCheck) ->
    true = NewHeight > (forks:get(53)),
    #job_buy_tx{pub = NewBoss, nonce = Nonce0, fee = Fee, id = ID, 
                balance = NewBalance} = Tx,
    true = is_integer(NewBalance),
    true = NewBalance > -1,
    Nonce = nonce_check:doit(
              NonceCheck, 
              Nonce0),
  
    Dict2 = jobs:update(ID, Dict, NewHeight),
    Job = jobs:dict_get(ID, Dict2),
    #job{balance = OldBalance, value = Value, salary = Salary, 
         boss = OldBoss} = Job,
    NewPrice = Value * 2,
    true = jobs:min_balance_ratio_check(
             NewPrice, NewBalance, Salary, NewHeight),
    
    Nacc = accounts:dict_update(NewBoss, Dict2, -Fee-NewBalance-Value, 
                                Nonce),
    Dict3 = accounts:dict_write(Nacc, Dict2),
    
    Oacc = accounts:dict_update(OldBoss, Dict3, OldBalance+Value, none),
    Dict4 = accounts:dict_write(Oacc, Dict3),
    
    Job2 = Job#job{boss = NewBoss, balance = NewBalance, 
                   value = NewPrice},
    jobs:dict_write(Job2, Dict4).
    
    
