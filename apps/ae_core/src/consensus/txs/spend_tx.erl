-module(spend_tx).
-export([go/3, make/5, make_dict/6, from/1, to/1]).
-record(spend, {from = 0, nonce = 0, fee = 0, to = 0, amount = 0, version = 0}).
-include("../../records.hrl").
from(X) -> X#spend.from.
to(X) -> X#spend.to. 
make_dict(To, Amount, Fee, From, Trees, Dict) ->
    Accounts = trees:accounts(Trees),
    Acc = case accounts:dict_get(From, Dict) of
		       empty ->  {_, A, P} = accounts:get(From, Accounts),
				 A;
		       X -> X
		   end,
    #spend{from = From, nonce = Acc#acc.nonce + 1, to = To, amount = Amount, fee = Fee}.
	    
make(To, Amount, Fee, From, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, Proof} = accounts:get(From, Accounts),
    {_, _Acc2, Proof2} = accounts:get(To, Accounts),
    Tx = #spend{from = From, nonce = Acc#acc.nonce + 1, to = To, amount = Amount, fee = Fee},
    {Tx, [Proof, Proof2]}.
go(Tx, Dict, NewHeight) ->
    case Tx#spend.version of
        0 -> ok;
        N -> N = version:doit(NewHeight)
    end,
    From = Tx#spend.from,
    To = Tx#spend.to,
    false = From == To,
    A = Tx#spend.amount,
    Facc = accounts:dict_update(From, Dict, -A-Tx#spend.fee, Tx#spend.nonce),
    Tacc = accounts:dict_update(To, Dict, A, none),
    Dict2 = accounts:dict_write(Facc, Dict),
    accounts:dict_write(Tacc, Dict2).
