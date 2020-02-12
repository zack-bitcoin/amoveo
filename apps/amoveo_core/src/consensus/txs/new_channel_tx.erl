-module(new_channel_tx).
-export([go/4, make/8, make_dict/7, spk/2, cid/1,
	 acc1/1, acc2/1, bal1/1, bal2/1, delay/1,
         salted_id/2, salted_id/1]).
-include("../../records.hrl").

acc1(X) -> X#nc.acc1.
acc2(X) -> X#nc.acc2.
bal1(X) -> X#nc.bal1.
bal2(X) -> X#nc.bal2.
delay(X) -> X#nc.delay.
cid(Tx) -> Tx#nc.id.
spk(Tx, Delay) -> 
    spk:new(Tx#nc.acc1, Tx#nc.acc2, Tx#nc.id,
            [], 0,0, 0, Delay).
make_dict(ID,Acc1,Acc2,Inc1,Inc2,Delay, Fee) ->
    A = trees:get(accounts, Acc1),
    Nonce = A#acc.nonce,
    <<_:256>> = ID,
    #nc{id = ID, acc1 = Acc1, acc2 = Acc2, 
	fee = Fee, nonce = Nonce+1, bal1 = Inc1,
	bal2 = Inc2, 
	delay = Delay}.
make(ID,Trees,Acc1,Acc2,Inc1,Inc2,Delay, Fee) ->
    <<_:256>> = ID,
    Accounts = trees:accounts(Trees),
    {_, A, Proof} = accounts:get(Acc1, Accounts),
    Nonce = A#acc.nonce,
    {_, _, Proof2} = accounts:get(Acc2, Accounts),
    %true = (Rent == 0) or (Rent == 1),
    Tx = #nc{id = ID, acc1 = Acc1, acc2 = Acc2, 
	     fee = Fee, nonce = Nonce+1, bal1 = Inc1,
	     bal2 = Inc2, 
	     delay = Delay
	     },
    {Tx, [Proof, Proof2]}.

salted_id(Tx) when is_record(Tx, nc) ->
    #nc{
         id = ID,
         acc1 = A,
         acc2 = B
       } = Tx,
    hash:doit(<<ID/binary, A/binary>>);
salted_id(Tx) when is_record(Tx, nc_accept) ->
    ID = new_channel_tx2:cid(Tx),
    A = new_channel_tx2:acc1(Tx),
    %B = new_channel_tx2:acc2(Tx),
    salted_id(ID, A).
salted_id(ID, A) ->
    hash:doit(<<ID/binary, A/binary>>).
				 
go(Tx, Dict, NewHeight, _) ->
    ID0 = Tx#nc.id,
    F29 = forks:get(29),
    Aid1 = Tx#nc.acc1,
    Aid2 = Tx#nc.acc2,
    ID = if
             (NewHeight > F29) -> salted_id(Tx);
             true -> ID0
         end,
    empty = channels:dict_get(ID, Dict),
    %txs:developer_lock(Aid1, NewHeight, Dict),
    %txs:developer_lock(Aid2, NewHeight, Dict),
    false = Aid1 == Aid2,
    Bal1 = Tx#nc.bal1,
    true = Bal1 >= 0,
    Bal2 = Tx#nc.bal2,
    true = Bal2 >= 0,
    Delay = Tx#nc.delay,
    NewChannel = channels:new(ID, Aid1, Aid2, Bal1, Bal2, NewHeight, Delay),
    Dict2 = channels:dict_write(NewChannel, Dict),
    HF = Tx#nc.fee div 2,
    Acc1 = accounts:dict_update(Aid1, Dict, -Bal1-HF, Tx#nc.nonce),
    Acc2 = accounts:dict_update(Aid2, Dict, -Bal2-HF, none),
    Dict3 = accounts:dict_write(Acc1, Dict2),
    accounts:dict_write(Acc2, Dict3).
