-module(new_channel_tx2).
-export([go/4, make/8, make_dict/7, spk/2, cid/1,
	 acc1/1, acc2/1, bal1/1, bal2/1, delay/1]).
-record(nc_offer, {acc1, nonce, nlocktime, bal1, bal2, 
              delay, id, contract_hash}).%this is the anyone can spend trade offer.
-record(nc_accept, {acc2, fee, nc_offer, contract_sig}).%this is the tx.
-include("../../records.hrl").

acc1(X) -> X#nc_accept.nc_offer#nc_offer.acc1.
acc2(X) -> X#nc_accept.acc2.
bal1(X) -> X#nc_accept.nc_offer#nc_offer.bal1.
bal2(X) -> X#nc_accept.nc_offer#nc_offer.bal2.
delay(X) -> X#nc_accept.nc_offer#nc_offer.delay.
cid(Tx) -> Tx#nc_accept.nc_offer#nc_offer.id.
spk(Tx, Delay) -> 
    spk:new(acc1(Tx), acc2(Tx), cid(Tx),
            [], 0, 0, 0, Delay).
make_dict(ID,Acc1,Acc2,Inc1,Inc2,Delay, Fee) ->
    A = trees:get(accounts, Acc1),
    Nonce = A#acc.nonce,
    <<_:256>> = ID,
    ok.
   % #nc{id = ID, acc1 = Acc1, acc2 = Acc2, 
	%fee = Fee, nonce = Nonce+1, bal1 = Inc1,
	%bal2 = Inc2, 
	%delay = Delay}.
make(ID,Trees,Acc1,Acc2,Inc1,Inc2,Delay, Fee) ->
    <<_:256>> = ID,
    Accounts = trees:accounts(Trees),
    {_, A, Proof} = accounts:get(Acc1, Accounts),
    Nonce = A#acc.nonce,
    {_, _, Proof2} = accounts:get(Acc2, Accounts),
    %true = (Rent == 0) or (Rent == 1),
    Tx = 0,
    %Tx = #nc{id = ID, acc1 = Acc1, acc2 = Acc2, 
	%     fee = Fee, nonce = Nonce+1, bal1 = Inc1,
	%     bal2 = Inc2, 
	%     delay = Delay
	%     },
    {Tx, [Proof, Proof2]}.
				 
go(Tx, Dict, NewHeight, _) -> ok.
%    ID = Tx#nc.id,
%    empty = channels:dict_get(ID, Dict),
%    Aid1 = Tx#nc.acc1,
%    Aid2 = Tx#nc.acc2,
%    false = Aid1 == Aid2,
%    Bal1 = Tx#nc.bal1,
%    true = Bal1 >= 0,
%    Bal2 = Tx#nc.bal2,
%    true = Bal2 >= 0,
%    Delay = Tx#nc.delay,
%    NewChannel = channels:new(ID, Aid1, Aid2, Bal1, Bal2, NewHeight, Delay),
%    Dict2 = channels:dict_write(NewChannel, Dict),
%    HF = Tx#nc.fee div 2,
%    Acc1 = accounts:dict_update(Aid1, Dict, -Bal1-HF, Tx#nc.nonce),
%    Acc2 = accounts:dict_update(Aid2, Dict, -Bal2-HF, none),
%    Dict3 = accounts:dict_write(Acc1, Dict2),
%    accounts:dict_write(Acc2, Dict3).
