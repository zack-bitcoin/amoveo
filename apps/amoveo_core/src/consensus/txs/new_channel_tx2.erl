-module(new_channel_tx2).
-export([go/4, make/8, make_dict/7, spk/2, cid/1,
	 acc1/1, acc2/1, bal1/1, bal2/1, delay/1]).
-record(nc_offer, {acc1, nonce, nlocktime, bal1, bal2, miner_commission, %miner commission between 0 and 10 000.
              delay, id, contract_hash}).%this is the anyone can spend trade offer.
-record(nc_accept, {acc2, fee, nc_offer, contract_sig}).%this is the tx.
-include("../../records.hrl").

acc1(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.acc1.
acc2(X) -> X#nc_accept.acc2.
bal1(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.bal1.
bal2(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.bal2.
delay(X) -> X#nc_accept.nc_offer#sgned.data#nc_offer.delay.
cid(Tx) -> Tx#nc_accept.nc_offer#signed.data#nc_offer.id.
spk(Tx, Delay) -> 
    spk:new(acc1(Tx), acc2(Tx), cid(Tx),
            [], 0, 0, 0, Delay).
make_dict(Pub, NCOffer, Fee) ->
    CH = NCOffer#nc_offer.contract_hash,
    CS = ok,
    #nc_accept{acc2 = Pub, nc_offer = NCOffer, fee = Fee, contract_sig = CS}.
make_offer(ID, Pub, TimeLimit, Bal1, Bal2, Delay, SPK) ->
    A = trees:get(accounts, Acc1),
    Nonce = A#acc.nonce,
    <<_:256>> = ID,
    CH = <<0:256>>,
    #nc_offer{id = ID, acc1 = Pub, nlocktime = 0, bal1 = Bal1, bal2 = Bal2, delay = Delay, contract_hash = CH}.
				 
go(Tx, Dict, NewHeight, _) -> 
    Fee = Tx#nc_accept.fee,
    ToAcc1 = ((Fee) - (DefaultFee)) * (10000 / Tx#nc_accept.nc_offer#nc_offer.miner_commission), %this is how we can incentivize limit-order like behaviour.
    CS = Tx#nc_accept.contract_sig,
%verify that CS is a valid signature of contract_hash
%verify that nc_offer is signed.
    1=2,
    ID = cid(Tx),

    empty = channels:dict_get(ID, Dict),
    Aid1 = acc1(Tx),
    Aid2 = acc2(Tx),
    false = Aid1 == Aid2,
    Bal1 = bal2(Tx),
    true = Bal1 >= 0,
    Bal2 = bal2(Tx),
    true = Bal2 >= 0,
    Delay = delay(Tx),
    NewChannel = channels:new(ID, Aid1, Aid2, Bal1, Bal2, NewHeight, Delay),
    Dict2 = channels:dict_write(NewChannel, Dict),
    Acc1 = accounts:dict_update(Aid1, Dict, -Bal1+ToAcc1, Tx#nc_accept.nc_offer#signed.data#nc_offer.nonce),
    Acc2 = accounts:dict_update(Aid2, Dict, -Bal2-Fee-ToAcc1, none),
    Dict3 = accounts:dict_write(Acc1, Dict2),
    accounts:dict_write(Acc2, Dict3).
