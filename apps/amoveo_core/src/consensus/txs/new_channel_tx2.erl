-module(new_channel_tx2).
-export([go/4, make_offer/8, make_dict/4,
         spk/2, cid/1,
	 acc1/1, acc2/1, bal1/1, bal2/1, delay/1]).
-record(nc_offer, {acc1, nonce, nlocktime, bal1, bal2, miner_commission, %miner commission between 0 and 10 000.
              delay, id, contract_hash}).%this is the anyone can spend trade offer.
-record(nc_accept, {acc2, fee, nc_offer, contract_sig}).%this is the tx.
-include("../../records.hrl").
-record(signed, {data="", sig="", sig2=""}).

acc1(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.acc1.
acc2(X) -> X#nc_accept.acc2.
bal1(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.bal1.
bal2(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.bal2.
delay(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.delay.
cid(Tx) -> Tx#nc_accept.nc_offer#signed.data#nc_offer.id.
spk(Tx, Delay) -> 
    spk:new(acc1(Tx), acc2(Tx), cid(Tx),
            [], 0, 0, 0, Delay).
make_dict(Pub, NCOffer, Fee, SPK) ->
    NCO = testnet_sign:data(NCOffer),
    CH = NCO#nc_offer.contract_hash,
    CH = hash:doit(sign:serialize(SPK)),
    %CS = crypto:sign(ecdsa, none, CH, [Priv, crypto:ec_curve(secp256k1)]),
    CS = keys:sign(SPK),
    Sig = if
              (element(3, CS) == []) -> element(4, CS);
              true -> element(3, CS)
          end,
    #nc_accept{acc2 = Pub, nc_offer = NCOffer, fee = Fee, contract_sig = Sig}.
make_offer(ID, Pub, TimeLimit, Bal1, Bal2, Delay, MC, SPK) ->
    A = trees:get(accounts, Pub),
    Nonce = A#acc.nonce,
    <<_:256>> = ID,
    CH = hash:doit(sign:serialize(SPK)),
    true = MC > 0,
    true = MC < 100000,
    #nc_offer{id = ID, nonce = Nonce, acc1 = Pub, nlocktime = 0, bal1 = Bal1, bal2 = Bal2, delay = Delay, contract_hash = CH, miner_commission = MC}.
				 
go(Tx, Dict, NewHeight, _) -> 
    1=2,
    Fee = Tx#nc_accept.fee,
    NCO = Tx#nc_accept.nc_offer#signed.data,
    DefaultFee = governance:dict_get_value(nc, Dict),
    ToAcc1 = ((Fee) - (DefaultFee)) * (10000 / NCO#nc_offer.miner_commission), %this is how we can incentivize limit-order like behaviour.
    CS = Tx#nc_accept.contract_sig,
    CH = NCO#nc_offer.contract_hash,
    Aid1 = acc1(Tx),
    io:fwrite(packer:pack([CH, CS, Aid1])),
    io:fwrite("\n"),
    true = crypto:verify(ecdsa, sha256, CH, CS, [Aid1, crypto:ec_curve(secp256k1)]),
    true = testnet_sign:verify(Tx#nc_accept.nc_offer),
    ID = cid(Tx),
    empty = channels:dict_get(ID, Dict),
    Aid2 = acc2(Tx),
    false = Aid1 == Aid2,
    Bal1 = bal2(Tx),
    true = Bal1 >= 0,
    Bal2 = bal2(Tx),
    true = Bal2 >= 0,
    Delay = delay(Tx),
    NewChannel = channels:new(ID, Aid1, Aid2, Bal1, Bal2, NewHeight, Delay),
    Dict2 = channels:dict_write(NewChannel, Dict),
    Acc1 = accounts:dict_update(Aid1, Dict, -Bal1+ToAcc1, NCO#nc_offer.nonce),
    Acc2 = accounts:dict_update(Aid2, Dict, -Bal2-Fee-ToAcc1, none),
    Dict3 = accounts:dict_write(Acc1, Dict2),
    accounts:dict_write(Acc2, Dict3).
