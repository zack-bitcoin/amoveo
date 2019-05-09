-module(new_channel_tx2).
-export([go/4, make_offer/8, make_dict/4,
         spk/2, cid/1,
	 acc1/1, acc2/1, bal1/1, bal2/1, delay/1]).
-record(nc_offer, {acc1, nonce, nlocktime, bal1, bal2, miner_commission, %miner commission between 0 and 10 000.
              delay, id, contract_hash}).%this is the anyone can spend trade offer.
-record(nc_accept, 
        {acc2, nc_offer, fee,
         contract_sig}).%this is the tx.
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
    CS = spk:sign(SPK, 2),%should be signed by acc2
    
    CH = spk:hash(SPK),

    Sig = if
              (element(3, CS) == []) -> 
                  element(2, element(4, CS));
              true -> 
                  element(2, element(3, CS))
          end,
    B1 = testnet_sign:verify_sig(CH, Sig, keys:pubkey()),

    %CS2 = setelement(3, setelement(2, CS, CH), Sig),
    %true = spk:verify_sig(CS),
    #nc_accept{acc2 = Pub, nc_offer = NCOffer, fee = Fee, contract_sig = Sig}.
make_offer(ID, Pub, TimeLimit, Bal1, Bal2, Delay, MC, SPK) ->
    A = trees:get(accounts, Pub),
    Nonce = A#acc.nonce + 1,
    <<_:256>> = ID,
    CH = spk:hash(SPK),
    true = MC > 0,
    true = MC < 100001,
    #nc_offer{id = ID, nonce = Nonce, acc1 = Pub, nlocktime = 0, bal1 = Bal1, bal2 = Bal2, delay = Delay, contract_hash = CH, miner_commission = MC}.
				 
go(Tx, Dict, NewHeight, _) -> 
    true = NewHeight > forks:get(11),
    Fee = Tx#nc_accept.fee,
    NCO = Tx#nc_accept.nc_offer#signed.data,
    NLock = NCO#nc_offer.nlocktime,
    true = ((NLock == 0) or (NewHeight < NLock)),
    DefaultFee = governance:dict_get_value(nc, Dict),
    ToAcc1 = ((Fee) - (DefaultFee)) * (10000 div NCO#nc_offer.miner_commission), %this is how we can incentivize limit-order like behaviour.
    CS = Tx#nc_accept.contract_sig,
    CH = NCO#nc_offer.contract_hash,
    Aid1 = acc1(Tx),
    Aid2 = acc2(Tx),
    true = testnet_sign:verify_sig(CH, CS, Aid2),
    %true = testnet_sign:verify(CS2),
    true = testnet_sign:verify(Tx#nc_accept.nc_offer),
    ID = cid(Tx),
    empty = channels:dict_get(ID, Dict),
    false = Aid1 == Aid2,
    %Bal1 = bal2(Tx),%BAD
    F13 = forks:get(13),
    Bal1 = if
               NewHeight > F13 -> bal1(Tx);
               true -> bal2(Tx)
           end,
    true = Bal1 >= 0,
    Bal2 = bal2(Tx),
    true = Bal2 >= 0,
    Delay = delay(Tx),
    NewChannel = channels:new(ID, Aid1, Aid2, Bal1, Bal2, NewHeight, Delay),
    Dict2 = channels:dict_write(NewChannel, Dict),
    F17 = (NewHeight > forks:get(17)),
    %F17 = false,
    %Bool1 = ((NLock - NewHeight) < NCO#nc_offer.delay),
    %Bool2 = NLock > 0,
    %Bool3 = (NCO#nc_offer.nonce == 0),
    Nonce1 = if
                 F17 -> none;
                 true -> NCO#nc_offer.nonce
             end,
    Acc1 = accounts:dict_update(Aid1, Dict, -Bal1+ToAcc1, Nonce1),
    Acc2 = accounts:dict_update(Aid2, Dict, -Bal2-Fee-ToAcc1, none),
    Dict3 = accounts:dict_write(Acc1, Dict2),
    nc_sigs:store(ID, Tx#nc_accept.contract_sig),
    accounts:dict_write(Acc2, Dict3).
