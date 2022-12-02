-module(new_channel_tx2).
-export([go/4, make_offer/8, make_dict/4,
         cid/1,
	 acc1/1, acc2/1, bal1/1, bal2/1, delay/1]).
-include("../../records.hrl").

acc1(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.acc1.
acc2(X) -> X#nc_accept.acc2.
bal1(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.bal1.
bal2(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.bal2.
delay(X) -> X#nc_accept.nc_offer#signed.data#nc_offer.delay.
cid(Tx) -> Tx#nc_accept.nc_offer#signed.data#nc_offer.id.
make_dict(Pub, NCOffer, Fee, SPK) ->
    NCO = signing:data(NCOffer),
    CH = NCO#nc_offer.contract_hash,
    CS = spk:sign(SPK, 2),%should be signed by acc2
    
    CH = spk:hash(SPK),

    Sig = if
              (element(3, CS) == []) -> 
                  element(2, element(4, CS));
              true -> 
                  element(2, element(3, CS))
          end,
    B1 = signing:verify_sig(CH, Sig, keys:pubkey()),

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
    F44 = forks:get(44),
    true = F44 > NewHeight,

    true = NewHeight > forks:get(11),
    #nc_accept{
                fee = Fee,
                nc_offer = SNCO,
                contract_sig = CS
              } = Tx,
    ID0 = cid(Tx),
    Aid1 = acc1(Tx),
    Aid2 = acc2(Tx),
    %Fee = Tx#nc_accept.fee,
    %NCO = SNCO#signed.data,
    #nc_offer{
               nlocktime = NLock,
               miner_commission = MC,
               contract_hash = CH,
               nonce = Nonce
             } = SNCO#signed.data,
    F29 = forks:get(29),
    ID = if
             (NewHeight > F29) ->
                 new_channel_tx:salted_id(ID0, Aid1);
             true -> ID0
         end,
    %NCO = Tx#nc_accept.nc_offer#signed.data,
    %NLock = NCO#nc_offer.nlocktime,
    true = ((NLock == 0) or (NewHeight < NLock)),
    DefaultFee = governance:dict_get_value(nc, Dict, NewHeight),
    %ToAcc1 = ((Fee) - (DefaultFee)) * (10000 div NCO#nc_offer.miner_commission), %this is how we can incentivize limit-order like behaviour.
    ToAcc1 = ((Fee) - (DefaultFee)) * (10000 div MC), %this is how we can incentivize limit-order like behaviour.
    %CS = Tx#nc_accept.contract_sig,
    %CH = NCO#nc_offer.contract_hash,
    true = signing:verify_sig(CH, CS, Aid2),
    %true = signing:verify(CS2),
    true = signing:verify(SNCO),
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
                 true -> Nonce
             end,
    Acc1 = accounts:dict_update(Aid1, Dict, -Bal1+ToAcc1, Nonce1),
    Acc2 = accounts:dict_update(Aid2, Dict, -Bal2-Fee-ToAcc1, none),

    F26 = forks:get(26),
    if
        (NewHeight > F26) ->
            %we want Acc1 to be able to cancel his channel offer by making some other unrelated tx to increase his nonce.
            true = Acc1#acc.nonce == (Nonce-1);
        true -> ok
    end,

    Dict3 = accounts:dict_write(Acc1, Dict2),
    nc_sigs:store(ID, CS),
    accounts:dict_write(Acc2, Dict3).
