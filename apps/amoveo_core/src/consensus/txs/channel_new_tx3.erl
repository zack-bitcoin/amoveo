-module(channel_new_tx3).
-export([go/4, make_offer/11, make_dict/2]).
-include("../../records.hrl").
%-record(sub_channel, {
%      id,
%      accounts,%root hash
%      amount,
%      nonce = 1,
%      last_modified,
%      delay,
%      closed = 0,
%      contract_id,
%      type
%}).
-record(nc_offer3, {acc1, nonce, start_limit, end_limit, bal1, bal2, miner_commission, %miner commission between 0 and 10 000.
              cid, contract_hash, source, source_type,
                   fee1, fee2}).%this is the anyone can spend trade offer.
-record(nc_accept3, 
        {acc2, offer, fee,
         contract_sig}).%this is the tx.

make_offer(ID, Pub, Bal1, Bal2, MC, SPK, CH, Fee1, Fee2, StartLimit, EndLimit) ->
    A = trees:get(accounts, Pub),
    Nonce = A#acc.nonce + 1,
    <<_:256>> = ID,
    CH = spk:hash(SPK),
    true = MC > 0,
    true = MC < 100001,
    #nc_offer3{cid = ID, nonce = Nonce, acc1 = Pub, bal1 = Bal1, bal2 = Bal2, start_limit = StartLimit, end_limit = EndLimit, contract_hash = CH, fee1 = Fee1, fee2 = Fee2}.

make_dict(From, SOffer) ->
    Offer = testnet_sign:data(SOffer),
    #nc_offer3{
                fee1 = Fee1,
                fee2 = Fee2} = Offer,
    #nc_accept3{acc2 = From, offer = SOffer,
                     fee = Fee1 + Fee2}.

go(Tx, Dict, NewHeight, _) ->
    #nc_accept3{
    offer = SOffer,
    acc2 = Acc2,
    contract_sig = CS
   } = Tx,
    true = testnet_sign:verify(SOffer),
    Offer = testnet_sign:data(SOffer),
    #nc_offer3{
                acc1 = Acc1,
                fee1 = Fee1,
                fee2 = Fee2,
                start_limit = SL,
                end_limit = EL,
                cid = CID0,
                nonce = Nonce,
                contract_hash = CH,
                bal1 = Bal1,
                bal2 = Bal2,
                source = Source,
                source_type = SourceType
              } = Offer,
    true = testnet_sign:verify_sig(CH, CS, Acc2),
    false = Acc1 == Acc2,
    true = Bal1 >= 0,
    true = Bal2 >= 0,
    true = NewHeight >= SL,
    true = NewHeight =< EL,
    CID = new_channel_tx:salted_id(CID0, Acc1),
    empty = sub_channels:dict_get(CID, Dict),
    A1 = accounts:dict_get(Acc1, Dict),
    Nonce = A1#acc.nonce + 1,
    Fee = Fee1 + Fee2,
    Dict2 = swap_tx:fee_helper(Fee1+Bal1, Acc1, Dict),
    Dict3 = swap_tx:fee_helper(Fee2+Bal2, Acc2, Dict2),

    SC = sub_channels:new(CID, Source, SourceType, [Acc1, Acc2], Bal1+Bal2),
    nc_sigs:store(CID, CS),
    sub_channels:dict_write(SC, Dict3).
