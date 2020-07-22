-module(channel_evidence_tx3).
-export([go/4, make_dict/5]).

-include("../../records.hrl").
-record(channel_evidence_tx3, {
          from, nonce, fee, accounts, scriptpubkey, evidence, prove
}).

make_dict(From, Fee, ScriptPubkey, Evidence, Prove) ->
    true = is_list(Evidence),
    CID = (testnet_sign:data(ScriptPubkey))#spk.cid,
    <<_:256>> = CID,
    Acc = trees:get(accounts, From),
    Channel = trees:get(sub_channels, CID),
    false = (empty == Channel),
    #channel_evidence_tx3{
              from = From, nonce = Acc#acc.nonce+1, 
              fee = Fee, 
              scriptpubkey = ScriptPubkey, 
              evidence = Evidence,
              prove = Prove}.
go(Tx, Dict, NewHeight, NonceCheck) ->
    #channel_evidence_tx3{
    from = From,
    nonce = Nonce,
    fee = Fee,
    scriptpubkey = SSPK,
    evidence = Evidence,
    prove = Prove
   } = Tx,
    Facc = accounts:dict_update(From, Dict, -Fee, Nonce),
    Dict2 = accounts:dict_write(Facc, Dict),

    SPK = testnet_sign:data(SSPK),
    #spk{
          acc1 = Acc1,
          acc2 = Acc2,
          cid = CID,
          time_gas = TG,
          bets = ByteCode,
          space_gas = SG
        } = SPK,
    TimeGasLimit = governance:dict_get_value(time_gas, Dict2),
    SpaceGasLimit = governance:dict_get_value(space_gas, Dict2),
    true = TG < TimeGasLimit,
    true = SG < SpaceGasLimit,
    Channel = sub_channels:dict_get(CID, Dict2),
    #sub_channel{
                  closed = 0,
                  accounts = AH,
                  nonce = Nonce
                } = Channel,
    Accounts = [Acc1, Acc2],
    AH = hash:doit(Accounts),
    true = spk:verify_sig(SPK, Acc1, Acc2),

    %run the contract
    %check the new channel nonce is higher.
    case contract_evidence_tx:run(NewHeight, Prove, Evidence, ByteCode, Dict2) of
        {error, Error} ->
            io:fwrite("\n in channel evidence tx3 contract has an error\n"),
            Dict2;
        Data ->
            case chalang:stack(Data) of
                [<<RNonce:32>>, <<RDelay:32>>, PayoutVector|_] when (is_list(PayoutVector)) ->
                    %pay out winnings to the owners according to the payout vector in the source currency.
                    ok;
                [<<CNonce:32>>,<<CDelay:32>>,<<ResultCH:256>>,Matrix|_] ->
                    %since it is a matrix we need to create a new contract and give them subcurrencies according to the Matrix.
                    ok
            end,
            ok
    end.
    
    %update the channel with the new nonce and result, delay, and newheight
