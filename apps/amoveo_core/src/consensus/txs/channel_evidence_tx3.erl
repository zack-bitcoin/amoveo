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
                  nonce = ChannelNonce,
                  contract_id = Source,
                  type = SourceType
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
                    %for now just store the hash of the payout vector in the result
                    B1 = RNonce > ChannelNonce,
                    B2 = (length(Accounts) == length(PayoutVector)),
                    TwoE32 = 4294967295,%(2**32 - 1) highest expressible value in chalang integers. payout quantities need to sum to this.
                    B3 = contract_evidence_tx:sum_vector(TwoE32, PayoutVector),
                    B4 = B1 and B2 and B3,
                    if
                        not(B4) ->
                            if
                                not(B1) -> io:fwrite("resolve contract tx, vector case, nonce is too low to update\n");
                                not(B2) -> 
                                    io:fwrite(packer:pack([length(Accounts), PayoutVector])),
                                    io:fwrite("resove_contract_tx, payout vector is the wrong length\n");
                                not(B3) -> 
                                    io:fwrite(packer:pack(PayoutVector)),
                                    io:fwrite("\ncontract_evidence_tx, payout vector doesn't conserve the total quantity of veo.\n")
                            end,
                            Dict2;
                        true ->
                            Channel2 = 
                                Channel#sub_channel{
                                  result = hash:doit(Accounts),
                                  nonce = RNonce,
                                  delay = RDelay,
                                  last_modified = NewHeight
                                 },
                            sub_channel:dict_write(Channel2, Dict2)
                    end;
                [<<CNonce:32>>,<<CDelay:32>>,<<ResultCH:256>>,Matrix|_] ->
                    B1 = CNonce > ChannelNonce,
                    B2 = is_list(Matrix),
                    B3 = (length(Accounts) == length(Matrix)),
                    RMany = length(hd(Matrix)),
                    B4 = contract_evidence_tx:all_lengths(RMany, Matrix),
                    TwoE32 = 4294967295,%(2**32 - 1) highest expressible value in chalang integers.
                    B5 = contract_evidence_tx:column_sum(TwoE32, Matrix),
                    MCF = governance:dict_get_value(max_contract_flavors, Dict),
                    B7 = RMany =< MCF,
                    B6 = B1 and B2 and B3 and B4 and B5 and B7,
                    if
                        not(B6) ->
                            if
                                not(B1) -> io:fwrite("resolve contract tx, nonce is too low to update contract.\n");
                                not(B2) -> io:fwrite("contract_evidence_tx, matrix is misformatted.\n");
                                not(B3) -> io:fwrite("contract_evidence_tx, matrix has wrong number of rows.\n");
                                not(B4) -> io:fwrite("contract_evidence_tx, matrix has a row with the wrong length.\n");
                                not(B5) -> io:fwrite("contract_evidence_tx, matrix does not conserve the total number of veo.\n");
                                not(B7) -> io:fwrite("contract_evidence_tx, matrix rows are too long. we can't have a contract with that many subcurrencies.\n")
                            end,
                            Dict2;
                        true ->
                            RCID = contracts:make_id(<<ResultCH:256>>, RMany,Source,SourceType),
                            {MRoot, M2} = contract_evidence_tx:make_tree(Matrix), 
                            %store the contract hash in the tree as well.
                            CFG = mtree:cfg(M2),
                            Leaf = leaf:new(0, <<ResultCH:256>>,0, CFG),
                            {MRoot2, M3} = mtree:store_batch([Leaf], MRoot, M2),
                            RootHash = mtree:root_hash(MRoot2, M3),
                            Channel2 = 
                                Channel#sub_channel{
                                  result = RootHash,
                                  nonce = CNonce,
                                  delay = CDelay,
                                  last_modified = NewHeight
                                 },
                            sub_channel:dict_write(Channel2, Dict2)
                    end
            end
    end.
    
    %update the channel with the new nonce and result, delay, and newheight
