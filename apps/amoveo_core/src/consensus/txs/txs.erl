-module(txs).
-export([digest/3, developer_lock/3, key2module/1, txid/1]).
txid(T) ->
    T2 = case element(1, T) of
             signed -> testnet_sign:data(T);
             _ -> T
         end,
    hash:doit(T2).
digest([C|T], Dict, H) ->
    case element(1, C) of
        coinbase ->
            NewDict = coinbase_tx:go(C, Dict, H),
            digest_txs(T, NewDict, H);
        signed -> digest_txs([C|T], Dict, H)
    end.
digest_txs([], Dict, _) -> Dict;
digest_txs([STx|T], Dict, Height) ->
    true = testnet_sign:verify(STx),
    Tx = testnet_sign:data(STx),
    Fee = element(4, Tx),
    true = Fee > 0,
    Key = element(1, Tx),
    M = key2module(Key),
    NewDict = M:go(Tx, Dict, Height, true),
    digest_txs(T, NewDict, Height).
key2module(sortition_new_tx) -> sortition_new_tx;
key2module(sortition_claim_tx) -> sortition_claim_tx;
key2module(sortition_evidence_tx) -> sortition_evidence_tx;
key2module(sortition_timeout_tx) -> sortition_timeout_tx;
key2module(rng_result_tx) -> rng_result_tx;
key2module(rng_challenge_tx) -> rng_challenge_tx;
key2module(rng_response_tx) -> rng_response_tx;
key2module(rng_refute_tx) -> rng_refute_tx;
key2module(multi_tx) -> multi_tx;
key2module(create_acc_tx) -> create_account_tx;
key2module(spend) -> spend_tx;
key2module(delete_acc_tx) -> delete_account_tx;
key2module(nc) -> new_channel_tx;
key2module(nc_accept) -> new_channel_tx2;
key2module(ctc) -> channel_team_close_tx;
key2module(ctc2) -> channel_team_close_tx2;
key2module(csc) -> channel_solo_close;
key2module(timeout) -> channel_timeout_tx;
key2module(cs) -> channel_slash_tx;
key2module(ex) -> existence_tx;
key2module(oracle_new) -> oracle_new_tx;
key2module(oracle_bet) -> oracle_bet_tx;
key2module(oracle_close) -> oracle_close_tx;
key2module(unmatched) -> oracle_unmatched_tx;
key2module(oracle_winnings) -> oracle_winnings_tx;
key2module(coinbase_old) -> coinbase_tx.
developer_lock(From, NewHeight, Dict) -> ok.
    
