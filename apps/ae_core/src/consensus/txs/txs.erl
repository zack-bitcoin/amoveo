-module(txs).
-export([digest_from_dict/3]).
digest_from_dict([C|T], Dict, H) ->
    case element(1, C) of
        coinbase ->
            NewDict = coinbase_tx:go(C, Dict, H),
            digest_from_dict3(T, NewDict, H);
        signed -> digest_from_dict3([C|T], Dict, H)
    end.
digest_from_dict3([], Dict, _) -> Dict;
digest_from_dict3([STx|T], Dict, Height) ->
    true = testnet_sign:verify(STx),
    Tx = testnet_sign:data(STx),
    NewDict = digest_from_dict2(Tx, Dict, Height),
    digest_from_dict3(T, NewDict, Height).
digest_from_dict2(Tx, Dict, H) ->
    case element(1, Tx) of
        create_acc_tx -> create_account_tx:go(Tx, Dict, H);
        spend -> spend_tx:go(Tx, Dict, H);
        delete_acc_tx -> delete_account_tx:go(Tx, Dict, H);
        nc -> new_channel_tx:go(Tx, Dict, H);
        gc -> grow_channel_tx:go(Tx, Dict, H);
        ctc -> channel_team_close_tx:go(Tx, Dict, H);
        csc -> channel_solo_close:go(Tx, Dict, H);
        timeout -> channel_timeout_tx:go(Tx, Dict, H);
        cs -> channel_slash_tx:go(Tx, Dict, H);
        ex -> existence_tx:go(Tx, Dict, H);
        oracle_new -> oracle_new_tx:go(Tx, Dict, H);
        oracle_bet -> oracle_bet_tx:go(Tx, Dict, H);
        oracle_close -> oracle_close_tx:go(Tx, Dict, H);
        unmatched -> oracle_unmatched_tx:go(Tx, Dict,H);
        oracle_winnings -> oracle_winnings_tx:go(Tx,Dict,H);
	coinbase_old -> coinbase_tx:go(Tx, Dict, H);
        X -> X = 2
    end.
