
    function api_bet_unlock(ip, port) {
        //get server id int server_id;
        var unlocked_object = channel_feeder_bets_unlock(server_id);
        teach_secrets(unlocked_object.secrets, ip, port);
        var msg = ["spk", my_pubkey];
        //use msg to get themspk
        channel_feeder_update_to_me(them_spk, server_id);
        return "ok";
    }
    function channel_feeder_bets_unlock(server_id) {
        var cd = channel_manager[server_id];
        if (!(true == cd.live)) {
            console.log("this channel has been closed");
            throw("this channel was closed");
        }
        var spk_me = cd.me;
        var ss_old = cd.ssme;
        var unlock_object = spk_bet_unlock(spk_me, ss_old);
        cd.me = unlock_object.spk;
        cd.ssme = unlock_object.newss;
        cd.ssthem = unlock_object.ssthem;
        channel_manager[server_id] = cd;
        return {"secrets":unlock_object.secrets,
                "spk":unlock_object.spk};
    }
    function spk_bet_unlock2(bets, ss) {
        var a = 0;
        var bets_out = [];
        var ss_out = [];
        var nonce = 0;
        var ssthem = [];
        var secrets = [];
        for (var i = 0; i < bets.length; i++) {
        }
    }
    function spk_bet_unlock(spk, ss) {
        var unlocked = spk_bet_unlock2(spk.bets, ss);
        var newspk = JSON.parse(JSON.stringify(spk));
        newspk.bets = unlocked.remaining;
        newspk.amount += unlocked.amount_change;
        newspk.nonce += unlocked.d_nonce;
        return {"newss": unlocked.ss_remaining,
                "spk": newspk;
                "secrets": unlocked.secrets,
                "ssthem":unlocked.ssthem}
    }
    function teach_secrets(secrets, ip, port) {
        for (var i = 0; i < secrets.length; i++) {
            var msg = ["learn_secret", my_pubkey, secrets[i].secret, secrets[i].code];
        }
        return "ok";
    }
