
    function channel_feeder_simplify_helper(from, ss) {
        var cd = channel_manager[from];
        var spk = cd.me;
        var unlocked = spk_bet_unlock(spk, ss);
        var ret = sign_tx(unlocked.spk);
        return {ss: unlocked.ssremaining, spk: ret};
    }
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
    function spk_bet_unlock(spk, ss) {
        var remaining = JSON.parse(JSON.stringify(bets));
        var amount_change = 0;
        var ssremaining = JSON.parse(JSON.stringify(ss));
        var secrets = [];
        var dnonce = 0;
        var bets = spk[4];
        var key;
        var ssthem;
        var f;
        for (var i = ss.length - 1, i > -1, i--) {
            key = bet[i].key;
            ssthem = ss[i];
            console.log("ssthem is ");
            console.log(JSON.stringify(ssthem));
            throw("working here");
            //look up fun limit and var limit and gas limit from config file.
            //verify none of in ssthem
            f = spk_prove_facts(
        }
        spk.bets = remaining;
        spk.amount += amount_change;
        spk.nonce += dnonce;
        return {"ssremaining": ssremaining,
                "spk": spk, //make sure to change spk in a few ways;
                "secrets": secrets,
                "ssthem": ssthem};
    }
    function teach_secrets(secrets, ip, port) {
        for (var i = 0; i < secrets.length; i++) {
            var msg = ["learn_secret", my_pubkey, secrets[i].secret, secrets[i].code];
        }
        return "ok";
    }
