function bet_unlock(ip, port) {
    //get server id int server_id;
    var unlocked_object = channel_feeder_bets_unlock(server_id);
    teach_secrets(unlocked_object.secrets, ip, port);
    var msg = ["spk", my_pubkey];
    //use msg to get themspk
    channel_feeder_update_to_me(them_spk, server_id);
    return "ok";
}
function teach_secrets(secrets, ip, port) {
    for (var i = 0; i < secrets.length; i++) {
        var msg = ["learn_secret", my_pubkey, secrets[i].secret, secrets[i].code];
    }
    return "ok";
}
