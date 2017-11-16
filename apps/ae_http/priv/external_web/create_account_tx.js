function create_account() {
    var create_address = document.getElementById("create_address");
    var to = create_address.value;
    var create_amount = document.getElementById("create_amount");
    var amount = parseFloat(create_amount.value, 10) * 100000000;
    var from = pubkey_64();
    var fee = 20;
    console.log([amount, fee, from, to]);
    variable_public_get(["create_account_tx", amount, fee, from, to],
                        create_tokens2);
    function create_tokens2(tx) {
        console.log("create account tx is ");
        console.log(tx);
        var to = create_address.value;
        var amount = parseFloat(create_amount.value, 10) * 100000000;
        var from = pubkey_64();
        var fee = 20;
        var from0 = tx[1];
        var fee0 = tx[3];
        var to0 = tx[4];
        var amount0 = tx[5];
        if (!(amount == amount0)) {
            console.log("abort: server changed the amount.");
        } else if (!(to == to0)) {
            console.log("abort: server changed who we are sending money to.");
        } else if (!(fee == fee0)) {
            console.log("abort: server changed the fee.");
        } else {
            var stx = sign_tx(tx);
            variable_public_get(["txs", [-6, stx]], function(x) {});
        }
    }
}
