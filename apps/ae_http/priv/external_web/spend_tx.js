function spend_tokens() {
    var to = spend_address.value;
    var amount = parseFloat(spend_amount.value, 10) * 100000000;
    var from = pubkey_64();
    var fee = 20;
    variable_public_get(["spend_tx", amount, fee, from, to],
                        spend_tokens2);

    function spend_tokens2(tx) {
        console.log("spend tx is");
        console.log(tx);
        var amount = parseFloat(spend_amount.value, 10) * 100000000;
        var amount0 = tx[5];
        var to = spend_address.value;
        var to0 = tx[4];
        var fee = 20;
        var fee0 = tx[3];
        if (!(amount == amount0)) {
            console.log("amounts");
            console.log(amount);
            console.log(amount0);
            console.log(tx[2]);
            console.log("abort: server changed the amount.");
        } else if (!(to == to0)) {
            console.log("abort: server changed who we are sending money to.");
        } else if (!(fee == fee0)) {
            console.log("abort: server changed the fee.");
        } else {
            console.log(JSON.stringify(tx));
            var stx = sign_tx(tx);
            console.log(JSON.stringify(stx));
            variable_public_get(["txs", [-6, stx]], function(x) {});
        }
    }
}
