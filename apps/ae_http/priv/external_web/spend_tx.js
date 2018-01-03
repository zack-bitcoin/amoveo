spend_1();
function spend_1() {
    var div = document.createElement("div");
    document.body.appendChild(div);
    div.appendChild(document.createElement("br"));
    var spend_amount = document.createElement("INPUT");
    spend_amount.setAttribute("type", "text");
    //spend_amount.id = "spend_amount";
    var spend_amount_info = document.createElement("h8");
    spend_amount_info.innerHTML = translate.words("amount_send").concat(": ");
    div.appendChild(spend_amount_info);
    div.appendChild(spend_amount);

    var spend_address = document.createElement("INPUT");
    spend_address.setAttribute("type", "text");
    //spend_address.id = "spend_address";
    var input_info = document.createElement("h8");
    input_info.innerHTML = translate.words("to_pubkey").concat(": ");
    div.appendChild(input_info);
    div.appendChild(spend_address);
    spend_button = button_maker("spend", spend_tokens);
    div.appendChild(spend_button);
    function spend_tokens() {
        //spend_address = document.getElementById("spend_address");
        var to = spend_address.value;
        //spend_amount = document.getElementById("spend_amount");
        var amount = Math.floor(parseFloat(spend_amount.value, 10) * 100000000);
        var from = keys.pub();
        var fee = 20;
        variable_public_get(["spend_tx", amount, fee, from, to],
                            spend_tokens2);
    }
    function spend_tokens2(tx) {
        var amount = Math.floor(parseFloat(spend_amount.value, 10) * 100000000);
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
            var stx = keys.sign(tx);
            console.log(JSON.stringify(stx));
            console.log("pubkey is ");
            console.log(to);
            console.log(keys.pub());
            variable_public_get(["txs", [-6, stx]], function(x) {});
        }
        spend_amount.value = "";
    }
}
