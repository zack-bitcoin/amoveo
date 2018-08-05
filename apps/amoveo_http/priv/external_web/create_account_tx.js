(function create_account1() {
    var div = document.createElement("div");
    document.body.appendChild(div);
    div.appendChild(document.createElement("br"));
    var create_amount = document.createElement("INPUT");
    create_amount.setAttribute("type", "text"); 
    var create_amount_info = document.createElement("h8");
    create_amount_info.innerHTML = "create account - initial balance: ";
    div.appendChild(create_amount_info);
    div.appendChild(create_amount);

    var create_address = document.createElement("INPUT");
    create_address.setAttribute("type", "text"); 
    var create_info = document.createElement("h8");
    create_info.innerHTML = "to pubkey: ";
    div.appendChild(create_info);
    div.appendChild(create_address);
    var create_button = button_maker2("create account", create_account);
    div.appendChild(create_button);
    div.appendChild(document.createElement("br"));
    var ca_fee = 152050;
    function create_account() {
        var to = create_address.value;
        var amount = Math.floor(parseFloat(create_amount.value, 10) * token_units());
        var from = keys.pub();
        console.log([amount, ca_fee, from, to]);
        variable_public_get(["create_account_tx", amount, ca_fee, from, to],
                            create_tokens2);
    }
    function create_tokens2(tx) {
        console.log("create account tx is ");
        console.log(tx);
        var amount = Math.floor(parseFloat(create_amount.value, 10) * token_units());
        var amount0 = tx[5];
        var to = create_address.value;
        var to0 = tx[4];
        var fee0 = tx[3];
        if (!(amount == amount0)) {
            console.log(amount);
            console.log(amount0);
            console.log("abort: server changed the amount.");
        } else if (!(to == to0)) {
            console.log("abort: server changed who we are sending money to.");
        } else if (!(ca_fee == fee0)) {
            console.log("abort: server changed the fee.");
        } else {
            var stx = keys.sign(tx);
            variable_public_get(["txs", [-6, stx]], function(x) {});
        }
        create_amount.value = "";
    }
})();
