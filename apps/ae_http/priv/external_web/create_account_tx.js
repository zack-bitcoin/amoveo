create_account1();
function create_account1() {
    var div = document.createElement("div");
    document.body.appendChild(div);
    div.appendChild(document.createElement("br"));
    var create_amount = document.createElement("INPUT");
    create_amount.setAttribute("type", "text"); 
    var create_amount_info = document.createElement("h8");
    create_amount_info.innerHTML = get_words("create_account").concat("- ").concat(get_words("initial_balance")).concat(": ");
    div.appendChild(create_amount_info);
    div.appendChild(create_amount);

    var create_address = document.createElement("INPUT");
    create_address.setAttribute("type", "text"); 
    var create_info = document.createElement("h8");
    create_info.innerHTML = get_words("to_pubkey").concat(": ");
    div.appendChild(create_info);
    div.appendChild(create_address);

    var create_button = document.createElement("BUTTON");
    var create_button_text = document.createTextNode(get_words("create_account"));
    create_button.appendChild(create_button_text);
    create_button.onclick = create_account;
    div.appendChild(create_button);
    div.appendChild(document.createElement("br"));

    function create_account() {
        var to = create_address.value;
        var amount = Math.floor(parseFloat(create_amount.value, 10) * 100000000);
        var from = pubkey_64();
        var fee = 20;
        console.log([amount, fee, from, to]);
        variable_public_get(["create_account_tx", amount, fee, from, to],
                            create_tokens2);
    }
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
            console.log(amount);
            console.log(amount0);
            console.log("abort: server changed the amount.");
        } else if (!(to == to0)) {
            console.log("abort: server changed who we are sending money to.");
        } else if (!(fee == fee0)) {
            console.log("abort: server changed the fee.");
        } else {
            var stx = sign_tx(tx);
            variable_public_get(["txs", [-6, stx]], function(x) {});
        }
        create_amount.value = "";
    }
}
