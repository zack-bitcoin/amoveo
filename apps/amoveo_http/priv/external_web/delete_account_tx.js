(function() {
    var div = document.createElement("div");
    document.body.appendChild(div);
    div.appendChild(document.createElement("br"));

    var create_address = document.createElement("INPUT");
    create_address.setAttribute("type", "text"); 
    var create_info = document.createElement("h8");
    create_info.innerHTML = "to pubkey: ";
    div.appendChild(create_info);
    div.appendChild(create_address);
    var create_button = button_maker2("send all your money to this account", create_account);
    div.appendChild(create_button);
    div.appendChild(document.createElement("br"));
    var ca_fee = 152050;
    function create_account() {
        var to = create_address.value;
        var from = keys.pub();
        variable_public_get(["delete_acc_tx", to, from, ca_fee],
                            function(x) { create_tokens2(x, to, from, ca_fee);}
			   );
    }
    function create_tokens2(tx, to, from, ca_fee) {
        console.log("create account tx is ");
        console.log(tx);
        var from0 = tx[1];
        var fee0 = tx[3];
        var to0 = tx[4];
	if (!(to == to0)) {
            console.log("abort: server changed who we are sending money to.");
        } else if (!(ca_fee == fee0)) {
            console.log("abort: server changed the fee.");
        } else {
            var stx = keys.sign(tx);
            variable_public_get(["txs", [-6, stx]], function(x) {});
        }
    }
})();
