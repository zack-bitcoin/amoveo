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
    var raw_tx = document.createElement("h8");
    div.appendChild(input_info);
    div.appendChild(spend_address);
    var mode;
    spend_button = button_maker("spend", function(){
	mode = "sign";
	spend_tokens();
    });
    raw_button = button_maker("raw_spend", function(){
	mode = "raw";
	spend_tokens();
    });
    div.appendChild(spend_button);
    div.appendChild(raw_button);
    div.appendChild(document.createElement("br"));
    div.appendChild(raw_tx);
    var fee;
    function spend_tokens() {
        //spend_address = document.getElementById("spend_address");
        var to = spend_address.value;
        //spend_amount = document.getElementById("spend_amount");
        var amount = Math.floor(parseFloat(spend_amount.value, 10) * 100000000);
        var from = keys.pub();
	variable_public_get(["account", to],
			    function(result) {
			       if (result == "empty") {
				   merkle.request_proof("governance", 14, function(gov_fee) {
				       fee = tree_number_to_value(gov_fee[2]) + 50;
				       variable_public_get(["create_account_tx", amount, fee, from, to], spend_tokens2);
				   
				   });
			       } else {
				   merkle.request_proof("governance", 15, function(gov_fee) {
				       fee = tree_number_to_value(gov_fee[2]) + 50;
				       variable_public_get(["spend_tx", amount, fee, from, to], spend_tokens2);
				   });
			       }});
    }
    function spend_tokens2(tx) {
        var amount = Math.floor(parseFloat(spend_amount.value, 10) * 100000000);
        var amount0 = tx[5];
        var to = spend_address.value;
        var to0 = tx[4];
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
	    console.log("fees");
	    console.log(fee);
	    console.log(fee0);
	    console.log(JSON.stringify(tx));
            console.log("abort: server changed the fee.");
        } else {
            console.log(JSON.stringify(tx));
	    if (mode == "sign") {
		var stx = keys.sign(tx);
		console.log(JSON.stringify(stx));
		console.log("pubkey is ");
		console.log(to);
		console.log(keys.pub());
		variable_public_get(["txs", [-6, stx]], function(x) {});
	    } else if (mode == "raw") {
		raw_tx.innerHTML = JSON.stringify(tx);
	    }
        }
        spend_amount.value = "";
    }
}
