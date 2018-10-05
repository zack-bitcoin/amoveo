spend_1();
function spend_1() {
    var div = document.createElement("div");
    document.body.appendChild(div);
    div.appendChild(document.createElement("br"));
    var spend_amount = document.createElement("INPUT");
    spend_amount.setAttribute("type", "text");
    //spend_amount.id = "spend_amount";
    var spend_amount_info = document.createElement("h8");
    spend_amount_info.innerHTML = "amount to send: ";

    var spend_address = document.createElement("INPUT");
    spend_address.setAttribute("type", "text");
    //spend_address.id = "spend_address";
    var input_info = document.createElement("h8");
    input_info.innerHTML = "to pubkey: ";
    var raw_tx = document.createElement("h8");
    var mode;
    spend_button = button_maker2("send", function(){
	mode = "sign";
	spend_tokens();
    });
    raw_button = button_maker2("print unsigned transaction to screen", function(){
	mode = "raw";
	spend_tokens();
    });
    var error_msg = document.createElement("div");
    var calculate_max_send_button = button_maker2("calculate max send amount", function() {
	keys.check_balance(function(Amount) {
            var to0 = spend_address.value;
	    var to = parse_address(to0);
	    if (to == 0) {
		error_msg.innerHTML = "please input the recipient's address";
	    } else {
		error_msg.innerHTML = "";
	    }
	    var CB2 = function(fee) {
		var A2 = Amount - fee - 1;
		spend_amount.value = (A2 / token_units()).toString();
	    };
	    fee_checker(to, CB2, CB2);
	});
    });
    div.appendChild(calculate_max_send_button);
    div.appendChild(document.createElement("br"));
    div.appendChild(spend_amount_info);
    div.appendChild(spend_amount);
    div.appendChild(input_info);
    div.appendChild(spend_address);
    div.appendChild(spend_button);
    div.appendChild(raw_button);
    div.appendChild(error_msg);
    div.appendChild(document.createElement("br"));
    div.appendChild(raw_tx);
    var fee;
    function spend_tokens() {
        //spend_address = document.getElementById("spend_address");
        var to0 = spend_address.value;
	var to = parse_address(to0);
        var amount = Math.floor(parseFloat(spend_amount.value, 10) * token_units());
	
	if (to == 0) {
	    error_msg.innerHTML = "Badly formatted address";
	} else {
	    error_msg.innerHTML = "";
        //spend_amount = document.getElementById("spend_amount");
            var from = keys.pub();
	    fee_checker(to, function (Fee) {
		fee = Fee;
		variable_public_get(["create_account_tx", amount, Fee, from, to], spend_tokens2);
	    }, function (Fee) {
		fee = Fee;
		variable_public_get(["spend_tx", amount, Fee, from, to], spend_tokens2);
	    });
	}
    }
    function fee_checker(address, Callback1, Callback2) {
	variable_public_get(["account", address],
			    function(result) {
			       if (result == "empty") {
				   merkle.request_proof("governance", 14, function(gov_fee) {
				       var fee = tree_number_to_value(gov_fee[2]) + 50;
				       Callback1(fee);
				   });
			       } else {
				   merkle.request_proof("governance", 15, function(gov_fee) {
				       var fee = tree_number_to_value(gov_fee[2]) + 50;
				       Callback2(fee);
				   });
			       }});
    }
    function spend_tokens2(tx) {
        var amount = Math.floor(parseFloat(spend_amount.value, 10) * token_units());
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
		variable_public_get(["txs", [-6, stx]], function(x) {
		    console.log(x);
		    var msg = ((amount/token_units()).toString()).concat(" VEO successfully sent. txid =  ").concat(x);
		    error_msg.innerHTML = msg;
		});
	    } else if (mode == "raw") {
		raw_tx.innerHTML = JSON.stringify(tx);
	    }
        }
        spend_amount.value = "";
    }
}
