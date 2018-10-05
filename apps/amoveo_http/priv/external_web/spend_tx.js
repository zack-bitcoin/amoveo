spend_1();

function spend_1() {
	var div = document.createElement("div");
	document.body.appendChild(div);
	//div.appendChild(document.createElement("br"));
	var spend_amount = document.createElement("INPUT");
	spend_amount.setAttribute("type", "text");
	spend_amount.id = "spend_amount";
	//spend_amount.id = "spend_amount";
	var spend_amount_info = document.createElement("label");
	spend_amount_info.innerHTML = "Amount to send:";
	spend_amount_info.htmlFor = "Spend_amount";

	var spend_address = document.createElement("TEXTAREA");
	//spend_address.setAttribute("type", "text");
	spend_address.id = "to-pubkey";
	//spend_address.id = "spend_address";
	var input_info = document.createElement("label");
	input_info.innerHTML = "To pubkey:";
	input_info.htmlFor = "to-pubkey";

	var raw_tx = document.createElement("label");
	var mode;
	spend_button = button_maker2("Send", function() {
		mode = "sign";
		spend_tokens();
	});
	spend_button.classList.add("btn_fw");
	spend_button.id = "spend_button"
	spend_button.disabled = true;

	raw_button = button_maker2("print unsigned transaction to screen", function() {
		mode = "raw";
		spend_tokens();
	});
	var error_msg = document.createElement("div");
	var calculate_max_send_button = button_maker2("Max amount", function() {
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

	var account_pubkey = document.getElementById('account_pubkey');

	var amount = wrapper("fieldset fieldset_nowr", [spend_amount_info, spend_amount, calculate_max_send_button]);
	var pubkey = wrapper("fieldset", [input_info, spend_address]);
	var amount_div = wrapper("tabs__box2", [amount, pubkey, spend_button, error_msg, raw_tx]);

	//append_children(account_pubkey, [amount, pubkey, error_msg, raw_tx, hr()]);

	account_pubkey.insertBefore(amount_div , account_pubkey.firstChild);

	var fee;

	function spend_tokens() {
		//spend_address = document.getElementById("spend_address");
		spend_button.classList.add("btn_loading");

		var to0 = spend_address.value;
		var to = parse_address(to0);
		var amount = Math.floor(parseFloat(spend_amount.value, 10) * token_units());
		console.log(amount);


		if (to == 0) {
			error_msg.innerHTML = "<p class='msg'>Badly formatted address</p>";
			spend_button.classList.remove("btn_loading");
		} else if (!amount) {
			console.log('empty');
			error_msg.innerHTML = "<p class='msg'>Amount field is empty</p>";
			spend_button.classList.remove("btn_loading");
		} else {
			error_msg.innerHTML = "";
			//spend_amount = document.getElementById("spend_amount");
			var from = keys.pub();
			fee_checker(to, function(Fee) {
				fee = Fee;
				variable_public_get(["create_account_tx", amount, Fee, from, to], spend_tokens2);
			}, function(Fee) {
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
				}
			});
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
			spend_button.classList.remove("btn_loading");
			error_msg.innerHTML = "<p class='msg'>Oops! Try again..</p>";
		} else if (!(to == to0)) {
			console.log("abort: server changed who we are sending money to.");
			spend_button.classList.remove("btn_loading");
			error_msg.innerHTML = "<p class='msg'>Oops! Try again..</p>";
		} else if (!(fee == fee0)) {
			console.log("fees");
			console.log(fee);
			console.log(fee0);
			console.log(JSON.stringify(tx));
			console.log("abort: server changed the fee.");
			spend_button.classList.remove("btn_loading");
			error_msg.innerHTML = "<p class='msg'>Oops! Try again..</p>";
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
					var msg = ((amount / token_units()).toString()).concat(" mveo successfully sent. txid =  ").concat(x);
					error_msg.innerHTML = "<p class='msg'>"+msg+"</p>";
				});
			} else if (mode == "raw") {
				raw_tx.innerHTML = JSON.stringify(tx);
			}
		}
		spend_amount.value = "";
		spend_button.classList.remove("btn_loading");
	}
}
