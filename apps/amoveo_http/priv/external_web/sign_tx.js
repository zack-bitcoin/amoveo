(function() {
    var div = document.createElement("div");
    content_block.appendChild(div);
    //div.appendChild(document.createElement("br"));

    var tx = document.createElement("TEXTAREA");
    //tx.setAttribute("type", "text");
    tx.id = "tx"
    var info = document.createElement("label");
    info.innerHTML = "Sign transaction:";

    var account = document.getElementById('transaction_wrap'); // keys.js

    var sign_button = button_maker2("Sign transaction", sign_tx);
    sign_button.id = "sign_button";
    sign_button.disabled = true;

    var tx_push = document.createElement("TEXTAREA");
    //tx.setAttribute("type", "text");
    var push_info = document.createElement("label");
    push_info.innerHTML = "Publish transaction:";
    var push_button = button_maker2("Push transaction", push_tx);
    push_button.id = "push_button";
    push_button.disabled = true;

    var signed_tx = document.createElement("div");
    signed_tx.innerHTML = ""

    var fieldset1 = wrapper("fieldset", [info, tx, signed_tx, sign_button]);
    var fieldset2 = wrapper("fieldset", [push_info, tx_push, push_button]);

    append_children(account, [fieldset1, fieldset2]);

    function sign_tx() {
	var t = JSON.parse(tx.value);
	console.log(tx.value);
	console.log(t);
	var t2 = keys.sign(t);
	console.log(t2);
	var s = JSON.stringify(t2);
	signed_tx.innerHTML = "<pre>"+s+"</div>";
	tx.value = "";
    }
    function push_tx() {
	var t = JSON.parse(tx_push.value);
	//console.log(t);
	//var t2 = keys.sign(t);
	variable_public_get(["txs", [-6, t]], function(x) {});
	tx_push.value = "";
    }
})();
