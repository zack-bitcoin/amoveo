(function() {
    var div = document.createElement("div");
    content_block.appendChild(div);
    //div.appendChild(document.createElement("br"));

    var tx = document.createElement("TEXTAREA");
    //tx.setAttribute("type", "text");
    tx.id = "tx"
    var info = document.createElement("label");
    info.innerHTML = "Sign transaction:";

    var account = document.getElementById('account_pubkey');

    var button = button_maker2("sign tx ", sign_tx);

    var fieldset1 = document.createElement("div");
    fieldset1.className = "fieldset";
    append_children(fieldset1, [info, tx, button]);

    var tx_push = document.createElement("TEXTAREA");
    //tx.setAttribute("type", "text");
    var push_info = document.createElement("label");
    push_info.innerHTML = "Publish transaction:";
    var push_button = button_maker2("push tx ", push_tx);

    var fieldset2 = document.createElement("div");
    fieldset2.className = "fieldset";
    append_children(fieldset2, [push_info, tx_push, push_button]);

    var signed_tx = document.createElement("label");

    append_children(account, [fieldset1, fieldset2, signed_tx]);

    function sign_tx() {
	var t = JSON.parse(tx.value);
	console.log(tx.value);
	console.log(t);
	var t2 = keys.sign(t);
	console.log(t2);
	var s = JSON.stringify(t2);
	signed_tx.innerHTML = s;
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
