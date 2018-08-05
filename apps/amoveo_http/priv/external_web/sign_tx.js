(function() {
    var div = document.createElement("div");
    document.body.appendChild(div);
    div.appendChild(document.createElement("br"));

    var tx = document.createElement("INPUT");
    tx.setAttribute("type", "text");
    var info = document.createElement("h8");
    info.innerHTML = "sign transaction";
    div.appendChild(info);
    div.appendChild(tx);
    var button = button_maker2("sign tx ", sign_tx);
    div.appendChild(button);
    div.appendChild(document.createElement("br"));

    var tx_push = document.createElement("INPUT");
    tx.setAttribute("type", "text");
    var push_info = document.createElement("h8");
    push_info.innerHTML = "publish transaction";
    div.appendChild(push_info);
    div.appendChild(tx_push);
    var push_button = button_maker2("push tx ", push_tx);
    div.appendChild(push_button);
    div.appendChild(document.createElement("br"));

    var signed_tx = document.createElement("h8");
    div.appendChild(signed_tx);
    div.appendChild(document.createElement("br"));
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
