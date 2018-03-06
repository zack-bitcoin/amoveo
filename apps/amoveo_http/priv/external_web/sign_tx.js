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
    var button = button_maker("sign", sign_tx);
    div.appendChild(button);
    div.appendChild(document.createElement("br"));
    var signed_tx = document.createElement("h8");
    div.appendChild(signed_tx);
    div.appendChild(document.createElement("br"));
    function sign_tx() {
	signed_tx.value = keys.sign(tx.value);
    }
})();
