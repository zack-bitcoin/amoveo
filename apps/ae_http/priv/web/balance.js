document.body.appendChild(document.createElement("br"));
document.body.appendChild(document.createElement("br"));
var balance = document.createElement("div");
balance.id = "balance";
document.body.appendChild(balance);

var channel_balance = document.createElement("div");
channel_balance.id = "channel_balance";
document.body.appendChild(channel_balance);
var channel_balance2 = document.createElement("div");
channel_balance2.id = "channel_balance2";
document.body.appendChild(channel_balance2);
var b_button = document.createElement("BUTTON");
b_button.id = "balance_button";
var button_text_node = document.createTextNode("update balance");
b_button.appendChild(button_text_node);
b_button.onclick = balance_update;
document.body.appendChild(b_button);

function balance_update() {
    console.log("update balance");
    var balance1 = document.getElementById("balance");
    var balance2 = document.getElementById("channel_balance");
    var balance3 = document.getElementById("channel_balance2");
    balance1.innerHTML = "";
    balance2.innerHTML = "";
    balance3.innerHTML = "";
    balance1.appendChild(document.createElement("br"));
    balance2.appendChild(document.createElement("br"));
    balance3.appendChild(document.createElement("br"));
    variable_get(["balance"], balance_update2);
}
function balance_update2(bal) {
    console.log("update 2");
    console.log(bal);
    var balance = document.getElementById("balance");
    var b = (bal).toString();
    balance.innerHTML = "your balance ".concat(s2c(b));
    variable_get(["channel_balance", get_ip(), get_port()], balance_update3);
}
function balance_update3(channel_balance) {
    var balance = document.getElementById("channel_balance");
    balance.innerHTML = "channel balance ".concat((s2c(channel_balance)).toString());
    variable_get(["channel_balance2",
                  get_ip(),
                  get_port()],
                  balance_update4);
}
function balance_update4(channel_balance) {
    var balance = document.getElementById("channel_balance2");
    balance.innerHTML = "partner's channel balance ".concat((s2c(channel_balance)).toString());
}
