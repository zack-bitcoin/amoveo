var balance = document.createElement("div");
balance.id  = "balance";

document.body.appendChild(balance);

var channel_balance = document.createElement("div");
channel_balance.id  = "channel_balance";

document.body.appendChild(channel_balance);

var channel_balance2 = document.createElement("div");
channel_balance2.id  = "channel_balance2";

document.body.appendChild(channel_balance2);

var b_button         = document.createElement("BUTTON");
b_button.id          = "balance_button";
var button_text_node = document.createTextNode("update balance");

b_button.appendChild(button_text_node);

b_button.onclick = balance_update;

document.body.appendChild(b_button);

function balance_update()
{
    console.log("update balance");
    variable_get(["balance"], balance_update2);
}


function balance_update2(bal)
{
    console.log("update 2");
    console.log(bal);

    var balance       = document.getElementById("balance");
    var b             = (bal).toString();
    balance.innerHTML = "your balance ".concat(b);

    variable_get(["channel_balance", IP, Port], balance_update3);
}


function balance_update3(channel_balance)
{
    var balance = document.getElementById("channel_balance");
    balance.innerHTML = "channel balance ".concat((channel_balance).toString());
    variable_get(["channel_balance2", IP, Port], balance_update4);
}


function balance_update4(channel_balance)
{
    var balance       = document.getElementById("channel_balance2");
    balance.innerHTML = "partner's channel balance ".concat((channel_balance).toString());
}
