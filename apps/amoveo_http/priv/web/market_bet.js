var market_bet_div = document.createElement("div");
market_bet_div.id = "market_bet_div";
document.body.appendChild(market_bet_div);
//market_bet1();
function market_bet1() {
    var market_bet_div = document.getElementById("market_bet_div");
    market_bet_div.appendChild(document.createElement("br"));
    market_bet_div.appendChild(document.createElement("br"));
    
    var price = document.createElement("INPUT");
    price.setAttribute("type", "text");
    var price_info = document.createElement("h8");
    price_info.innerHTML = "price (between 0 and 100) : ";
    market_bet_div.appendChild(price_info);
    market_bet_div.appendChild(price);
    
    var trade_type = document.createElement("INPUT");
    trade_type.setAttribute("type", "text");
    var trade_type_info = document.createElement("h8");
    trade_type_info.innerHTML = "trade_type (either 'true' or 'false'): ";
    market_bet_div.appendChild(trade_type_info);
    market_bet_div.appendChild(trade_type);
    
    var amount = document.createElement("INPUT");
    amount.setAttribute("type", "text");
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = "amount: ";
    market_bet_div.appendChild(amount_info);
    market_bet_div.appendChild(amount);
    
    var oid = document.createElement("INPUT");
    oid.setAttribute("type", "text");
    var oid_info = document.createElement("h8");
    oid_info.innerHTML = "market id: ";
    market_bet_div.appendChild(oid_info);
    market_bet_div.appendChild(oid);
    
    var button = document.createElement("BUTTON");
    button.id = "button";
    var buttonText = document.createTextNode("make bet");
    button.appendChild(buttonText);
    button.onclick = make_bet;
    market_bet_div.appendChild(button);
    
    function make_bet() {
        //trade(Price, Type, Amount, OID)
        return variable_get(["height"], make_bet2);
    }
    function make_bet2(height_integer) {
        var price_final = Math.floor(100 * parseFloat(price.value, 10));
        var type_final;
        if (trade_type.value == "true") {
            type_final = 1;
        } else if (trade_type.value == "false") {
            type_final = 2;
        }
        var amount_final = c2s(amount);
        var oid_final = parseInt(oid.value, 10);
        local_get(["trade", price_final, type_final, amount_final, oid_final,
                   height_integer,
                   JSON.parse(server_ip.value),
                   parseInt(server_port.value, 10)]);
        amount.value = "";
        outstanding_bets2();
        balance_update();
    }
}
