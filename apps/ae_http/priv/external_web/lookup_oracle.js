list_oracles1();
function list_oracles1() {
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(document.createElement("br"));
    var lookup_oracle = document.createElement("div");
    document.body.appendChild(lookup_oracle);
    var lookup_oracle_button = document.createElement("BUTTON");
    var lookup_oracle_text_node = document.createTextNode("list markets");
    lookup_oracle_button.appendChild(lookup_oracle_text_node);
    lookup_oracle_button.onclick = lookup_helper;
    document.body.appendChild(lookup_oracle_button);
    function lookup_helper() {
        variable_public_get(["list_oracles"], lookup_helper2);
    }
    function lookup_helper2(x) {
        lookup_oracle.innerHTML = "live markets: ".concat(x.slice(1));
    }
}

lookup_oracle1();
function lookup_oracle1() {
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(document.createElement("br"));
    var lookup_oracle = document.createElement("div");
    document.body.appendChild(lookup_oracle);
    var lookup_oracle_address = document.createElement("INPUT");
    lookup_oracle_address.setAttribute("type", "text");
    document.body.appendChild(lookup_oracle_address);

    var lookup_oracle_button = document.createElement("BUTTON");
    var lookup_oracle_text_node = document.createTextNode("lookup market");
    lookup_oracle_button.appendChild(lookup_oracle_text_node);
    lookup_oracle_button.onclick = lookup_oracle_helper;
    document.body.appendChild(lookup_oracle_button);
    document.body.appendChild(document.createElement("br"));
    var price = document.createElement("div");
    document.body.appendChild(price);
    var expires = document.createElement("div");
    document.body.appendChild(expires);
    var batch_period = document.createElement("div");
    document.body.appendChild(expires);
    var height = document.createElement("div");
    document.body.appendChild(height);
    var chart = document.createElement("div");
    document.body.appendChild(chart);
    function lookup_oracle_helper() {
        var x = parseInt(lookup_oracle_address.value, 10);
        variable_public_get(["oracle", x], lookup_oracle_helper2);
    }
    function lookup_oracle_helper2(x) {
        console.log(x);
        console.log("buy orders ");
        console.log(x[3]);
        console.log("sell orders ");
        console.log(x[4]);
        var P = ((x[2])/100);
        price.innerHTML = "odds of event occuring: ".concat(P.toString()).concat("%");
        expires.innerHTML = "expires at height: ".concat((x[6]).toString());
        batch_period.innerHTML = "batch period: ".concat((x[7]).toString());
        height.innerHTML = "last height matched: ".concat((x[8]).toString());
        console.log("last line");
        var buys = JSON.stringify(price_amount(x[3]));
        var sells = JSON.stringify(price_amount(x[4]));
        chart.innerHTML = "open orders: {price from 0 to 10000, amount}, buys: ".concat(buys).concat(" sells: ").concat(sells);
        //lookup_oracle.innerHTML = "balance: ".concat(x[1]);
    }

}
function price_amount(L) {
    var x = [];
    for (var i = 1; i < L.length; i++) {
        x.push([L[i][2], L[i][4]]);
    }
    return x.reverse();
}
