// we should also list the active oracles. order_book:keys().

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
    //var input_info = document.createElement("h8");
    //input_info.innerHTML = "pubkey: ";
    //document.body.appendChild(input_info);
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
    function lookup_oracle_helper() {
        var x = parseInt(lookup_oracle_address.value, 10);
        variable_public_get(["oracle", x], lookup_oracle_helper2);
    }
    function lookup_oracle_helper2(x) {
        console.log(x);
        console.log("price is ");
        console.log((x[2])/100);
        console.log("expires at ");
        console.log(x[6]);
        console.log("batch period is ");
        console.log(x[7]);
        console.log("height last matched is ");
        console.log(x[8]);
        console.log("buy orders ");
        console.log(x[3]);
        console.log("sell orders ");
        console.log(x[4]);
        price.innerHTML = "price 0-100 :".concat(((x[2])/100).toString());
        //lookup_oracle.innerHTML = "balance: ".concat(x[1]);
    }

}
