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
    function lookup_oracle_helper() {
        var x = parseInt(lookup_oracle_address.value, 10);
        variable_public_get(["oracle", x], lookup_oracle_helper2);
    }
    function lookup_oracle_helper2(x) {
        console.log(x);
        //lookup_oracle.innerHTML = "balance: ".concat(x[1]);
    }

}
