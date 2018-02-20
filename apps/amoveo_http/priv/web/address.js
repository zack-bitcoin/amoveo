// actually for displaying your pubkey.
address1();
function address1() {
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(document.createElement("br"));
    var addressA = document.createElement("div");
    addressA.id = "addressA";
    document.body.appendChild(addressA);
    var height_button = document.createElement("BUTTON");
    var button_text_node = document.createTextNode("get pubkey");
    height_button.appendChild(button_text_node);
    height_button.onclick = address_func;
    document.body.appendChild(height_button);
    function address_func() {
        console.log("address 0");
        variable_get(["pubkey"], address1);
    }
    function address1(id) {
        console.log("address 1");
        var balance = document.getElementById("addressA");
        balance.innerHTML = "your pubkey: ".concat(atob(id));
        console.log("address 3");
    }
}

