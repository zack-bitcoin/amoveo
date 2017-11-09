//local_get(["lightning_spend", [127,0,0,1], 3020, partner, amount]);
var lightning_spend_div = document.createElement("div");
lightning_spend_div.id = "lightning_spend_div";
document.body.appendChild(lightning_spend_div);
function lightning_spend1() {
    var div = document.getElementById("lightning_spend_div");
    div.appendChild(document.createElement("br"));
    div.appendChild(document.createElement("br"));
    var spend_address = document.createElement("INPUT");
    spend_address.setAttribute("type", "text"); 
    var input_info = document.createElement("h8");
    input_info.innerHTML = "spend to: ";
    //div.appendChild(document.createElement("br"));
    div.appendChild(input_info);
    div.appendChild(spend_address);


    var amount = document.createElement("INPUT");
    amount.setAttribute("type", "text"); 
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = "spend amount: ";
    div.appendChild(amount_info);
    div.appendChild(amount);
    
    var spend_button = document.createElement("BUTTON");
    spend_button.id = "lightning_button";
    var spend_button_text = document.createTextNode("lightning spend");
    spend_button.appendChild(spend_button_text);
    spend_button.onclick = function() {
	var B = c2s(amount);
	var C = B + (B%2);
	local_get(["lightning_spend",
                   JSON.parse(server_ip.value),
                   parseInt(server_port, 10),
                   spend_address.value, C]);
    };
    div.appendChild(spend_button);

}
