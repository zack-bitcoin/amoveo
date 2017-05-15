//local_get(["lightning_spend", [127,0,0,1], 3020, partner, amount]);

spend1();
function spend1() {
    var spend_address = document.createElement("INPUT");
    spend_address.setAttribute("type", "text"); 
    var input_info = document.createElement("h8");
    input_info.innerHTML = "spend to: ";
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(input_info);
    document.body.appendChild(spend_address);


    var amount = document.createElement("INPUT");
    amount.setAttribute("type", "text"); 
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = "channel spend amount: ";
    document.body.appendChild(amount_info);
    document.body.appendChild(amount);
    
    var spend_button = document.createElement("BUTTON");
    spend_button.id = "lightning_button";
    var spend_button_text = document.createTextNode("lightning spend");
    spend_button.appendChild(spend_button_text);
    spend_button.onclick = function() {
	var B = parseInt(amount.value, 10);
	var C = B + (B%2);
	local_get(["lightning_spend", IP, Port, parseInt(spend_address.value, 10), C]);
    };
    document.body.appendChild(spend_button);

}
