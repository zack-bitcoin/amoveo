spend1();
function spend1() {
    var amount = document.createElement("INPUT");
    amount.setAttribute("type", "text"); 
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = "channel spend amount: ";
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(amount_info);
    document.body.appendChild(amount);
    
    var spend_button = document.createElement("BUTTON");
    spend_button.id = "spend_button";
    var spend_button_text = document.createTextNode("channel spend");
    spend_button.appendChild(spend_button_text);
    spend_button.onclick = function() {
	local_get(["channel_spend", get_ip(), get_port(), parseInt(amount.value, 10)]);
    };
    document.body.appendChild(spend_button);

}
