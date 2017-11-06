document.body.appendChild(document.createElement("br"));
document.body.appendChild(document.createElement("br"));
var spend_div = document.createElement("div");
spend_div.id = "spend_div";
document.body.appendChild(spend_div);
spend1();
function spend1() {
    var spend_amount = document.createElement("INPUT");
    spend_amount.setAttribute("type", "text"); 
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = "amount to spend: ";
    div.appendChild(amount_info);
    div.appendChild(spend_amount);
    
    div = document.getElementById("spend_div");
    var spend_address = document.createElement("INPUT");
    spend_address.setAttribute("type", "text"); 
    var input_info = document.createElement("h8");
    input_info.innerHTML = "to pubkey: ";
    div.appendChild(input_info);
    div.appendChild(spend_address);
    
    //var spend_fee = document.createElement("INPUT");
    //spend_fee.setAttribute("type", "text"); 
    //var fee_info = document.createElement("h8");
    //fee_info.innerHTML = "fee: ";
    //div.appendChild(fee_info);
    //div.appendChild(spend_fee);
    
    var spend_button = document.createElement("BUTTON");
    spend_button.id = "spend_button";
    var spend_button_text = document.createTextNode("spend");
    spend_button.appendChild(spend_button_text);
    spend_button.onclick = function() {
	// var to = parseInt(spend_address.value, 10);
        var to = spend_address.value;
	// var fee = parseInt(spend_fee.value, 10);
	local_get(["spend", to, c2s(spend_amount)]);
    };
    div.appendChild(spend_button);

}
