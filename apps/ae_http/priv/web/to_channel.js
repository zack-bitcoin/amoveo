to_channel1();
function to_channel1() {
    var inc1 = document.createElement("INPUT");
    inc1.setAttribute("type", "text"); 
    var input_info = document.createElement("h8");
    input_info.innerHTML = "you pay: ";
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(input_info);
    document.body.appendChild(inc1);
    
    var inc2 = document.createElement("INPUT");
    inc2.setAttribute("type", "text"); 
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = "server pays: ";
    document.body.appendChild(amount_info);
    document.body.appendChild(inc2);
    
    var spend_fee = document.createElement("INPUT");
    spend_fee.setAttribute("type", "text"); 
    var fee_info = document.createElement("h8");
    fee_info.innerHTML = "fee: ";
    document.body.appendChild(fee_info);
    document.body.appendChild(spend_fee);
    
    var spend_button = document.createElement("BUTTON");
    spend_button.id = "spend_button";
    var spend_button_text = document.createTextNode("pay money into channel");
    spend_button.appendChild(spend_button_text);
    spend_button.onclick = function() {
	var b1 = parseInt(inc1.value, 10);
	var b2 = parseInt(inc2.value, 10);
	var fee = parseInt(spend_fee.value, 10);
	local_get(["grow_channel", IP, Port, b1, b2]);
	local_get(["sync", IP, Port]);
    };
    document.body.appendChild(spend_button);

}
