spend1();

function spend1()
{
    var spend_address = document.createElement("INPUT");

    spend_address.setAttribute("type", "text");

    var input_info       = document.createElement("h8");
    input_info.innerHTML = "spend to: ";

    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(input_info);
    document.body.appendChild(spend_address);
    
    var spend_amount = document.createElement("INPUT");
    spend_amount.setAttribute("type", "text"); 

    var amount_info       = document.createElement("h8");
    amount_info.innerHTML = "amount to spend: ";

    document.body.appendChild(amount_info);
    document.body.appendChild(spend_amount);
    
    var spend_fee = document.createElement("INPUT");

    spend_fee.setAttribute("type", "text");

    var fee_info       = document.createElement("h8");
    fee_info.innerHTML = "fee: ";

    document.body.appendChild(fee_info);
    document.body.appendChild(spend_fee);
    
    var spend_button = document.createElement("BUTTON");
    spend_button.id  = "spend_button";

    var spend_button_text = document.createTextNode("spend");

    spend_button.appendChild(spend_button_text);

    spend_button.onclick =
        function()
        {
	        var to     = parseInt(spend_address.value, 10);
	        var amount = parseInt(spend_amount.value, 10);
	        var fee    = parseInt(spend_fee.value, 10);

	        local_get(["spend", to, amount, fee]);
        };

    document.body.appendChild(spend_button);

}
