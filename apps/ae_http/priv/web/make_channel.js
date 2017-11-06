make_channel2();
function make_channel2() {
    var height = document.createElement("div");
    height.id = "make_channel";
    document.body.appendChild(height);
    variable_get(["channel_keys"], make_channel3);
}
function make_channel3(x) {
    console.log("make channel 2");
    console.log(x);
    if ( ( x.length == 1) && ( x.pop() == -6 )) {
        make_channel1();
    } else {
        to_channel1();
        lightning_spend1();
        market_bet1();
        //channel_data();
    }
}

//make_channel1();
function make_channel1() {
    var height = document.getElementById("make_channel");
    height.appendChild(document.createElement("br"));
    height.appendChild(document.createElement("br"));
    var height_button = document.createElement("BUTTON");
    var button_text_node = document.createTextNode("make channel");
    height_button.appendChild(button_text_node);
    height_button.onclick = height_helper;
    height.appendChild(height_button);

    var spend_amount = document.createElement("INPUT");
    spend_amount.setAttribute("type", "text"); 
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = "amount to lock in channel: ";
    height.appendChild(amount_info);
    height.appendChild(spend_amount);

    var spend_delay = document.createElement("INPUT");
    spend_delay.setAttribute("type", "text"); 
    var delay_info = document.createElement("h8");
    delay_info.innerHTML = "channel delay (in blocks): ";
    //document.body.appendChild(delay_info);
    //document.body.appendChild(spend_delay);
    height.appendChild(delay_info);
    height.appendChild(spend_delay);

    function height_helper() {
        variable_get(["channel_keys"], register_doit);
    }
    function register_doit(x) {
        //if (typeof x == 'undefined'){
	//    setTimeout(function() {variable_get(["channel_keys"], function(x) {register_doit(x)});}, 1000);
        if ( ( x.length == 1 ) && ( x.pop() == -6 ) ) {
            var amount = c2s(spend_amount);
            var delay = parseInt(spend_delay.value, 10);
            //var bal2 = parseInt(amount * 101 / 100);
            var bal2 = amount - 1;
            local_get(["new_channel_with_server", amount, bal2, delay]);
	    //variable_get(["id"], new_channel);
        } else {
	    console.log("did not work, x was");
	    console.log(x);
        }
    }
}

function channel_data() {

}
