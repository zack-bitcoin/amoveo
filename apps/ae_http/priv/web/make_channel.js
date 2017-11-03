variable_get(["channel_keys"], make_channel2);
function make_channel2(keys) {
    if ( ( keys.length == 1) && ( keys.pop() == -6 )) {
        make_channel1();
    } else {
        channel_data();
    }
}

//make_channel1();
function make_channel1() {
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(document.createElement("br"));
    var height = document.createElement("div");
    document.body.appendChild(height);
    var height_button = document.createElement("BUTTON");
    var button_text_node = document.createTextNode("make channel");
    height_button.appendChild(button_text_node);
    height_button.onclick = height_helper;
    document.body.appendChild(height_button);

    var spend_amount = document.createElement("INPUT");
    spend_amount.setAttribute("type", "text"); 
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = "amount to lock in channel: ";
    document.body.appendChild(amount_info);
    document.body.appendChild(spend_amount);

    var spend_delay = document.createElement("INPUT");
    spend_delay.setAttribute("type", "text"); 
    var delay_info = document.createElement("h8");
    delay_info.innerHTML = "channel delay: ";
    document.body.appendChild(delay_info);
    document.body.appendChild(spend_delay);

    function height_helper() {
        variable_get(["channel_keys"], register_doit);
    }
    function register_doit(x) {
        //if (typeof x == 'undefined'){
	//    setTimeout(function() {variable_get(["channel_keys"], function(x) {register_doit(x)});}, 1000);
        if ( ( x.length == 1 ) && ( x.pop() == -6 ) ) {
            var amount = parseInt(spend_amount.value, 10);
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
