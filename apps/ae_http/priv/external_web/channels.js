
var channels = {};
channels1();
function channels1() {
    //check if we have a chnnel with the server yet.
    //if we don't, then give an interface for making one.

    variable_get(["pubkey"], channels2);//ask for their pubkey
    function make_channel_func(pubkey) {
        var amount = parseFloat(spend_amount.value, 10) * 100000000;
        var delay = parseInt(spend_delay.value, 10);
        var bal2 = amount - 1;
        //[new_channel_with_server, amount, bal2, delay, ip, port]
        spend_amount.value = "";
        console.log("in make channel");
        console.log(pubkey);
        return 0;
    }
    function channels2(pubkey) {
        console.log("pubkey is ");
        console.log(pubkey);
        var v = channels[pubkey];
        if (v == undefined) {
            console.log("give interface for making channels.");
            var make_channel = document.createElement("div");
            document.body.appendChild(make_channel);
            make_channel.appendChild(document.createElement("br"));
            var height_button = document.createElement("BUTTON");
            var button_text_node = document.createTextNode("make channel");
            height_button.appendChild(button_text_node);
            height_button.onclick = function() { return make_channel_func(pubkey) };
            make_channel.appendChild(height_button);
            
            var spend_amount = document.createElement("INPUT");
            spend_amount.setAttribute("type", "text"); 
            var amount_info = document.createElement("h8");
            amount_info.innerHTML = "amount to lock in channel: ";
            make_channel.appendChild(amount_info);
            make_channel.appendChild(spend_amount);

            var spend_delay = document.createElement("INPUT");
            spend_delay.setAttribute("type", "text"); 
            var delay_info = document.createElement("h8");
            delay_info.innerHTML = "channel delay (in blocks): ";
            make_channel.appendChild(delay_info);
            make_channel.appendChild(spend_delay);

            
        } else {
            console.log("give interfaces for: lightning spending, and making bets in channels.");
        }
    }
}
