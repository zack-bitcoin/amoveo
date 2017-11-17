
console.log("serialize test");
console.log(serialize([-6]));
console.log(JSON.stringify(serialize(["abc", 1, [-6]])));
tx = ["spend","BHjaeLteq9drDIhp8d0R6JmUqkivIW1M0Yoh5rsGnw4wePMKowcNGHqfttAF52jMYhsZicFr7eIOWN/Sr0XI+OI=",7,20,"BHuqX6EKohvveqkcbyGgE247jQ5O0i2YKO27Yx50cXd+8J/dCVTnMz8QWUUS9L5oGWUx5CPtseeHddZcygmGVaM=",100000000,[-6],0];
console.log(JSON.stringify(serialize(tx)));

    //sig = btoa(btoa(array_to_string(sign(tx, keys))));

var channels = {};
channels1();
function channels1() {
    //check if we have a chnnel with the server yet.
    //if we don't, then give an interface for making one.
    var cd;
    variable_get(["pubkey"], channels2);//ask for their pubkey
    function make_channel_func(pubkey) {
        var spend_amount = document.getElementById("spend_amount");
        var amount = parseFloat(spend_amount.value, 10) * 100000000;
        var spend_delay = document.getElementById("spend_delay");
        var delay = parseInt(spend_delay.value, 10);
        var bal2 = amount - 1;
        //[new_channel_with_server, amount, bal2, delay, ip, port]
        spend_amount.value = "";
        console.log("in make channel");
        console.log(pubkey);
        var fee = 20;
        var acc1 = pubkey_64();
        var acc2 = pubkey;
        //we only allow one channel per pair of accounts when you are off-chain, because we do not keep track of entropy.
        //let the server choose an unused cid for us.
        variable_public_get(["new_channel_tx", acc1, pubkey, amount, bal2,delay, fee], function(x) { make_channel_func2(x, amount, bal2, fee, acc1, acc2, delay); } );
    }
    function make_channel_func2(tx, amount, bal2, fee, acc1, acc2, delay) {
        //ask a server to make the tx for us, then check that all our data matches.
        console.log("make channel tx is ");
        console.log(tx);
        var amount0 = tx[5];
        var bal20 = tx[6];
        var fee0 = tx[3];
        var acc10 = tx[1];
        var acc20 = tx[2];
        var entropy = tx[7];
        var cid = tx[9];
        var delay0 = tx[8];
        if ((!(delay == delay0)) || (!(amount == amount0)) ||
            (!(bal2 == bal20)) || (!(fee == fee0)) ||
            (!(acc1 == acc10)) || (!(acc2 == acc20))) {
            console.log("server edited the tx. aborting");
        } else {
            console.log("tx is valid");
            var spk = ["spk", acc1, acc2, entropy, [-6], 0, 0, cid, 0, 0, delay];
            console.log("spk is ");
            console.log(JSON.stringify(spk));
            /*-record(spk, {acc1,acc2, entropy, 
	      bets, space_gas, time_gas, 
	      cid, amount = 0, nonce = 0,
	      delay = 0
	      }).
            */
            var stx = sign_tx(tx);
            console.log("signed tx");
            console.log(JSON.stringify(stx));
            var sspk = sign_tx(spk);
            console.log("signed spk");
            console.log(JSON.stringify(sspk));
            variable_get(["new_channel", stx, sspk], channels3);
        }
    }
    function channels3(x) {
        console.log("channels3 ");
        console.log(x);
        var sstx = x[0];
        var s2spk = x[1];
        variable_public_get(["txs", [-6, sstx]], function(x) {});
        var spk = s2spk[1];
        var cd = {"me": spk, "them": spk, "entropy": entropy, "cid": cid};
        channel_feeder__new_channel(sstx[1], s2spk);

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
            spend_amount.id = "spend_amount";
            var amount_info = document.createElement("h8");
            amount_info.innerHTML = "amount to lock in channel: ";
            make_channel.appendChild(amount_info);
            make_channel.appendChild(spend_amount);

            var spend_delay = document.createElement("INPUT");
            spend_delay.setAttribute("type", "text");
            spend_delay.id = "spend_delay";
            var delay_info = document.createElement("h8");
            delay_info.innerHTML = "channel delay (in blocks): ";
            make_channel.appendChild(delay_info);
            make_channel.appendChild(spend_delay);

            
        } else {
            console.log("give interfaces for: lightning spending, and making bets in channels.");
        }
    }
}
