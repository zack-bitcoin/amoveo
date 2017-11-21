
var channel_manager = {};
channels1();
function channels1() {
    //check if we have a chnnel with the server yet.
    //if we don't, then give an interface for making one.
    var cd;
    document.body.appendChild(document.createElement("br"));
    var channels_div = document.createElement("div");
    document.body.append(channels_div);
    var channel_warning_div = document.createElement("div");
    channels_div.appendChild(channel_warning_div);
    var channel_interface_div = document.createElement("div");
    
    var load_button = document.createElement("input");
    load_button.type = "file";
    load_button.onchange = load_channels;
    channels_div.appendChild(load_button);

    channels_div.appendChild(document.createElement("br"));
    channels_div.appendChild(document.createElement("br"));
    var save_button = document.createElement("input");
    save_button.type = "button";
    save_button.value = "save channel data to file";
    save_button.onclick = save_channel_data;
    channels_div.appendChild(save_button);
    channels_div.appendChild(document.createElement("br"));
    channels_div.appendChild(channel_interface_div);
    
    variable_public_get(["pubkey"], refresh_channels_interfaces);
    function make_channel_func(pubkey) {
        var spend_amount = document.getElementById("spend_amount");
        var amount = parseFloat(spend_amount.value, 10) * 100000000;
        var spend_delay = document.getElementById("spend_delay");
        var delay = parseInt(spend_delay.value, 10);
        var bal2 = amount - 1;
        spend_amount.value = "";
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
            console.log(JSON.stringify([[delay, delay0],
                                        [amount, amount0],
                                        [bal2, bal20],
                                        [fee, fee0],
                                        [acc1, acc10],
                                        [acc2, acc20]]));
            console.log("server edited the tx. aborting");
        } else {
            console.log("tx is valid");
            var spk = ["spk", acc1, acc2, entropy, [-6], 0, 0, cid, 0, 0, delay];
            /*-record(spk, {acc1,acc2, entropy, 
	      bets, space_gas, time_gas, 
	      cid, amount = 0, nonce = 0,
	      delay = 0
	      }).
            */
            var stx = sign_tx(tx);
            var sspk = sign_tx(spk);
            console.log("signed spk");
            console.log(JSON.stringify(sspk));
            variable_public_get(["new_channel", stx, sspk], channels3);
        }
    }
    function channels3(x) {
        console.log("channels3 ");
        console.log(x);
        var sstx = x[1];
        var s2spk = x[2];
        var tx = sstx[1];
        var entropy = tx[7];
        var cid = tx[9];
        var acc2 = tx[2];
        console.log("double signed tx ");
        console.log(JSON.stringify(sstx));
        //variable_public_get(["txs", [-6, sstx]], function(x) {});
        var spk = s2spk[1];
        var cd = {"me": spk, "them": s2spk, "entropy": entropy, "cid": cid};
        console.log("cd is ");
        console.log(cd);
        channel_manager[acc2] = cd;
        channel_warning();
        variable_public_get(["pubkey"], refresh_channels_interfaces);//we already asked for the pubkey, it would be faster to reuse it instead of redownloading.
    }
    function channel_warning() {
        channel_warning_div.innerHTML = "channel state needs to be saved!~~~~~~~";
    }
    function save_channel_data() {
        download(JSON.stringify(channel_manager), "amoveo_channel_state", "text/plain");
        channel_warning_div.innerHTML = "channel state is saved.";
    }
    function load_channels() {
        var file = (load_button.files)[0];
        var reader = new FileReader();
        reader.onload = function(e) {
            channel_manager = JSON.parse(reader.result);
            variable_public_get(["pubkey"], refresh_channels_interfaces);//we already asked for the pubkey, it would be faster to reuse it instead of redownloading.
        }
        reader.readAsText(file);
    }
    function refresh_channels_interfaces(pubkey) {
        console.log("server pubkey is ");
        console.log(pubkey);
        var div = channel_interface_div;
        div.innerHTML = "";
        var v = channel_manager[pubkey];
        if (v == undefined) {
            var make_channel = document.createElement("div");
            console.log("give interface for making channels.");
            div.appendChild(make_channel);
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
            console.log("give interface for making bets in channels.");
            var balance_div = document.createElement("div");
            balance_div.id = "balance_div";
            balance_div.innerHTML = "your balance: unknown";
            div.appendChild(balance_div);

            var channel_balance_button = document.createElement("input");
            channel_balance_button.type = "button";
            channel_balance_button.value = "check channel balance";
            channel_balance_button.onclick = function() {refresh_balance(pubkey);};
            div.appendChild(channel_balance_button);
            div.appendChild(document.createElement("br"));

            var price = document.createElement("INPUT");
            price.setAttribute("type", "text");
            var price_info = document.createElement("h8");
            price_info.innerHTML = "price (between 0 and 100) : ";
            div.appendChild(price_info);
            div.appendChild(price);

            var trade_type = document.createElement("INPUT");
            trade_type.setAttribute("type", "text");
            var trade_type_info = document.createElement("h8");
            trade_type_info.innerHTML = "trade_type (either 'true' or 'false'): ";
            div.appendChild(trade_type_info);
            div.appendChild(trade_type);

            var amount = document.createElement("INPUT");
            amount.setAttribute("type", "text");
            var amount_info = document.createElement("h8");
            amount_info.innerHTML = "amount: ";
            div.appendChild(amount_info);
            div.appendChild(amount);

            var oid = document.createElement("INPUT");
            oid.setAttribute("type", "text");
            var oid_info = document.createElement("h8");
            oid_info.innerHTML = "market id: ";
            div.appendChild(oid_info);
            div.appendChild(oid);

            var button = document.createElement("BUTTON");
            button.id = "button";
            var buttonText = document.createTextNode("make bet");
            button.appendChild(buttonText);
            button.onclick = make_bet;
            div.appendChild(button);
            function make_bet() {
                //server's pubkey is pubkey.
                var oid_final = parseInt(oid.value, 10);
                variable_public_get(["market_data", oid_final], make_bet2);
            }
            function make_bet2(l) {
                var price_final = Math.floor(100 * parseFloat(price.value, 10));
                var type_final;
                if (trade_type.value == "true") {
                    type_final = 1;
                } else if (trade_type.value == "false") {
                    type_final = 2;
                }
                var amount_final = Math.floor(parseFloat(amount.value, 10) * 100000000);
                var oid_final = parseInt(oid.value, 10);
                var fee = 20
                console.log("expires, pubkey, period");
                console.log(JSON.stringify(l));
                //pubkey is server id.
                sc = market:market_smart_contract(betlocation, marketid, type, expires, price, pubkey, period, amount, oid);
                sspk = channel_feeder:trade(amount, sc, pubkey, oid_final)
                msg = {"trade", pubkey_64(), price_final, type_final, amount_final, oid_final, sspk, fee};
                variable_public_get(msg, make_bet3);
            }
            function make_bet3(sspk2) {
                //make sure that both spks match
                //store sspk2 and the new SS into the channel_manager.
                var ss = market:unmatched(oid);
                var ssk;
                amount.value = "";
                

            }
        }
    }
    function refresh_balance(pubkey) {
        console.log(channel_manager[pubkey]);
        var cd = channel_manager[pubkey];
        var trie_key = cd.me[7];//channel id, cid
        var top_hash = hash(serialize_header(top_header));
        variable_public_get(["proof", btoa("channels"), trie_key, btoa(array_to_string(top_hash))], function(x) { refresh_balance2(trie_key, x); });

    }
    function refresh_balance2(trie_key, proof) {
        // id acc1 acc2 bal1 bal2 amount nonce timeout_height, last_modified,
        // entropy, delay, slasher, closed

        //we should modify the balances based on the contract code we store
        var val = verify_merkle(trie_key, proof);
        var balance_div = document.getElementById("balance_div");
        var mybalance = (val[4] / 100000000).toString();
        var serverbalance = (val[5] / 100000000).toString();
        console.log(val[4]);
        console.log(parseInt(val[4], 10));
        console.log(mybalance);
        balance_div.innerHTML = ("your balance: ").concat(
            mybalance).concat("  server balance: ").concat(
                serverbalance);
        //add or remove more based on any channel state we are storing.
        console.log(val);
    }
}
