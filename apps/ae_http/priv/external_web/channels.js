
function channels_main() {
    //Model
    var channel_manager = {};
    var tv = -1;
    function read(x) {
        var y = channel_manager[x];
        if (y == undefined) {
            return undefined;
        } else {
            return JSON.parse(JSON.stringify(y));
        }
    }
    function write(key, value) {
        channel_manager[key] = value;
    }
    function new_cd(me, them, ssme, ssthem, expiration, cid) {
        return {"me": me, "them": them, "ssme": ssme, "ssthem": ssthem, "cid":cid, "expiration": expiration};
    }
    function new_ss(code, prove, meta) {
        if (meta == undefined) {
            meta = 0;
        }
        return {"code": code, "prove": prove, "meta": meta};
    }

    //View

    var channel_title = document.createElement("h3");
    channel_title.innerHTML = translate.words("channel");
    var channels_div = document.createElement("div");
    var channel_warning_div = document.createElement("div");
    var channel_interface_div = document.createElement("div");
    var load_button = document.createElement("input");
    load_button.type = "file";
    var save_name = document.createElement("INPUT");
    save_name.type = "text";
    save_name.id = "channel_name";
    save_name.value = translate.words("channel_state");
    var save_button = button_maker("save_channel", save_channel_data);
    var refresh_channels_button = button_maker("refresh_channels_interfaces_button", function() {
        variable_public_get(["pubkey"], function(pubkey) {
            return refresh_channels_interfaces(pubkey);
        });
    });
    document.body.appendChild(channel_title);
    document.body.appendChild(channels_div);
    append_children(channels_div, [channel_warning_div, load_button, br(), br(), save_name, save_button, br(), refresh_channels_button, br(), br(), channel_interface_div]);
    
    var fee = 50050;
    var oid = document.createElement("INPUT");
    oid.setAttribute("type", "text");
    var oid_info = document.createElement("h8");
    oid_info.innerHTML = translate.words("market").concat(": ");
    var price = document.createElement("INPUT");
    price.setAttribute("type", "text");
    var price_info = document.createElement("h8");
    price_info.innerHTML = translate.words("price").concat(" (between 0 and 100) : ");
    var trade_type = document.createElement("INPUT");
    trade_type.setAttribute("type", "text");
    var trade_type_info = document.createElement("h8");
    trade_type_info.innerHTML = translate.words("trade_type").concat(translate.words("true")).concat(translate.words("false"));
    var amount = document.createElement("INPUT");
    amount.setAttribute("type", "text");
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = translate.words("amount").concat(": ");
    var height_button = button_maker("make_channel", function() { })
    var spend_amount = document.createElement("INPUT");
    spend_amount.setAttribute("type", "text");
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = translate.words("amount_channel");
    var spend_delay = document.createElement("INPUT");
    spend_delay.setAttribute("type", "text");
    var delay_info = document.createElement("h8");
    delay_info.innerHTML = translate.words("channel_delay");
    var lifespan = document.createElement("input");
    lifespan.type = "text";
    var lifespan_info = document.createElement("h8");
    lifespan_info.innerHTML = translate.words("channel_lifespan");
    var balance_div = document.createElement("div");
    balance_div.innerHTML = translate.words("your_balance").concat(translate.words("unknown"));
    var channel_balance_button = button_maker("check_channel", function() { });
    var market_title = document.createElement("h3");
    market_title.innerHTML = translate.words("markets");
    var market_link = document.createElement("a");
    market_link.innerHTML = translate.words("markets_link");
    market_link.href = "http://146.185.142.103:8080/explorer.html";
    var button = button_maker("make_bet", make_bet);
    var bet_update_button = button_maker("finalize_bets", function() {});
    var combine_cancel_button = button_maker("gather_bets", function() {});
    var list_bets_button = button_maker("refresh_bets", bets_object.main);
    var close_channel_button = button_maker("close_channel", function(){ return; });
    variable_public_get(["pubkey"], function(pubkey) {
        return refresh_channels_interfaces(pubkey);
    });
    function channel_warning() {
        channel_warning_div.innerHTML = "channel state needs to be saved!~~~~~~~";
    }
    function save_channel_data() {
        var save_name = document.getElementById("channel_name");
        download(JSON.stringify(channel_manager), save_name.value, "text/plain");
        channel_warning_div.innerHTML = translate.words("save_confirm");
    }
    function load_channels(pubkey) {
        var file = (load_button.files)[0];
        var reader = new FileReader();
        reader.onload = function(e) {
            channel_manager = JSON.parse(reader.result);
            refresh_channels_interfaces(pubkey);
        }
        reader.readAsText(file);
    }
    function refresh_channels_interfaces(pubkey) {
        console.log("refresh channels interfaces");
        variable_public_get(["time_value"], function(x) {
            tv = x;
            refresh_channels_interfaces2(pubkey);
        });
    }
    function refresh_channels_interfaces2(pubkey) {
        load_button.onchange = function() {return load_channels(pubkey) };
        //refresh_channels_button.onclick = function() {return refresh_channels_interfaces2(pubkey)};
        var div = channel_interface_div;
        div.innerHTML = "";
        var tv_display = document.createElement("div");
        tv_display.innerHTML = translate.words("time_value").concat(": ").concat((tv).toString());
        div.appendChild(tv_display);
        //check if we have a chnnel with the server yet.
        //if we don't, then give an interface for making one.
        if (read(pubkey) == undefined) {
            console.log("give interface for making channels.");
            height_button.onclick = function() { return make_channel_func(pubkey) };
            append_children(div, [height_button, amount_info, spend_amount, br(), delay_info, spend_delay, br(), lifespan_info, lifespan]);
        } else {
            console.log("give interface for making bets in channels.");
            append_children(div, [balance_div, channel_balance_button, br(), market_title, market_link, br(), price_info, price, trade_type_info, trade_type, amount_info, amount, oid_info, oid, button, br(), bet_update_button, br(), br(), combine_cancel_button, br(), br(), list_bets_button, br()]);
            channel_balance_button.onclick = function() {refresh_balance(pubkey);};
            bet_update_button.onclick = function() {
                chalang_object.pull_channel_state();
                refresh_channels_interfaces(pubkey);
            };
            combine_cancel_button.onclick = function() {
                combine_cancel_object.main(pubkey);
            }
        }
    }
    function make_bet() {
        var oid_final = parseInt(oid.value, 10);
        variable_public_get(["market_data", oid_final], make_bet2);
    }
    function make_bet2(l) {
        var price_final = Math.floor(100 * parseFloat(price.value, 10));
        var type_final;
        var ttv = trade_type.value;
        if ((ttv == "true") ||
            (ttv == 1) ||
            (ttv == "yes") ||
            (ttv == "si") ||
            (ttv == "cierto") ||
            (ttv == "lon") ||
            (ttv == "真正") ||
            (ttv == "既不是")) {
            type_final = 1;
        } else if ((ttv == "false") ||
                   (ttv == 0) ||
                   (ttv == 2) ||
                   (ttv == "falso") ||
                   (ttv == "no") ||
                   (ttv == "lon ala") ||
                   (ttv == "也不是") ||
                   (ttv == "假")) {
            type_final = 2;
        }
        var amount_final = Math.floor(parseFloat(amount.value, 10) * 100000000);
        var oid_final = parseInt(oid.value, 10);
        var expires = l[1];
        var server_pubkey = l[2];
        var period = l[3];
        var sc = market_contract(type_final, expires, price_final, server_pubkey, period, amount_final, oid_final, headers_object.top()[1]);
        var cd = read(pubkey);
        var spk = market_trade(cd, amount_final, price_final, sc, pubkey, oid_final);
        var sspk = keys.sign(spk);
        var msg = ["trade", keys.pub(), price_final, type_final, amount_final, oid_final, sspk, fee];
        return variable_public_get(msg, function(x) {
            make_bet3(x, sspk, server_pubkey, oid_final);
        });
    }
    function make_bet3(sspk2, sspk, server_pubkey, oid_final) {
        //verify signature on sspk2
        var hspk2 = JSON.stringify(sspk2[1]);
        var hspk = JSON.stringify(sspk[1]);
        if (hspk2 == hspk) { //make sure that both spks match
            var cd = read(server_pubkey);
            cd.me = sspk[1];
            cd.them = sspk2;
            var newss = new_ss([0,0,0,0,4], [-6, ["oracles", oid_final]]);
            cd.ssme = ([newss]).concat(cd.ssme);
            cd.ssthem = ([newss]).concat(cd.ssthem);
            write(server_pubkey, cd);
            amount.value = "";
            channel_warning();
        } else {
            console.log("error, we calculated the spk differently from the server. you calculated this: ");
            console.log(JSON.stringify(sspk[1]));
            console.log("the server calculated this: ");
            console.log(JSON.stringify(sspk2[1]));
        }
    }

    //Controller

    function make_channel_func(pubkey) {
        var amount = Math.floor(parseFloat(spend_amount.value, 10) * 100000000);
        var delay = parseInt(spend_delay.value, 10);
        var expiration = parseInt(lifespan.value, 10) + headers_object.top()[1];
        var bal2 = amount - 1;
        spend_amount.value = "";
        var acc1 = keys.pub();
        var acc2 = pubkey;
        //let the server choose an unused cid for us.
        variable_public_get(["new_channel_tx", acc1, pubkey, amount, bal2,delay, fee], function(x) { make_channel_func2(x, amount, bal2, acc1, acc2, delay, expiration, pubkey); } );
    }
    function make_channel_func2(tx, amount, bal2, acc1, acc2, delay, expiration, pubkey) {
        //ask a server to make the tx for us, then check that all our data matches.
        console.log("make channel tx is ");
        console.log(tx);
        var amount0 = tx[5];
        var bal20 = tx[6];
        var fee0 = tx[3];
        var acc10 = tx[1];
        var acc20 = tx[2];
        var cid = tx[8];
        var delay0 = tx[7];
        if ((!(delay == delay0)) || (!(amount == amount0)) ||
            (!(bal2 == bal20)) || (!(fee == fee0)) ||
            (!(acc1 == acc10)) || (!(acc2 == acc20))) {
            console.log(JSON.stringify([[delay, delay0], [amount, amount0], [bal2, bal20], [fee, fee0], [acc1, acc10], [acc2, acc20]]));
            console.log("server edited the tx. aborting");
        } else {
            var current_height = headers_object.top()[1];
            var lifespan = expiration - current_height;
            var spk_amount = Math.floor((tv * (delay + lifespan) * (amount + bal2) ) / 100000000);
            var spk = ["spk", acc1, acc2, [-6], 0, 0, cid, spk_amount, 0, delay];
            var stx = keys.sign(tx);
            var sspk = keys.sign(spk);
            variable_public_get(["new_channel", stx, sspk, expiration], function(x) { return channels3(x, expiration, pubkey) });
        }
    }
    function channels3(x, expiration, pubkey) {//we should also pass tx and spk, to verify that the server didn't manipulate them.
        var sstx = x[1];
        var s2spk = x[2];
        // verify that both are signed twice.
        // verify that this is the same spk that we requested they sign.
        var tx = sstx[1];
        var cid = tx[9];
        var acc2 = tx[2];
        console.log("double signed tx ");
        console.log(JSON.stringify(sstx));
        //variable_public_get(["txs", [-6, sstx]], function(x) {});
        var spk = s2spk[1];
        var cd = new_cd(spk, s2spk, [], [], expiration, cid);
        write(acc2, cd);
        channel_warning();
        refresh_channels_interfaces(pubkey);
    }
    function refresh_balance(pubkey) {
        //console.log(channel_manager[pubkey]);
        var cd = read(pubkey);
        var trie_key = cd.me[6];//channel id, cid
        var top_hash = hash(headers_object.serialize(headers_object.top()));
        verify_callback("channels", trie_key, function(val) {
            //var balance_div = document.getElementById("balance_div");
            var spk = cd.them[1];
            var amount = spk[7];
            var betAmount = sum_bets(spk[3]);

            var mybalance = ((val[4] - amount - betAmount)/ 100000000).toString();
            var serverbalance = ((val[5] + amount) / 100000000).toString();
            balance_div.innerHTML = (translate.words("your_balance").concat(": ")).concat(
                mybalance).concat(translate.words("server_balance").concat(": ")).concat(
                    serverbalance);
        });
    }
    function sum_bets(bets) {
        var x = 0;
        for (var i = 1; i < bets.length; i++) {
            console.log("sum bets bet is ");
            console.log(JSON.stringify(bets[i][2]));
            x += bets[i][2];
        }
        return x;
    }
    return {new_cd: new_cd,
            read: read,
            new_ss: new_ss,
            write: write}
}
var channels_object = channels_main();
