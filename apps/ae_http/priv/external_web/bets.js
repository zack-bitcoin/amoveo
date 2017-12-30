//bet: code, amount, key, meta
//spk: acc1, acc2, bets, space_gas, time_gas, cid, amount, nonce, delay
//cd: me, them, ssme, ssthem, emsg, live, cid
//["market",1,1,3000,"BJjOADT/mMg0BsqQkCDcEb/ylv6W85wipEKrY3qV5z3XvVrNygvVoEXsA6tncAoMuyvMB5Prepzqql3zZ1sDjjo=",40,1]
//{market, 1, MarketID, Expires, Pubkey, Period, OID}.
var outstanding_bets_button_div = document.createElement("div");
outstanding_bets_button_div.id = "outstanding_bets_button_div";
document.body.appendChild(outstanding_bets_button_div);
var outstanding_bets_div = document.createElement("div");
outstanding_bets_div.id = "outstanding_bets_div";
document.body.appendChild(outstanding_bets_div);
var offchain_assets_div = document.createElement("div");
offchain_assets_div.id = "offchain_assets_div";
document.body.appendChild(offchain_assets_div);
//outstanding_bets1();
function outstanding_bets1() {
    var button_div = document.getElementById("outstanding_bets_button_div");
    button_div.innerHTML = "";
    var div = document.getElementById("outstanding_bets_div");
    div.appendChild(document.createElement("br"));
    div.appendChild(document.createElement("br"));
    var button = document.createElement("BUTTON");
    //var button_text_node = document.createTextNode("update balance of off-chain assets");
    var button_text_node = document.createTextNode(get_words(refresh_bets));
    button.appendChild(button_text_node);
    button.onclick = outstanding_bets2;
    button_div.appendChild(button);
}
function outstanding_bets2() {
    var div = document.getElementById("outstanding_bets_div");
    var oadiv = document.getElementById("offchain_assets_div");
    variable_public_get(["pubkey"], outstanding_bets3);
    function outstanding_bets3(server_pubkey) {
        var x = channel_manager_read(server_pubkey);
        var bets = x.me[4];
        var ssme = x.ssme;
        div.innerHTML = "";
        oadiv.innerHTML = "";
        outstanding_bets_print_bets(bets, ssme, server_pubkey);
    }
}
function cancel_trade(n, server_pubkey) {
        //the nth bet in the channel (starting at 1) is a unmatched trade that we want to cancel.
    var oldCD = channel_manager_read(server_pubkey);
    var spk = oldCD.me;
    var ss = oldCD.ssme[n-2];
    //var sscode = ss[1];
    console.log("oldCD ssme, n");
    console.log(JSON.stringify([oldCD.ssme, n]));
    console.log("cancel trade ss is");
    console.log(JSON.stringify(ss));
    if (JSON.stringify(ss.code) == JSON.stringify([0,0,0,0,4])) {//this is what an unmatched trade looks like.
        var spk2 = remove_bet(n-1, spk);
        var sspk2 = sign_tx(spk2);
        var msg = ["cancel_trade", pubkey_64(), n, sspk2];
        variable_public_get(msg, function(x) {
            return cancel_trade2(x, sspk2, server_pubkey, n-2);
        });
    } else {
        console.log(ss);
        console.log("this trade has already been partially or fully matched. it cannot be canceled now.");
    }
}
function remove_bet(n, spk0) {
    var spk = JSON.parse(JSON.stringify(spk0));
    var bets = spk[3];
    var bet = bets[n];
    var bets2 = remove_nth(n, bets);
    var bet_meta = bet[4];
    var a;
    if (bet_meta == 0) {
        a = 0;
    } else {
        var bet_amount = bet[2];
        var cgran = 10000;
        var price = bet_meta[2];
        a = Math.floor((bet_amount * price) / cgran);
    }
    spk[3] = bets2;
    spk[7] = spk[7] + a;
    return spk;
}
function cancel_trade2(sspk2, sspk, server_pubkey, n) {
    var cd = channel_manager_read(server_pubkey);
    //verify that sspk2 is signed by our partner.
    var spk = sspk[1];
    var spk2 = sspk2[1];
    if (JSON.stringify(hash(serialize(spk))) ==
        JSON.stringify(hash(serialize(spk2)))) {
        cd.them = sspk2;
        cd.me = spk;
        cd.ssme = remove_nth(n, cd.ssme);
        cd.ssthem = remove_nth(n, cd.ssthem);
        channel_manager[server_pubkey] = cd;
        outstanding_bets2();
    } else {
        console.log("the server didn't calculate the same update as us");
        console.log(spk);
        console.log(spk2);
    }
}
function remove_nth(n, a) {
    var b = a.slice(0, n);
    var c = a.slice(n+1, a.length);
    return b.concat(c);
}
function outstanding_bets_print_bets(bets, ssme, server_pubkey) {
    var div = document.getElementById("outstanding_bets_div");
    var oadiv = document.getElementById("offchain_assets_div");
    var cancel_buttons = [];
    for (var i = 1; i < bets.length; i++) {
        var bet = bets[i];
        console.log("bet is ");
        console.log(bet);
        var oid = bet[3][6];
        var amount = bet[2];
        var order = document.createElement("h8");
        var outcome = "";
        var meta = bet[4];
        if (bet[4][1] == 1) {
            outcome = "true";
        } else if (bet[4][1] == 2) {
            outcome = "false";
        }
        console.log("making cancel orders button, ssme is");
        console.log(JSON.stringify(ssme));
        if ( JSON.stringify(ssme[i-1].code) == JSON.stringify([0,0,0,0,4]) ) {
            //console.log("unmatched");
            //console.log(JSON.stringify([i, oid, amount, "unmatched", bet[4]]));
            order.innerHTML = "in market ".concat(parseInt(oid)).concat(" you have an open order to trade this many tokens ").concat(s2c(amount)).concat(", you are trading at this price: ").concat(parseFloat(((bet[4][2])/100), 10)).concat(", you are betting on outcome: ").concat(outcome);
            div.appendChild(order);
            var cancel_button = document.createElement("input");
            cancel_button.type = 'button';
            //cancel_button.value = 'cancel trade';
            cancel_button.value = get_words("cancel").concat(get_words("contract"));
            div.appendChild(cancel_button);
            div.appendChild(document.createElement("br"));
            cancel_buttons.push(cancel_button);
        } else {
            //console.log("matched");
            //console.log(JSON.stringify([i, oid, amount, "matched", bet[4]]));
            order.innerHTML = get_words("market").concat(parseInt(oid)).concat(get_words("win_if")).concat(outcome).concat(get_words("amount")).concat(s2c(amount));
            oadiv.appendChild(order);
            oadiv.appendChild(document.createElement("br"));
        }
    }
    for (var i = 0; i < cancel_buttons.length; i++) {
        (function(k){
            cancel_buttons[i].onclick = function() { cancel_trade(k+2, server_pubkey); };
                                                    
        })(i);
    }
}
