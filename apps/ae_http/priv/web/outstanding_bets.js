//display active bets, and balances in each asset type.
var outstanding_bets_button_div = document.createElement("div");
outstanding_bets_button_div.id = "outstanding_bets_button_div";
document.body.appendChild(outstanding_bets_button_div);
var outstanding_bets_div = document.createElement("div");
outstanding_bets_div.id = "outstanding_bets_div";
document.body.appendChild(outstanding_bets_div);
var offchain_assets_div = document.createElement("div");
offchain_assets_div.id = "offchain_assets_div";
document.body.appendChild(offchain_assets_div);
function outstanding_bets1() {
    var button_div = document.getElementById("outstanding_bets_button_div");
    button_div.innerHTML = "";
    var div = document.getElementById("outstanding_bets_div");
    div.appendChild(document.createElement("br"));
    div.appendChild(document.createElement("br"));
    var button = document.createElement("BUTTON");
    var button_text_node = document.createTextNode("update balance of off-chain assets");
    button.appendChild(button_text_node);
    button.onclick = outstanding_bets2;
    button_div.appendChild(button);
}
function outstanding_bets2() {
    var div = document.getElementById("outstanding_bets_div");
    var oadiv = document.getElementById("offchain_assets_div");
    variable_get(["channel_state"], outstanding_bets3);
    function outstanding_bets3(x) {
        x = x.pop();
        console.log("outstanding_bets3");
        console.log(JSON.stringify(x));
        //bet: code, amount, key, meta
        //spk: acc1, acc2, bets, space_gas, time_gas, cid, amount, nonce, delay
        //cd: me, them, ssme, ssthem, emsg, live, cid
        var me = x[1];
        console.log("me");
        console.log(JSON.stringify(me));
        var bets = me[4];
        console.log("bets");
        console.log(JSON.stringify(bets));
        var ssme = x[3];
        console.log("ssme");
        console.log(JSON.stringify(ssme));
        div.innerHTML = "";
        oadiv.innerHTML = "";
        outstanding_bets_print_bets(bets, ssme);
        //["market",1,1,3000,"BJjOADT/mMg0BsqQkCDcEb/ylv6W85wipEKrY3qV5z3XvVrNygvVoEXsA6tncAoMuyvMB5Prepzqql3zZ1sDjjo=",40,1]
    //{market, 1, MarketID, Expires, Pubkey, Period, OID}.
    }
}
function cancel_trade(n) {
        //the nth bet in the channel (starting at 1) is a unmatched trade that we want to cancel.
    console.log("cancel trade");
    console.log(n);
    variable_get(["cancel_trade", n,
                  JSON.parse(server_ip.value),
                  parseInt(server_port.value, 10)], cancel_trade2);
}
function cancel_trade2(x) {
    if ( x == 0 ) {
        outstanding_bets2();
        balance_update();
    }
    return 0;
}
function outstanding_bets_print_bets(bets, ssme) {
    var div = document.getElementById("outstanding_bets_div");
    var oadiv = document.getElementById("offchain_assets_div");
    var cancel_buttons = [];
    for (var i = 1; i < bets.length; i++) {
        var bet = bets[i];
        var oid = bet[3][6];
        var amount = bet[2];
        var order = document.createElement("h8");
            var outcome = "";
        var meta = bet[4];
        console.log("meta");
        console.log(meta);
        if (bet[4][1] == 1) {
            outcome = "true";
        } else if (bet[4][1] == 2) {
            outcome = "false";
        }
        if ( ssme[i][1] == "AAAAAAQ=" ) {
            //console.log("unmatched");
            //console.log(JSON.stringify([i, oid, amount, "unmatched", bet[4]]));
            order.innerHTML = "in market ".concat(parseInt(oid)).concat(" you have an open order to trade this many tokens ").concat(s2c(amount)).concat(", you are trading at this price: ").concat(parseFloat(((bet[4][2])/100), 10)).concat(", you are betting on outcome: ").concat(outcome);
            div.appendChild(order);
            //var cancel_button = document.createElement("BUTTON");
            var cancel_button = document.createElement("input");
            cancel_button.type = 'button';
            cancel_button.value = 'cancel trade';
            //cancel_button.id = "cancel_button".concat(i.toString());
            //cancel_button.onclick = function() { cancel_trade(i); };
            div.appendChild(cancel_button);
            div.appendChild(document.createElement("br"));
            cancel_buttons.push(cancel_button);
            
        } else {
            //console.log("matched");
            //console.log(JSON.stringify([i, oid, amount, "matched", bet[4]]));
            order.innerHTML = "in market ".concat(parseInt(oid)).concat(" you are betting on outcome ").concat(outcome).concat(" with this many tokens: ").concat(s2c(amount));
            oadiv.appendChild(order);
            oadiv.appendChild(document.createElement("br"));
        }
    }
    for (var i = 0; i < cancel_buttons.length; i++) {
        (function(k){
            cancel_buttons[i].onclick = function() { cancel_trade(k+2); };
                                                    
        })(i);
    }
}
