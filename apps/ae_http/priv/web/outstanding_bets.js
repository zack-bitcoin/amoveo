//display active bets, and balances in each asset type.
var outstanding_bets_button_div = document.createElement("div");
outstanding_bets_button_div.id = "outstanding_bets_button_div";
document.body.appendChild(outstanding_bets_button_div);
var outstanding_bets_div = document.createElement("div");
outstanding_bets_div.id = "outstanding_bets_div";
document.body.appendChild(outstanding_bets_div);
function outstanding_bets1() {
    var button_div = document.getElementById("outstanding_bets_button_div");
    var div = document.getElementById("outstanding_bets_div");
    div.appendChild(document.createElement("br"));
    div.appendChild(document.createElement("br"));
    var button = document.createElement("BUTTON");
    var button_text_node = document.createTextNode("update balance of off-chain assets");
    button.appendChild(button_text_node);
    button.onclick = outstanding_bets2;
    button_div.appendChild(button);
    function outstanding_bets2() {
        variable_get(["channel_state"], outstanding_bets3);
    }
    function outstanding_bets3(x) {
        x = x.pop();
        console.log("outstanding_bets3");
        console.log(JSON.stringify(x));
        //bet: code, amount, key, meta
        //spk: acc1, acc2, entropy, bets, space_gas, time_gas, cid, amount, nonce, delay
        //cd: me, them, ssme, ssthem, emsg, live, entropy, cid
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
        print_bets(bets, ssme);
        //["market",1,1,3000,"BJjOADT/mMg0BsqQkCDcEb/ylv6W85wipEKrY3qV5z3XvVrNygvVoEXsA6tncAoMuyvMB5Prepzqql3zZ1sDjjo=",40,1]
    //{market, 1, MarketID, Expires, Pubkey, Period, OID}.
    }
    function print_bets(bets, ssme) {
        for (var i = 1; i < bets.length; i++) {
            var bet = bets[i];
            var oid = bet[3][6];
            var amount = bet[2];
            var order = document.createElement("p");
            var outcome = "";
            var meta = bet[4];
            console.log("meta");
            console.log(meta);
            if (bet[4] == 1) {
                outcome = "true";
            } else if (bet[4] == 2) {
                outcome = "false";
            }
            if ( ssme[i][1] == "AAAAAAQ=" ) {
                //console.log("unmatched");
                //console.log(JSON.stringify([i, oid, amount, "unmatched", bet[4]]));
                order.innerHTML = "in market ".concat(parseInt(oid)).concat(" you have an open order to trade this many tokens ").concat(s2c(amount)).concat(", you are trading at this price: ").concat("??").concat(", you are betting on outcome: ").concat(outcome);
            } else {
                //console.log("matched");
                //console.log(JSON.stringify([i, oid, amount, "matched", bet[4]]));
                order.innerHTML = "in market ".concat(parseInt(oid)).concat(" you are betting on outcome ").concat(outcome).concat(" with this many tokens: ").concat(s2c(amount));
            }
            div.appendChild(order);
        }

    }
}
