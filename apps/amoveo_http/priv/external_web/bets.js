//bet: code, amount, key, meta
//spk: acc1, acc2, bets, space_gas, time_gas, cid, amount, nonce, delay
//cd: me, them, ssme, ssthem, emsg, live, cid
//["market",1,1,3000,"BJjOADT/mMg0BsqQkCDcEb/ylv6W85wipEKrY3qV5z3XvVrNygvVoEXsA6tncAoMuyvMB5Prepzqql3zZ1sDjjo=",40,1]
//{market, 1, MarketID, Expires, Pubkey, Period, OID}.
function bets_main() {
    var div;
    var oadiv;
    function draw() {
	var bets_div = document.getElementById("bets_div");
	div = document.createElement("div");
	bets_div.appendChild(div);
	oadiv = document.createElement("div");
	bets_div.appendChild(oadiv);
    }
    function main() {
        variable_public_get(["pubkey"], outstanding_bets3);
    }
    function outstanding_bets3(server_pubkey) {
        var x = channels_object.read(server_pubkey);
	console.log("outstanding bets channel object bets are ");
	console.log(JSON.stringify(x.me[3].length));
	console.log(JSON.stringify(x.me[3]));
        var bets = x.me[3];
        var ssme = x.ssme;
        div.innerHTML = "";
        oadiv.innerHTML = "";
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
                console.log("unmatched");
                //console.log(JSON.stringify([i, oid, amount, "unmatched", bet[4]]));
                order.innerHTML = "in market ".concat(oid).concat(" you have an open order to trade this many tokens ").concat(s2c(amount)).concat(", you are trading at this price: ").concat(parseFloat(((bet[4][2])/100), 10)).concat(", you are betting on outcome: ").concat(outcome);
                div.appendChild(order);
                var cancel_button = document.createElement("input");
                cancel_button.type = 'button';
                cancel_button.value = "cancel trade";
                div.appendChild(cancel_button);
                div.appendChild(document.createElement("br"));
                cancel_buttons.push(cancel_button);
            } else {
                console.log("matched");
                order.innerHTML = ("market ").concat(oid).concat("you win if the outcome is ").concat(outcome).concat("amount ").concat(s2c(amount));
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
    function cancel_trade(n, server_pubkey) {
        //the nth bet in the channel (starting at 1) is a unmatched trade that we want to cancel.
        var oldCD = channels_object.read(server_pubkey);
        var spk = oldCD.me;
        var ss = oldCD.ssme[n-2];
        //var sscode = ss[1];
        console.log("oldCD ssme, n");
        console.log(JSON.stringify([oldCD.ssme, n]));
        console.log("cancel trade ss is");
        console.log(JSON.stringify(ss));
        if (JSON.stringify(ss.code) == JSON.stringify([0,0,0,0,4])) {//this is what an unmatched trade looks like.
            var spk2 = remove_bet(n-1, spk);
	    spk2[8] += 1000000;
            var sspk2 = keys.sign(spk2);
            var msg = ["cancel_trade", keys.pub(), n, sspk2];
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
        var cd = channels_object.read(server_pubkey);
	console.log("cancel trade2, fail to verify this: ");
	console.log(JSON.stringify(sspk2));
	var bool = verify_both(sspk2);
	if (!(bool)) {
	    throw("cancel trade badly signed");
	}
        var spk = sspk[1];
        var spk2 = sspk2[1];
        if (!(JSON.stringify(spk) ==
              JSON.stringify(spk2))) {
            console.log("the server didn't calculate the same update as us");
            console.log(spk);
            console.log(spk2);
	    throw("cancel trade spk does not match");
	}
        cd.them = sspk2;
        cd.me = spk;
        cd.ssme = remove_nth(n, cd.ssme);
        cd.ssthem = remove_nth(n, cd.ssthem);
        channels_object.write(server_pubkey, cd);
        main();
    }
    function remove_nth(n, a) {
        var b = a.slice(0, n);
        var c = a.slice(n+1, a.length);
        return b.concat(c);
    }
    return {main: main, draw: draw};
}
bets_object = bets_main(); 
