function combine_cancel_assets_maker() {
    function matchable(bet, ss) {
        var ssc = ss.code;
        var bk = bet[3];
        var direction = bet[4][1];
        var price = bet[4][2];
        var price2 = ss.meta;//meta is price for ss
        console.log("matchable ssc is ");
        console.log(JSON.stringify(["prices", price, price2]));
        //console.log(JSON.stringify(ss));
        if (ssc == [0,0,0,0,4]) {
            console.log(ssc);
            console.log("not cancelable because it is an open order");
            return false;
        } else if ((!(bk.length == 7)) && (!(bk.length == 9))) {
            console.log("bk is wrong length");
            console.log(JSON.stringify(bk));
            throw("match length error");
            return false;
        } else if (!(bk[0] == "market")) {
            console.log("key is not market type");
            console.log(bk[0]);
            throw("match market key");
            return false;
        } else if ((!(bk[1] == 1)) && (!(bk[1] == 2))) {
            console.log("key is not of an existing market type");
            throw("match market key 2");
            console.log(bk[1]);
            return false;
        } else if (price2 == price) {
            console.log("not cancelable because it is a partially open order.");
            console.log(JSON.stringify([price2, price]));
            return false;
        } else {
            return true;
        } 
    }
    function combine_cancel_common4(bet, ssm, bt, mt) {
        var b4 = [];
        var m4 = [];
        var amount;
        var direction1;
        var direction2;
        var key1;
        var key2;
        var oid1;
        var oid2;
        var bm;
        var a1;
        var a2;
        for (var j = 0; j < bt.length; j++) {
            amount = bet[2];
            direction1 = bet[4][1]; //4 is for bets
            direction2 = bt[j][4][1];
            key1 = bet[3];
            key2 = bt[j][3];
            oid1 = key1[6];
            oid2 = key2[6];
            bm = matchable(bt[j], mt[j]);
            if (amount == 0) {
                console.log("combine cancel common 4 amount 0");
                return {"ob": [], "om": [],
                        "bets": (bt.slice(j, bt.length)).reverse().concat(b4),
                        "ss": (mt.slice(j, mt.length)).reverse().concat(m4)}
            } else if ((!(bm)) || (!(oid1 == oid2)) || (direction1 == direction2) || (!(key1[1] == key2[1]))) {
                console.log("not matchable or different oracle, or different direction");
                console.log(JSON.stringify([bm, [oid1, oid2], [direction1, direction2]])),
                b4 = [bt[j]].concat(b4);
                //b4 = [bt[0]].concat(b4);
                m4 = [mt[j]].concat(m4);
                //m4 = [mt[0]].concat(m4);
                //bt = bt.slice(1, bt.length);
                //mt = mt.slice(1, mt.length);
            } else {
                a1 = bet[2];//amount
                a2 = bt[j][2];
                if (a1 == a2) {
                    console.log("match both away");
                    return {"ob": [], "om": [],
                            "bets": (bt.slice(j, bt.length)).reverse().concat(b4),
                            "ss": (mt.slice(j, mt.length)).reverse().concat(m4)};
                } else if (a1 > a2) {
                    console.log("match first away");
                    //bet = spk_update_bet_amount(bet, a1 - a2);
                    bet[2] = a1 - a2;
                    //bt = bt.slice(1, bt.length);
                    //mt = mt.slice(1, mt.length);
                } else if (a1 < a2) {
                    console.log("match other away");
                    //var bh2 = JSON.parse(JSON.stringify(bt[j]));
                    //bh2[2] = a2 - a1;
                    bt[j][2] = a2 - a1;
                    return {"ob": [], "om":[],
                            "bets": (bt.slice(j, bt.length)).reverse().concat(b4),
                            "ss": (mt.slice(j, mt.length)).reverse().concat(m4)}
                } else {
                    throw("combine cancel amount error");
                }
            }
        }
        //var amount = bet[2];
        console.log("combine cancel finishing amount is ");
        console.log(bet[2]);
        if (bet[2] == 0) {
            return {"ob": [], "om": [], "bets": b4, "ss": m4};
        } else {
            return {"ob": [bet], "om": [ssm], "bets": b4, "ss": m4};
        }
    }
    function combine_cancel_common2(bets, ss) {
        var ob = [];
        var om = [];
        for (var i = 0; i < bets.length; i++) {
            var amount = bets[i][2];
            if (amount == 0) {
                console.log("amount is 0");
            } else {
                var b = matchable(bets[i], ss[i]);
                if (b) {
                    var canceled =
                        combine_cancel_common4(bets[i], ss[i], 
                                               bets.slice(i+1, bets.length),
                                               ss.slice(i+1, ss.length));
                    bets = canceled.bets;
                    ss = canceled.ss;
                    ob = (canceled.ob).concat(ob);
                    om = (canceled.om).concat(om);

                } else {
                    ob = ([bets[i]]).concat(ob);
                    om = ([ss[i]]).concat(om);
                }
            }
        }
        return {"bets": ob, "ss": om};
    }
    function combine_cancel_common(oldCD) {
        var spk = JSON.parse(JSON.stringify(oldCD.me));
        var bets = spk[3].slice(1);
        console.log("combine cancel common bets are ");
        var combine2 = combine_cancel_common2(bets, oldCD.ssme);
	var n = bets.length - combine2.bets.length;
	var m = n * 1000000;
        spk[3] = ([-6]).concat(combine2.bets.reverse());
	spk[8] = spk[8] + m;
        return {"sspk": keys.sign(spk), "ss": combine2.ss.reverse()};
    }
    function main(server_pubkey) {
        var oldCD = channels_object.read(server_pubkey);
        var canceled = combine_cancel_common(oldCD);
        var sspk = canceled.sspk;
        var ss = canceled.ss;
        variable_public_get(["combine_cancel_assets", keys.pub(), canceled.sspk], function(sspk2) {
            //verify that sspk2 is signed by them.
            if (JSON.stringify(sspk2[1]) == JSON.stringify(sspk[1])) {
                oldCD.them = sspk2;
                oldCD.me = sspk[1];
                oldCD.ssme = ss;
                oldCD.ssthem = ss;
                channels_object.write(server_pubkey, oldCD);
            } else {
                console.log(JSON.stringify(sspk[1]));
                console.log(JSON.stringify(sspk2[1]));
                throw("combine cancel spks do not match");
            }
        });
    }
    return {"main": main};
}
var combine_cancel_object = combine_cancel_assets_maker();
