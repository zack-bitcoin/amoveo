function spk_main() {
    const ops = chalang_object.ops();
    function prove_facts(facts, callback) {
        if (JSON.stringify(facts) == JSON.stringify([])) {
            return callback([ops.empty_list]);
        }
        return prove_facts2(facts, 1, [ops.empty_list], callback); // [
    }
    var tree2id = {accounts: 1, channels: 2, existence: 3, burn: 4, oracles: 5, governance: 6};
    function prove_facts2(facts, i, r, callback) {
	var ops = chalang_object.ops();
        if (i == facts.length) {
            r.concat([ops.reverse]); // converts a , to a ]
            return callback(r);
        }
        console.log("prove facts 2");
        console.log(JSON.stringify(facts));
        var tree = facts[i][0];
        var key = facts[i][1];
	//var key = hash(string_to_array(atob(facts[i][1])));
        merkle.request_proof(tree, key, function(value) {
            //var value = merkle.verify(key, proof);
            //we are making chalang like this:
            //[ int id, key, binary size serialized_data ]
            // '[', ']', and ',' are macros for making a list.
            var id = tree2id[tree];
            r = r.concat([ops.empty_list]); // [
            r = r.concat([0]).concat(integer_to_array(id, 4));
            r = r.concat([ops.swap, ops.cons]); // ,
            if (Number.isInteger(key)) {
                r = r.concat([0]);
                r = r.concat(integer_to_array(key, 4));
            } else {
		key = string_to_array(atob(key));
                r = r.concat([2]);
                r = r.concat(integer_to_array(key.length, 4));
                r = r.concat(key);
            }
            r = r.concat([ops.swap, ops.cons]); // ,
            var serialized_data = merkle.serialize(value, key);//this is the serialized version of the thing who's existence we are proving. make it from value.
            var s = serialized_data.length;
            r = r.concat([2]).concat(integer_to_array(s, 4));
            r = r.concat(serialized_data);
            r = r.concat([ops.swap, ops.cons, ops.reverse]); // ]
            r = r.concat([ops.swap, ops.cons]); // ,
            return prove_facts2(facts, i+1, r, callback);
        });
        //return r.concat([ops.reverse]); // converts a , to a ]
    }
    function spk_run(mode, ss0, spk0, height, slash, fun_limit, var_limit, callback) {//mode unused
        var spk = JSON.parse(JSON.stringify(spk0));
        var ss = JSON.parse(JSON.stringify(ss0));
        var state = chalang_object.new_state(height, slash);
        //var key1 = "fun_limit";
        var ret;
        if (!(ss.length == (spk[3].length - 1))) {//spk[4] == bets is formated with a -6 in front for packer.erl
            //console.log(JSON.stringify(ss));
            //console.log(JSON.stringify(spk));
            //console.log(JSON.stringify(spk[4]));
            throw("ss and bets need to be the same length");
        }
        spk_run2(ss, spk[3], spk[5], spk[4], fun_limit, var_limit, state, spk[9], 0, 0, 1, function(ret) {
            return callback(ret);
        });
    }
    function spk_run2(ss, bets, opgas, ramgas, funs, vars, state, delay, nonce, amount, i, callback) {
        if (i > (ss.length)) {
            return callback({"amount": amount, "nonce": nonce, "delay": delay});//, "opgas": opgas});
        }
        spk_run3(ss[i-1], bets[i], opgas, ramgas, funs, vars, state, function(run_object) {
            if (!(Number.isInteger(run_object.nonce))) {
                console.log(JSON.stringify(run_object.nonce));
                throw("nonce should be an integer");
            }
            return spk_run2(ss, bets, opgas, ramgas, funs, vars, state,
                            Math.max(delay, run_object.delay),
                            nonce + run_object.nonce,
                            amount + run_object.amount,
                            i+1, callback);
        });
    }
    function spk_run3(ss, bet, opgas, ramgas, funs, vars, state, callback) {
        var script_sig = ss.code;
        if (!(chalang_none_of(script_sig))) {
            throw("error: return op in the script sig");
        }
	console.log("spk run3");
	console.log(JSON.stringify(ss.prove));
        prove_facts(ss.prove, function(f) {
            var c = string_to_array(atob(bet[1]));
            //var c = bet.code;
	    console.log("spk run3 f is ");
	    console.log(f);
	    console.log("and c is ");
	    console.log(c);
            var code = f.concat(c);
            var data = chalang_object.data_maker(opgas, ramgas, vars, funs, script_sig, code, state);
            var data2 = chalang_object.run5(script_sig, data);
            var data3 = chalang_object.run5(code, data2);
            //console.log("just ran contract, stack returned as ");
            //console.log(JSON.stringify(data3.stack));
            //console.log("bet was ");
            //console.log(JSON.stringify(bet));
            var amount = data3.stack[0];
            var nonce = data3.stack[1];
            var delay = data3.stack[2];
            var cgran = 10000; //constants.erl
            if ((amount > cgran) || (amount < -cgran)) {
                throw("you can't spend money you don't have in the channel.");
            }
            //var a3 = Math.floor(amount * bet.amount / cgran);
            var a3 = Math.floor(amount * bet[2] / cgran);
            return callback({"amount": a3, "nonce": nonce, "delay": delay, "opgas": data3.opgas});
        });
    }
    function spk_force_update(spk0, ssold0, ssnew0, fun_limit, var_limit, callback) {
        var spk = JSON.parse(JSON.stringify(spk0));
        var ssold = JSON.parse(JSON.stringify(ssold0));
        var ssnew = JSON.parse(JSON.stringify(ssnew0));
        console.log("force update");
        var height = headers_object.top()[1];
        var ret;
        spk_run("fast", ssold, spk, height, 0, fun_limit, var_limit, function(ran1) {
            var nonceOld = ran1.nonce;
            spk_run("fast", ssnew, spk, height, 0, fun_limit, var_limit, function(ran2) {
                var nonceNew = ran2.nonce;
                if (nonceNew > nonceOld) {
                    spk_force_update2(spk[3], ssnew, height, function(updated) {
                        spk[3] = updated.new_bets;
                        spk[7] += updated.amount;
                        spk[8] += updated.nonce;
                        //console.log("force udpate final ss is ");
                        //console.log(JSON.stringify(updated.newss));
                        return callback({"spk":spk, "ss":updated.newss});
                    });
                } else {
                    console.log("spk force update had nothing to do.");
                    return callback(false);
                }
            });
        });
    }
    function chalang_none_of(c) {
        console.log("none of");
        var n;
        for (var i = 0; i < c.length; i++) {
            if ( c[i] == ops.finish ) {
                return false;
            } else if ( c[i] == ops.int_op ) {
                i += 4
            } else if ( c[i] == ops.binary_op ) {
                n = array_to_int(c.slice(i+1, i+5));
                i += (4 + n);
            }
        }
        return true;
    }
    function spk_force_update2(bets, ss, height, callback) {
        var amount = 0;
        var nonce = 0;
        var new_bets = JSON.parse(JSON.stringify(bets));
        var newss = JSON.parse(JSON.stringify(ss));
        var fun_limit = 1000;//config
        var var_limit = 10000;
        var bet_gas_limit = 100000;
        var cgran = 10000; //constants.erl
        spk_force_update22(bets, ss, height, amount, nonce, new_bets, newss, fun_limit, var_limit, bet_gas_limit, bets.length-1, callback);
    }
    function spk_force_update22(bets, ss, height, amount, nonce, new_bets, newss, fun_limit, var_limit, bet_gas_limit, i, callback) {
        //console.log("spke force update 22");
        if (i < 1) {
            return callback({"new_bets": new_bets, "newss": newss, "amount": amount, "nonce": nonce});
        }
        var b = chalang_none_of(ss[i-1].code);//ss.code
        if (!(b)) {
            throw("you can't put return op into the ss");
        }
        var state = chalang_object.new_state(height, 0);
        prove_facts(ss[i-1].prove, function(f) { //PROBLEM HERE
            //var code = f.concat(bets[i].code);
            var code = f.concat(string_to_array(atob(bets[i][1])));
            var data = chalang_object.data_maker(bet_gas_limit, bet_gas_limit, var_limit, fun_limit, ss[i-1].code, code, state);
            var data2 = chalang_object.run5(ss[i-1].code, data);
            var data3 = chalang_object.run5(code, data2);
            var s = data3.stack;
            var cgran = 10000; //constants.erl
            /*
console.log(JSON.stringify([
                //"code", code,
                "ss", ss[i-1],
                "data2", data2.stack,
                "data3", data3.stack,
                "delay", s[2],
                "amount", s[0],
                "nonce", s[1]]));
*/
            if (!(s[2] > 50)) { //if the delay is long, then don't close the trade.
                if (s[0] > cgran) {
                    throw("you can't spend money that you don't have");
                }
                amount += Math.floor(s[0] * bets[i] / cgran);
                nonce += s[1];
                new_bets = new_bets.slice(0, i).concat(new_bets.slice(i+1, new_bets.length));
                newss = newss.slice(0, i).concat(newss.slice(i+1, newss.length));
            }
            return spk_force_update22(bets, ss, height, amount, nonce, new_bets, newss, fun_limit, var_limit, bet_gas_limit, i-1, callback); 
        });
    }
    function tree_number_to_value(t) {
        if (t < 101) {
            return t;
        } else {
            var top = 101;
            var bottom = 100;
            var x = tree_number_det_power(10000, top, bottom, t);
            return Math.floor(x / 100);
        }
    }
    function tree_number_det_power(base, top, bottom, t) {
        if (t == 1) {
            return Math.floor((base * top) / bottom);
        }
        var r = Math.floor(t % 2);
        if (r == 1) {
            var base2 = Math.floor((base * top) / bottom);
            return tree_number_det_power(base2, top, bottom, t-1);
        } else if (r == 0) {
            var top2 = Math.floor((top * top)  / bottom);
            return tree_number_det_power(base, top2, bottom,
                                         Math.floor(t / 2));
        }
    }
    function ss_to_internal(ess) {
        var ss = [];
        for (var i = 1; i < ess.length; i++) {
            ss = ss.concat([channels_object.new_ss(string_to_array(atob(ess[i][1])), ess[i][2], ess[i][3])])
        }
        return ss;
    }
    function channel_feeder_they_simplify(from, themspk, cd, callback) {
        cd0 = channels_object.read(from);
        //true = cd0.live; //verify this is true
        //true = cd.live; //verify this is true
        var spkme = cd0.me;
        var ssme = cd0.ssme;
        //console.log("ssme is ");
        //console.log(JSON.stringify(ssme));
        //verify that they signed themspk
        var newspk = themspk[1];
        //console.log("spkme is ");
        var newspk2 = cd[1];
        if (!(JSON.stringify(newspk) == JSON.stringify(newspk2))) {
            console.log(JSON.stringify(newspk));
            console.log(JSON.stringify(newspk2));
            throw("spks they gave us do not match");
        }
        var ss = ss_to_internal(cd[3]);
        var ss4 = ss_to_internal(cd[4]);
        merkle.request_proof("governance", 14, function(tree_fun_limit) {
            var fun_limit = tree_number_to_value(tree_fun_limit[2]);
            merkle.request_proof("governance", 15, function(tree_var_limit) {
                var var_limit = tree_number_to_value(tree_var_limit[2]);
                spk_force_update(spkme, ssme, ss4, fun_limit, var_limit, function(b2) {
                    var cid = cd[7];
                    console.log("are we able to force update?");
                    console.log(JSON.stringify([b2, {"spk": newspk, "ss": ss}]));
                    if ( JSON.stringify(b2) == JSON.stringify({"spk": newspk, "ss": ss})) {
                        var ret = keys.sign(newspk);
                        var newcd = channels_object.new_cd(newspk, themspk, ss, ss, cid);
                        channels_object.write(from, newcd);
			ss4_text = document.createElement("h8");
			ss4_text.innerHTML = JSON.stringify(ss4);
			document.body.append(ss4_text);
			//append ss4 to the document somewhere.
                        return callback(ret);
                    } else {
                        is_improvement(spkme, ssme, newspk, ss, fun_limit, var_limit, function(b3) {//maybe ss should be ss4?
                            if ( b3 ) {
                                //If they give free stuff, then accept.
                                ret = keys.sign(newspk);
                                var newcd = channels_object.new_cd(newspk, themspk, ss, ss, cid);
                                channels_object.write(from, newcd);
                                return callback(ret);
                            } else {
                                console.log("channel feeder they simplify had nothing to do");
                                return callback(false);
                                //this part is only used for lightning.
                                /*
                                  var sh=channel_feeder_simplify_helper(From, ss4);
                                  var ss5 = sh.ss;
                                  var ret = sh.ret;
                                  var spk = themspk[1];
                                  var spk2 = ret[1];
                                  if (!( JSON.stringify(spk) == JSON.stringify(spk2))) {
                                  console.log("spks do not match");
                                  } else {
                                  var data = channels_object.new_cd(spk, themspk, ss5, ss5, cid);
                                  channels_object.write(from, data);
                                  return ret;
                                  }
                                */
                            }
                        });
                    }
                });
            });
        });
    }
    function is_improvement(old_spk, old_ss, new_spk, new_ss, fun_limit, var_limit, callback) {
        //get height
        //check that space gas and time limit are below or equal to what is in the config file.
	var height = headers_object.top()[1];
        if (new_spk[4] > 100000) {//space gas
            console.log("this contract uses too much space.");
            return callback(false);
        }
        if (new_spk[5] > 100000) {//time gas
            console.log("this contract uses too much time");
            return callback(false);
        }
        spk_run("fast", new_ss, new_spk, height, 0, fun_limit, var_limit, function(run2) {
            var nonce2 = run2.nonce;
            var delay2 = run2.delay;
            spk_run("fast", old_ss, old_spk, height, 0, fun_limit, var_limit, function(run1) {
                var nonce1 = run1.nonce;
                if (!(nonce2 > nonce1)) {
                    console.log(JSON.stringify([new_ss, old_ss]));
                    console.log(JSON.stringify([nonce2, nonce1]));
                    console.log("the new spk can't produce a lower nonce than the old.");
                    return callback(false);
                }
                var old_bets = old_spk[3];
                var old_amount = old_spk[7];
                old_spk[3] = new_spk[3];
                old_spk[5] = new_spk[5];//time gas tg;
                old_spk[4] = new_spk[4];//space gassg;
                old_spk[7] = new_spk[7];
                old_spk[8] = new_spk[8];
                if (!(JSON.stringify(old_spk) == JSON.stringify(new_spk))) {
                    console.log("spk was changed in unexpected ways");
		    console.log(JSON.stringify(old_spk));
		    console.log(JSON.stringify(new_spk));
                    return callback(false);
                }
                var cid = new_spk[6];
                var ret = false;
                merkle.request_proof("channels", cid, function(channel) {
                    //variable_public_get(["proof", btoa("channels"), cid, btoa(array_to_string(top_hash))], function(proof) {
                    //var channel = merkle.verify(cid, proof);
                    var acc1 = channel[2]
                    var acc2 = channel[3]
                    var profit;
                    if (keys.pub() == acc1) {
                        profit = new_spk[7] - old_amount;
                    } else {
                        profit = old_amount - new_spk[7];
                    }
                    var bets2 = new_spk[3];
                    if ((JSON.stringify(old_bets) == JSON.stringify(bets2)) && (profit > 0)) {
                        //if they give us money for no reason, then accept.
                        ret = true;
                        return 0;
                    }
                    if ((!(profit < 0)) && //costs nothing
                        ((new_spk[3].length - old_bets.length) > 0)) { //increases number of bets
	                //if we have the same or greater amount of money, and they make a bet that possibly gives us more money, then accept it.
                        var new_bet = bets2[0];
                        var t = bets2.slice(1, bets2.length);
                        if (!(JSON.stringify(t) == old_bets)) {
                            console.log("we can only absorb one bet at a time this way.");
                            ret = false;
                            return 0;
                        }
                        var betAmount = new_bet.amount;
                        var potentialGain;
                        if (keys.pub() == acc1) {
                            potentialGain = -betAmount;
                        } else if (keys.pub() == acc2) {
                            potentialGain = betAmount;
                        } else {
                            console.log("error, this spk isn't for your pubkey")
                            ret = false;
                            return 0;
                        }
                        if (!(potentialGain > 0)) {
                            ret = false;
                            return 0;
                        }
                        var obligations1 = spk_obligations(1, bets2);
                        var obligations2 = spk_obligations(2, bets2);
                        var channelbal1 = channel[4];
                        var channelbal2 = channel[5];
                        if (obligations1 > channelbal1) {
                            console.log("acc1 doesn't have enough money in the channel to make that bet");
                            ret = false;
                            return 0;
                        }
                        if (obligations2 > channelbal2) {
                            console.log("acc2 doesn't have enough money in the channel to make that bet");
                            ret = false;
                            return 0;
                        }
                        ret = true;
                        return 0;
                    }
                    ret = false;
                    return 0;
                });
                return callback(ret);
            });
        });
    }
    function spk_obligations(n, bets) {
            var x = 0;
            for (var i = 0; i < n; i++) {
            var b = bets[i].amount;
            if (b > 0) {
                if (b > 0) {
                    x += b;
                }
            } else if (n == 2) {
                if (b < 0) {
                    x -= b;
                }
            } else {
                throw("spk_obligations error");
            }
        }
        return x;
    }
    function api_decrypt_msgs(l) {
        for (var i = 0; i < l.length; i++) {
            var d = encryption_object.get(l[i]);
            console.log(JSON.stringify(d));
            throw("api decrypt msgs");
            lightning_object.add(d.code, d.secret, d.amount);
        }
    }
    function api_bet_unlock(pubkey) {
        var bu = channel_feeder_bets_unlock(pubkey);
        var msg = ["learn_secret", pubkey_64(), secret, code];
        variable_public_get(msg, function () {} );
        variable_public_get([spk, pubkey_64()], function (x) {
            console.log("api bet unlock x is ");
            console.log(JSON.stringify(x));
            throw("api bet unlock");
            var cd = x[1];
            var spk = x[2];
            channel_feeder:update_to_me(spk, pubkey);
        } );
        
    }
    function pull_channel_state(callback) {
        //get their pubkey
        variable_public_get(["pubkey"], function(server_pubkey) {
            variable_public_get(["spk", keys.pub()], function(spk_return) {
                var cd = spk_return[1];
                var them_spk = spk_return[2];
                //returns cd and them_spk
                var cd0 = channels_object.read(server_pubkey);
		//console.log("javascript channels object is ");
		//console.log(JSON.stringify(cd0));
                if (cd0 == undefined) {
                    console.log("you don't have a record of a channel with this server. Did you load your channel data file?");
		    console.log("attempting to trustfully download a copy of the channel state from the server. Warning, this can be a security vulnerability!");
		    var spk = them_spk[1];
		    var ss = cd[4];
		    var expiration = cd[7];
		    var cid = spk[6];
		    var NewCD = channels_object.new_cd(spk, them_spk, ss, ss, expiration, cid);
		    channels_object.write(server_pubkey, NewCD);
		    return callback();
                }
		/*
                  if (!(cd0.live == true)) {
                    var s = "this channel has been closed";
                    console.log(s);
                    throw(s);
                }
		*/
                channel_feeder_they_simplify(server_pubkey, them_spk, cd, function(ret) {
                    if (!(ret == false)) {
                        var msg2 = ["channel_sync", keys.pub(), ret];
                        variable_public_get(msg2, function(foo) {});
                        api_decrypt_msgs(cd.emsg);
                        api_bet_unlock();
                        //throw("working here");
                    } else {
			console.log("channel feeder they simplify failed.");
		    }
                });
            });
        });
    }
    return {pull_channel_state: pull_channel_state, spk_run: spk_run};
}

var spk_object = spk_main();
