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
        //console.log("prove facts 2");
        //console.log(JSON.stringify(facts));
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
        if (!(ss.length == (spk[3].length - 1))) {//spk[3] == bets is formated with a -6 in front for packer.erl
            console.log(JSON.stringify(ss));
            console.log(JSON.stringify(spk));
            throw("ss and bets need to be the same length");
        }
        spk_run2(ss, spk[3], spk[5], spk[4], fun_limit, var_limit, state, spk[9], spk[8], 0, 1, function(ret) {
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
	console.log("spk run 3 ss is ");
	console.log(ss);
        var script_sig = ss.code;
        if (!(chalang_none_of(script_sig))) {
            throw("error: return op in the script sig");
        }
	//console.log("spk run3");
	//console.log(JSON.stringify(ss.prove));
        prove_facts(ss.prove, function(f) {
            var c = string_to_array(atob(bet[1]));
            //var c = bet.code;
	    //console.log("spk run3 f is ");
	    //console.log(f);
	    //console.log("and c is ");
	    //console.log(c);
            var code = f.concat(c);
            var data = chalang_object.data_maker(opgas, ramgas, vars, funs, script_sig, code, state);
            var data2 = chalang_object.run5(script_sig, data);
            var data3 = chalang_object.run5(code, data2);
            //console.log("just ran contract, stack returned as ");
            //console.log(JSON.stringify(data3.stack));
            //console.log("bet was ");
            //console.log(JSON.stringify(bet));
            var amount = data3.stack[0] | 0;//This should be a signed integer, but for some reason the integer is being stuck into a 32 byte unsigned value, so -2000 becomes 4294965296
            var nonce = data3.stack[1];
            var delay = data3.stack[2];
            var cgran = 10000; //constants.erl
	    console.log(amount);
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
	if (ssold[0] == -6) {
	    ssold = ssold.slice(1);
	}
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
                        console.log("force udpate final ss is ");
                        console.log(JSON.stringify(updated.newss));
                        console.log("force udpate final spk is ");
                        console.log(JSON.stringify(spk));
			console.log("updated is ");
			console.log(JSON.stringify(updated));
                        return callback({"spk":spk, "ss":updated.newss});
                    });
                } else {
		    console.log(JSON.stringify([nonceNew, nonceOld]));
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
	console.log("spk force update 2 compare bets and ss");
	console.log(JSON.stringify(ss));
	console.log(JSON.stringify(bets));
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
	    console.log("spk force update 22. code is ");
	    console.log(JSON.stringify(code));
            var data = chalang_object.data_maker(bet_gas_limit, bet_gas_limit, var_limit, fun_limit, ss[i-1].code, code, state);
            var data2 = chalang_object.run5(ss[i-1].code, data);
            var data3 = chalang_object.run5(code, data2);
            var s = data3.stack;
            var cgran = 10000; //constants.erl
	    console.log("ran code stack is ");
	    console.log(JSON.stringify(s));
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
            if (!(s[2] > 0)) { //if the delay is long, then don't close the trade.
		console.log("short delay, close the trade.");
                if (s[0] > cgran) {
                    throw("you can't spend money that you don't have");
                }
		console.log("update amount");
		console.log(JSON.stringify([s[0], bets[i][2], cgran]));
                amount += Math.floor(s[0] * bets[i][2] / cgran);
                nonce += s[1];
                new_bets = new_bets.slice(0, i).concat(new_bets.slice(i+1, new_bets.length));
                newss = newss.slice(0, i).concat(newss.slice(i+1, newss.length));
            } else {
		console.log("long delay, do not close the trade.");
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
		    var expiration = cd[7];
                    //console.log("are we able to force update?");
                    //console.log(JSON.stringify([b2, {"spk": newspk, "ss": ss}]));
                    if ( JSON.stringify(b2) == JSON.stringify({"spk": newspk, "ss": ss})) {
                        var ret = keys.sign(newspk);
                        var newcd = channels_object.new_cd(newspk, themspk, ss, ss, expiration, cid);
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
                                //var newcd = channels_object.new_cd(newspk, themspk, ss, ss, cid);
                                channels_object.write(from, newcd);
                                return callback(ret);
                            } else {
                                //console.log("channel feeder they simplify had nothing to do");
                                //return callback(false);
                                //this part is only used for lightning.
                                var sh=channel_feeder_simplify_helper(from, ss4);
                                var ss5 = sh.ss;
                                var ret = sh.spk;
                                var spk = themspk[1];
                                var spk2 = ret[1];
                                if (!( JSON.stringify(spk) == JSON.stringify(spk2))) {
				    console.log(JSON.stringify(spk));
				    console.log(JSON.stringify(spk2));
                                    console.log("spks do not match");
                                } else {
                                    var data = channels_object.new_cd(spk, themspk, ss5, ss5, expiration, cid);
                                    channels_object.write(from, data);
                                    return callback(ret);
                                }
                            }
                        });
                    }
                });
            });
        });
    }
    function channel_feeder_simplify_helper(from, ss) {
	var cd = channels_object.read(from);
	var spk = cd.me;//fix
	var bet_unlock_object = spk_bet_unlock(spk, ss);
	var ret = keys.sign(bet_unlock_object.spk);
	return {ss: bet_unlock_object.ss, spk: ret};
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
			console.log("the server sent us money.");
			return callback(true);
                    }
		    var many_new_bets = new_spk[3].length - old_bets.length;
                    if ((!(profit < 0)) && //costs nothing
                        (many_new_bets > 0)) { //increases number of bets
	                //if we have the same or greater amount of money, and they make a bet that possibly gives us more money, then accept it.
                        //var t = bets2.slice(1);
                        var t = [-6].concat(bets2.slice(1+many_new_bets));
                        if (!(JSON.stringify(t) ==
			      JSON.stringify(old_bets))) {
			    console.log("t is ");
			    console.log(JSON.stringify(t));
			    console.log("old bets");
			    console.log(JSON.stringify(old_bets));
                            console.log("update improperly formatted");
			    return callback(false);
                        }
			for (var i = 1; i < many_new_bets + 1; i++) {
                            var new_bet = bets2[i];
                            var betAmount = new_bet[2];
                            var potentialGain;
                            if (keys.pub() == acc1) {
				potentialGain = -betAmount;
                            } else if (keys.pub() == acc2) {
				potentialGain = betAmount;
                            } else {
				console.log("error, this spk isn't for your pubkey");
				return callback(false);
                            }
                            if (!(potentialGain > 0)) {
				console.log(potentialGain);
				console.log(betAmount);
				console.log(JSON.stringify(new_bet));
				console.log(JSON.stringify(bets2));
				console.log("error, this could make us lose money.");
				return callback(false);
                            }
			}
                        var obligations1 = spk_obligations(1, bets2);
                        var obligations2 = spk_obligations(2, bets2);
                        var channelbal1 = channel[4];
                        var channelbal2 = channel[5];
                        if (obligations1 > channelbal1) {
                            console.log("acc1 doesn't have enough money in the channel to make that bet");
			    return callback(false);
                        }
                        if (obligations2 > channelbal2) {
                            console.log("acc2 doesn't have enough money in the channel to make that bet");
			    return callback(false);
                        }
			console.log("successfully updated channel. They made a contract which costs nothing, and might give us money.");
			return callback(true);
                    }
		    console.log("this contract that the server offers might cost us something, so we refuse.");
		    return callback(false);
                });
            });
        });
    }
    function spk_obligations(n, bets) {
	if (n == 1) {
	    return spk_obligations1(bets);
	} else if (n == 2) {
	    return spk_obligations2(bets);
	}
    }
    function spk_obligations1(bets) {
	var c = 0;
	for (i = 1; i < bets.length; i++) {
	    var b = bets[i][2];
	    if (b > 0) { c += b; }
	}
	return c;
    }
    function spk_obligations2(bets) {
	var c = 0;
	for (i = 1; i < bets.length; i++) {
	    var b = bets[i][2];
	    if (b < 0) { c -= b; }
	}
	return c;
    }
    function api_decrypt_msgs(ms) {//list ms starts with -6
	console.log("msgs to decrypt");
	console.log(JSON.stringify(ms));
	for (var i = 1; i < ms.length; i++){
	    var emsg = ms[i];
	    console.log("about to decrypt this ");
	    console.log(JSON.stringify(emsg));
	    var dec = keys.decrypt(emsg);
	    var secret = dec[1];
	    var code = dec[2];
	    var amount = dec[3];
	    secrets_object.add(code, secret, amount);
	}
	return true;
    }
    function pull_channel_state(callback) {
        //get their pubkey
        variable_public_get(["pubkey"], function(server_pubkey) {
            variable_public_get(["spk", keys.pub()], function(spk_return) {
                var cd = spk_return[1];
                var them_spk = spk_return[2];
		//we need to verify that they signed them_spk.
                //returns cd and them_spk
                var cd0 = channels_object.read(server_pubkey);
		//console.log("javascript channels object is ");
		//console.log(JSON.stringify(cd0));
                if (cd0 == undefined) {
                    console.log("you don't have a record of a channel with this server. Did you load your channel data file?");
		    console.log("attempting to trustfully download a copy of the channel state from the server. Warning, this can be a security vulnerability!");
		    var spk = them_spk[1];
		    var ss = ss_to_internal(cd[4]);
		    var expiration = cd[7];
		    var cid = spk[6];
		    var NewCD = channels_object.new_cd(spk, them_spk, ss, ss, expiration, cid);
		    channels_object.write(server_pubkey, NewCD);
		    return callback();
                }
		console.log("cd0 is ");
		console.log(JSON.stringify(cd0));
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
                        api_decrypt_msgs(cd[5]);
                        api_bet_unlock(server_pubkey);
			callback();
                    } else {
			console.log("channel feeder they simplify failed.");
		    }
                });
            });
        });
    }
    function api_bet_unlock(server_pubkey, secret) {
	//The javascript version can be much simpler than the erlang version, because each secret is only for one smart contract for us. We don't have to search for other contracts that use it.

	var secrets = channel_feeder_bets_unlock(server_pubkey, secret);
	teach_secrets(secrets);
	variable_public_get(["spk", keys.pub()], function(spk_data) {
	    console.log("should start with -6");
	    console.log(JSON.stringify(spk_data));
	    var them_spk = spk_data[2];
	    return channel_feeder_update_to_me(them_spk, server_pubkey);
	});
    }
    function channel_feeder_bets_unlock(server_id, secret) {
        var cd = channels_object.read(server_id);
        /*
	  if (!(true == cd.live)) {
	    console.log(JSON.stringify(cd));
            console.log("this channel has been closed");
            throw("this channel was closed");
        }
	*/
	console.log("channel feeder bets unlock ");
	console.log(JSON.stringify(cd));
	    
        var unlock_object = spk_bet_unlock(cd.me, cd.ssme);
        cd.me = unlock_object.spk;
        cd.ssme = unlock_object.newss;
        cd.ssthem = unlock_object.ssthem;
	channels_object.write(server_id, cd);
        return {"secrets":unlock_object.secrets,
                "spk":unlock_object.spk};
	/*
    {ok, CD0} = channel_manager:read(ID),
    true = CD0#cd.live,
    SPKME = CD0#cd.me,
    SSOld = CD0#cd.ssme,
    {NewSS, SPK, Secrets, SSThem} = spk:bet_unlock(SPKME, SSOld),
    NewCD = CD0#cd{me = SPK, ssme = NewSS, ssthem = SSThem},
    channel_manager:write(ID, NewCD),
    Out = {Secrets, SPK},
	*/

    }
    function teach_secrets(Secrets) {
	//secrets is a dictionary code -> [secret, amount]
	// send ["secret", Secret, Key]
	//talker:talk({learn_secret, ID, Secret, Code}, IP, Port),
    }
    function channel_feeder_update_to_me(sspk, from) {
	var myid = keys.pub();
	var spk = sspk[1];
	var acc1 = spk[1];
	var acc2 = spk[2];
	if (!(((myid == acc1) && (from == acc2))
	      || ((myid == acc2) && (from == acc1))) ){
	    console.log(JSON.stringify(spk));
	    console.log(JSON.stringify(acc1));
	    console.log(JSON.stringify(acc2));
	    console.log(JSON.stringify(myid));
	    console.log(JSON.stringify(from));
	    console.log("channel_feeder_update_to_me has incorrect accounts in the spk.");
	    return false;
	}
	sspk2 = keys.sign(sspk);
	var b = verify_both(sspk2);
	if (!(b)) {
	    console.log("they didn't sign the spk");
	    return false;
	}
	cd = channels_object.read(from);
	if (!(JSON.stringify(cd.me) ==
	      JSON.stringify(sspk[1]))) {
	    console.log(JSON.stringify(cd.me));
	    console.log(JSON.stringify(sspk[1]));
	    console.log("can't update to me if they aren't the same.");
	    return false;
	}
	cd.them = sspk
	cd.ssthem = cd.ssme
	channels_object.write(from, cd);
    }
    function spk_bet_unlock(spk, ssold) {
	console.log("spk bet unlock spk is ");
	console.log(JSON.stringify(spk));
	console.log("spk bet unlock ssold is ");
	console.log(JSON.stringify(ssold));
	var bets = spk[3];//starts with -6
        var remaining = JSON.parse(JSON.stringify(bets));
        var amount_change = 0;
        var ssremaining = JSON.parse(JSON.stringify(ssold));
        var secrets = [];
        var dnonce = 0;
        var key;
        var ssthem = [];
        var f;
        for (var i = ssold.length - 1; i > -1; i--) {
	    var ss = ssold[i];
            key = bets[i+1].key;
	    var key_junk = secrets_object.read(key);
	    if (key_junk == undefined) {
		ssremaining = ([ss]).concat(ssremaining);
		ssthem = ([ss]).concat(ssremaining);
		var ss2 = key_junk[0];
		var amount = key_junk[1];
		//ssthem = ss[i];
		console.log("ssthem is ");
		console.log(JSON.stringify(ssthem));
		throw("working here");
		//look up fun limit and var limit and gas limit from config file.
		//verify none of in ssthem
		//f = spk_prove_facts(
            }
	}
        spk.bets = remaining;
        spk.amount += amount_change;
        spk.nonce += dnonce;
        return {"ssremaining": ssremaining,
                "spk": spk, //make sure to change spk in a few ways;
                "secrets": secrets,
                "ssthem": ssthem};
    }
    /*
Bets = SPK#spk.bets,
    {Remaining, AmountChange, SSRemaining, Secrets, Dnonce, SSThem} = bet_unlock2(Bets, [], 0, SS, [], [], 0, []),
    {lists:reverse(SSRemaining),
     SPK#spk{bets = lists:reverse(Remaining),
	     amount = SPK#spk.amount + (AmountChange),
	     nonce = SPK#spk.nonce + Dnonce},
     Secrets, SSThem}.
bet_unlock2([], B, A, [], SS, Secrets, Nonce, SSThem) ->
    {B, A, SS, Secrets, Nonce, lists:reverse(SSThem)};
bet_unlock2([Bet|T], B, A, [SS|SSIn], SSOut, Secrets, Nonce, SSThem) ->
    Key = Bet#bet.key, 
    case secrets:read(Key) of
	<<"none">> -> 
            io:fwrite("no secret known\n"),
	    bet_unlock2(T, [Bet|B], A, SSIn, [SS|SSOut], Secrets, Nonce, [SS|SSThem]);
	{SS2, Amount} -> 
	    %Just because a bet is removed doesn't mean all the money was transfered. We should calculate how much of the money was transfered.
            io:fwrite("we have a secret\n"),
            TP = tx_pool:get(),
            Trees = TP#tx_pool.block_trees,
            Height = TP#tx_pool.height,
	    State = chalang_state(Height, 0, Trees),
	    {ok, FunLimit} = application:get_env(ae_core, fun_limit),
	    {ok, VarLimit} = application:get_env(ae_core, var_limit),
	    {ok, BetGasLimit} = application:get_env(ae_core, bet_gas_limit),
	    true = chalang:none_of(SS2#ss.code),
	    F = prove_facts(SS#ss.prove, Trees),
	    C = Bet#bet.code,
	    Code = <<F/binary, C/binary>>,
	    Data = chalang:data_maker(BetGasLimit, BetGasLimit, VarLimit, FunLimit, SS2#ss.code, Code, State, constants:hash_size()),
	    Data2 = chalang:run5(SS2#ss.code, Data),
	    Data3 = chalang:run5(Code, Data2),
	    case Data3 of
		{error, _E} -> 
		    io:fwrite("spk bet unlock, ss doesn't work\n"),
		    io:fwrite(packer:pack(SS2)),
		    io:fwrite("\n"),
                    %io:fwrite("spk bet_unlock2 chalang run third\n"),
		    Data4 = chalang:run5(SS#ss.code, Data),
                    %io:fwrite("spk bet_unlock2 chalang run fourth\n"),
		    Y = chalang:run5(Code, Data4),
		    case Y of
			{error, E2} ->
			    io:fwrite("bet unlock2 ERROR"),
			    bet_unlock2(T, [Bet|B], A, SSIn, [SS|SSOut], Secrets, Nonce, [SS|SSThem]);
			Z -> 
			    bet_unlock3(Z, T, B, A, Bet, SSIn, SSOut, SS, Secrets, Nonce, SSThem)
		    end;
		X -> 
                    if
                        is_integer(Amount) ->
                            true = (abs(Amount) == abs(Bet#bet.amount));
                        true -> ok
                    end,
                    bet_unlock3(X, T, B, A, Bet, SSIn, SSOut, SS2, Secrets, Nonce, SSThem)
	    end
    end.
bet_unlock3(Data5, T, B, A, Bet, SSIn, SSOut, SS2, Secrets, Nonce, SSThem) ->
    io:fwrite("spk bet_unlock3\n"),
    [<<ContractAmount:32>>, <<Nonce2:32>>, <<Delay:32>>|_] = chalang:stack(Data5),
   if
        Delay > 0 ->
	   io:fwrite("delay is "),
	   io:fwrite(integer_to_list(Delay)),
	   io:fwrite("delay >0, keep the bet.\n"),
	   bet_unlock2(T, [Bet|B], A, SSIn, [SS2|SSOut], Secrets, Nonce, [SS2|SSThem]);
       true -> 
	   io:fwrite("delay <1, remove it.\n"),
	   CGran = constants:channel_granularity(),
	   true = ContractAmount =< CGran,
	   A3 = ContractAmount * Bet#bet.amount div CGran,
	   Key = Bet#bet.key, 
	   bet_unlock2(T, B, A+A3, SSIn, SSOut, [{secret, SS2, Key}|Secrets], Nonce + Nonce2, [SS2|SSThem])
   end.
    */
    return {pull_channel_state: pull_channel_state, spk_run: spk_run};
}


var spk_object = spk_main();
