//It is messy because each server has 2 ports. One for internal commands, and one for external. 
//The test is only between 2 nodes for now.
/*
function get2(t, callback) {
    u = url(PORT + 10, "localhost");
    return getter(t, u, callback);
}
function local_get2(t, callback) {
    u = url(PORT + 11, "localhost");
    return getter(t, u, callback);
}

function variable_get2(cmd, callback) {
    var x = local_get2(cmd);
    var_get(x, callback);
}
function variable_public_get2(cmd, callback) {
    var x = get2(cmd);
    var_get(x, callback);
}
*/
local_get(["new_pubkey", btoa("abc")]);

variable_get(["pubkey"], function(pubkey) {
    local_get2(["create_account", pubkey, 1000000, 50]);
    wait_for_id();
})
function wait_for_id() {
    local_get(["sync", [127,0,0,1], 3020]);
    console.log("wait for id");
    variable_get(["id"], new_channel);
}
function new_channel(id) {
    if (id == -1) {variable_get(["id"], new_channel);}
    else {
	console.log("new channel creator");
	console.log("new id ".concat(id));
	//the channel needs to be signed by both participants before it can be published.
	variable_get2(["create_channel", id, 112030, 0, btoa("delegated_1"), 50], function(ch) {
	    console.log("talking to 2");
	    console.log(ch);
	    variable_get(["sign", ch], function(ch2) {
		console.log(ch2);
		variable_get2(["sign", ch2], function(ch3) {
		    console.log(ch3);
		    get(["tx_absorb", ch3]);
		    get2(["tx_absorb", ch3]);
		    wait_for_channel_id();
		});
	    });
	});
    }
}

function wait_for_channel_id() {
    local_get(["sync", [127,0,0,1], 3020]);
    console.log("wait for channel id");
    variable_get(["channel_id", 0], function(chid) {
	console.log("channel id ".concat(chid));
	if (chid == [-6]) {wait_for_channel_id();}
	else {channel_spend(chid[1]);}
    });
}

function channel_spend(chid) {
    console.log("channel spend id ".concat(chid));
    variable_get(["channel_spend", chid, -10000], function(ch) {
	console.log("channel spend ".concat(ch));
	variable_public_get2(["channel_recieve", chid, -10001, ch], 
			     function(return_ch) {
	console.log("return_ch ".concat(return_ch));
	get(["channel_recieve", chid, 9999, return_ch]);
	hashlock(chid);
				 })
	});
}

function hashlock(chid) {
    //hash(1) == "qfPbi+pbLu+SN+VICd2kPwwhj4DQ0yijP1tM//8zSOY="
    console.log("hashlock");
    variable_get(
	["hashlock", chid, 1000, "+5mXXEquHXT+Xs6TOGU8lH/GbQbiH2+4IdQq0pe5EtA="],
	//"qfPbi+pbLu+SN+VICd2kPwwhj4DQ0yijP1tM//8zSOY="], 
	function(ch) {
	    console.log("hashlock 2 ".concat(ch));
	    variable_public_get2(
		["channel_locked_payment", chid, ch], 
		function(return_ch) {
		    console.log("return_ch ".concat(return_ch));
		    local_get(["spend_locked_payment", chid, return_ch]);
		    unlock(chid);
		})
	});
}
function unlock(chid) {
    console.log("unlock1");
    secret = "AQIDBA==";
    variable_public_get2(
	["unlock", chid, secret],
	function(ch) {
	    console.log("unlock2");
	    console.log(ch);
	    variable_public_get(
		["unlock2", chid, secret, ch],
		function(ch2) {
		    console.log("unlock3");
		    console.log(ch2);
		    get2(["unlock2", chid, secret, ch2]);
		});
	    });
}
