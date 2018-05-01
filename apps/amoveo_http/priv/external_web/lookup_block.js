lookup_block1();
function lookup_block1() {
    var lookup_block_height = document.createElement("INPUT");
    lookup_block_height.setAttribute("type", "text"); 
    var input_info = document.createElement("h8");
    input_info.innerHTML = "block number: ";
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(input_info);
    document.body.appendChild(lookup_block_height);
    
    var lookup_block_button = document.createElement("BUTTON");
    lookup_block_button.id = "lookup block number";
    var lookup_block_button_text = document.createTextNode("lookup block");
    lookup_block_button.appendChild(lookup_block_button_text);
    lookup_block_button.onclick = function() {
	var num = parseInt(lookup_block_height.value, 10);
	//variable_public_get(["height"], function(x) {height_f(x)});
	//Lets maybe check to see if height is too height?
	variable_public_get(["block", num], function(x) {lookup_block2(x)});
    };
    document.body.appendChild(lookup_block_button);
    var current_block = document.createElement("div");
    current_block.id = "block div";
    document.body.appendChild(current_block);
}
function txs2html(txs, N, out) {//currently unused
    if (txs.length == N) { return out; }
    else { 
	return txs2html(txs, N+1, out.concat("- ").concat(tx2html(txs[N])));
    }
}
function tx2html(tx) { //currently unused
    var t = tx[1];
    if (t[0] == "sign_tx") {
	return "block was signed by account number ".concat(t[1]).concat(", using nonce number: ").concat(t[2]).concat("<br/>");
    }
    if (t[0] == "signed_cb") {
	return "closing a channel. this address ".concat(t[1]).concat(" used this nonce: ").concat(t[2]).concat(" and payed this fee: ").concat(t[4]).concat(" channel with this id: ").concat(t[3][1][6]).concat("<br/>");
    }
    if (t[0] == "timeout") {
	return "begin closing a channel without partner's participation. this account ".concat(t[1]).concat(" used this nonce: ").concat(t[2]).concat(" and payed this fee: ").concat(t[3]).concat(" channel with this id: ").concat(t[4][1][6]).concat(" the delay till it can be completely closed is: ").concat(t[4][1][8]).concat(" the amount of money being transfered by this channel is: ").concat(t[4][1][3]).concat("<br/>");
    }
    if (t[0] == "channel_close") {
	return "finish closing a channel without partner's participation. this account ".concat(t[1]).concat(" used this nonce ").concat(t[2]).concat(" to close the channel with this id: ").concat(t[3]).concat(" and paid this fee ").concat(t[4]).concat("<br/>");
    }
    if (t[0] == "channel_slash") {
	return "stop your partner from closing channel at wrong state. this account ".concat(t[1]).concat(" used this nonce ").concat(t[2]).concat(" to close the channel with this id: ").concat(t[3][1][6]).concat(" and paid this fee ").concat(t[4]).concat("<br/>");
    }
    if (t[0] == "create_account_tx") {
	return "create a new account. Created by ".concat(t[1]).concat(" used this nonce: ").concat(t[2]).concat(" with initial balance: ").concat(t[4]).concat(" paying this fee ").concat(t[5]).concat(" with this pubkey: ").concat(t[3]).concat("<br/>");
    }
    if (t[0] == "spend") {
	return "spend money. account number ".concat(t[1]).concat(" spends. They used this nonce ").concat(t[2]).concat(" to spend to ").concat(t[3]).concat(". They spent this much: ").concat(t[4]).concat(" and they paid this fee: ").concat(t[5]).concat("<br/>");
    }
    if (t[0] == "slasher_tx") {
	return "punish an account for double-signing. punisher is ".concat(t[1]).concat(" used nonce ").concat(t[2]).concat(" punished this account: ").concat(t[3][1][1]).concat("<br/>");
    }
    if ((t[0] == "tc") && (t[8] == -1)) {
	return "create a new channel. account ".concat(t[1]).concat(" now has ").concat(t[4]).concat(" in the channel, and account ").concat(t[2]).concat(" has ").concat(t[5]).concat(" in the channel.").concat("<br/>");
    }
    if (t[0] == "tc") {
	return "add money to channel number: ".concat(t[8]).concat(". account ").concat(t[1]).concat(" now has ").concat(t[4]).concat(" in the channel, and account ").concat(t[2]).concat(" now has ").concat(t[5]).concat(" in the channel. in total we added ").concat(t[9]).concat(" to the channel<br/>");
    }
    if (t[0] == "fork_slash_tx") {
	return "account number ".concat(t[1]).concat(" gets punished for signing on a fork. ").concat("They used nonce ").concat(t[2]).concat(" punished this account: ").concat(t[3][1][1]).concat(", punished the signature from block number ").concat(t[4]).concat("<br/>");
	}
    else {
	console.log(t);
	return t.concat("<br/>");
    }
}
function to_now(t) {
    var start_time = 15192951759;
    var n = (t + start_time);//10 * seconds since jan 1 1970
    var curdate = new Date(null);
    curdate.setTime(n*100);
    var final_now = curdate.toLocaleString();
    console.log(final_now);
    return final_now;
}
function lookup_block2(block) {
    block2 = block[1];
    console.log("lookup_block2");
    var current_block = document.getElementById("block div");
    //console.log(JSON.stringify(block));
    //console.log(JSON.stringify(block[10]));
    //console.log(JSON.stringify(block[10][1][1]));
    if (block == "empty") {
	current_block.innerHTML = "this block doesn't exist.";
    } else {
	var miner = block[10][1][1];
	var time0 = block[4];
	var time = to_now(time0);
	//acc, number, hash, txs, power, nonce, total_coins, db_root
	current_block.innerHTML = "block: ".concat(block[1]).concat(", was mined by: ").concat(miner).concat(" , has timestamp: ").concat(time);
	//current_block.innerHTML = "<br/>created by account number ".concat(block2[1]).concat("<br/>at height: ").concat(block2[2]).concat("<br/>containing transactions: ").concat(txs2html(block2[4], 1, "<br/>"));
	//console.log(block[1]);
    }
}
