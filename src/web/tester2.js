function local_get2(t, callback) {
    u = url(PORT + 11, "localhost");
    return getter(t, u, callback);
}

local_get(["new_pubkey", btoa("abc")]);

variable_get(["pubkey"], function(pubkey) {
    local_get2(["create_account", pubkey, 2000000, 50]);
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
	console.log("new channel");
	console.log("id is ");
	console.log(id);
	local_get(["new_channel", [127,0,0,1], 3020, 1120000, 1100000, 50]);
	console.log("after new channel");
	variable_get(["channel_keys"], buy_block);
    }
}
function buy_block(keys) {
    if (keys == [-6]) {variable_get(["channel_keys"], channel_spend);}
    else {
	console.log("channel spend");
	console.log("keys ");
	console.log(keys);
	local_get2(["buy_block"]);
	setTimeout(sync, 100);
    }
}
function sync() {
    console.log("channel spend 2");
    local_get(["sync", [127,0,0,1], 3020]);
    //setTimeout(lightning_spend, 1000);
    setTimeout(function() {variable_get(["id"], message);}, 100);
    //setTimeout(message, 1000);
}
function message(id) {
    console.log("send message");
    local_get(["send_msg", [127,0,0,1], 3020, id, btoa("test message"), 6]);
    setTimeout(get_message, 100);
}
function get_message() {
    console.log("get message");
    local_get(["get_msg", [127,0,0,1], 3020]);
    setTimeout(function() {variable_get(["id"], read_message);}, 100);
}
function read_message(id) {
    console.log("read message");
    variable_get(["read_msg", id, 0], read_message2);
}
function read_message2(message) {
    console.log("read message 2");
    document.getElementById("main").innerHTML = atob(message);
}


function channel_spend() {
    //spends 100.
    local_get(["channel_spend", [127,0,0,1], 3020, 100]);
    
}
function lightning_spend() {
    var partner = 1;
    var amount = 200;
    console.log("channel spend 3");
    local_get(["lightning_spend", [127,0,0,1], 3020, partner, amount]);
    //message();
}
