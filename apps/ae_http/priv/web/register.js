//register_doit();
local_get(["sync", get_ip(), get_port()]);
variable_get(["channel_keys"], function(x) {register_doit(x)});
function register_doit(x) {
    if (typeof x == 'undefined'){
	setTimeout(function() {variable_get(["channel_keys"], register_doit);}, 1000);
    } else if ( ( x.length == 1 ) && ( x.pop() == -6 ) ) {
	//variable_get(["id"], new_channel);
        new_channel();
    } else {
	console.log("did not work, x was");
	console.log(x);
    }
}
function new_channel() {
    //if (id == -1) {variable_get(["id"], new_channel);}
    //else {
	variable_get(["balance"], function(x) {new_channel2(id,x);})
    //}
}
function new_channel2(bal) {
    //C = Math.min(Math.floor(bal/2), 1000000);
    bal2 = bal - 1;
    local_get(["new_channel_with_server", bal, bal2, delay]);
}
