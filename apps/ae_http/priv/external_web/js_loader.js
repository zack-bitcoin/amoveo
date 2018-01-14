(function() {
    function load_many(l) {
	setTimeout(function(){
            var script = document.createElement("script");
            script.type = "text/javascript";
            script.src = "/".concat(l[0]).concat(".js");
            document.body.appendChild(script);
	}, 0);
        if (!(JSON.stringify(l.slice(1)) == "[]")) {
            load_many(l.slice(1));
        }
    };
    function load_consecutive(l, callback) {
        if (!(JSON.stringify(l) == "[]")) {
            var script = document.createElement("script");
            script.onload = function(){return load_consecutive(l.slice(1), callback)};
            script.type = "text/javascript";
            script.src = "/".concat(l[0]).concat(".js");
            document.body.appendChild(script);
        } else {
	    callback();
	}
    };
    load_consecutive(["sjcl", "elliptic.min", "format", "keys", "rpc"], function() {
	load_many([//"format",
	    "files", "codecBytes", "sha256", "crypto", "merkle_proofs", "encryption_library", "title", "server", //"rpc",
	    "headers", "miner", "signing", "spend_tx", "create_account_tx", "combine_cancel_assets", "market", "chalang", "spk", "bets", "lightning", "channels", "encryption"]);
    });
    
})();
