(function() {
    function load_many(l) {
        //if (!(JSON.stringify(l) == "[]")) {
	setTimeout(function(){//new
            var script = document.createElement("script");
            //script.onload = function(){return load_many(l.slice(1))};
            script.type = "text/javascript";
            script.src = "/".concat(l[0]).concat(".js");
            document.body.appendChild(script);
	}, 0);//new
        if (!(JSON.stringify(l.slice(1)) == "[]")) {//new
            load_many(l.slice(1));//new
        }
    };
    function load_consecutive(l) {
        if (!(JSON.stringify(l) == "[]")) {
            var script = document.createElement("script");
            script.onload = function(){return load_consecutive(l.slice(1))};
            script.type = "text/javascript";
            script.src = "/".concat(l[0]).concat(".js");
            document.body.appendChild(script);
        }
    };
    
    load_consecutive(["sjcl", "elliptic.min", "keys"]);
    load_many(["format", "files", //"sjcl",
	       "codecBytes", "sha256", "crypto", //"elliptic.min",
	       "merkle_proofs", "encryption_library", "title", "server", "rpc", "headers", "miner", //"keys",
	       "signing", "spend_tx", "create_account_tx", "combine_cancel_assets", "market", "chalang", "spk", "bets", "lightning", "channels", "encryption"]);
})();
