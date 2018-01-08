(function() {
    function load_many(l) {
        if (!(JSON.stringify(l) == "[]")) {
            var script = document.createElement("script");
            script.onload = function(){return load_many(l.slice(1))};
            script.type = "text/javascript";
            script.src = "/".concat(l[0]).concat(".js");
            document.body.appendChild(script);
        }
    };
    load_many(["format", "files", "sjcl", "codecBytes", "sha256", "crypto", "elliptic.min", "merkle_proofs", "encryption_library", "title", "server", "rpc", "headers", "miner", "keys", "signing", "spend_tx", "create_account_tx", "combine_cancel_assets", "market", "chalang", "spk", "bets", "lightning", "channels", "encryption"]);
})();
