(function() {
    function load_js(x, callback) {
        var script = document.createElement("script");
        script.onload = callback;
        script.src = x;
        document.body.appendChild(script);
    };
    function load_many(l) {
        if (JSON.stringify(l) == "[]") {
            return;
        } else {
            load_js(l[0], function() {
                return load_many(l.slice(1));
            });
        }
    };
    load_many(["/format.js", "/files.js", "/sjcl.js", "/codecBytes.js", "/sha256.js", "/crypto.js", "elliptic.min.js", "merkle_proofs.js", "/encryption_library.js", "/title.js", "/server.js", "/rpc.js", "/headers.js", "/miner.js", "/keys.js", "/signing.js", "/spend_tx.js", "/create_account_tx.js", "/combine_cancel_assets.js", "/market.js", "/chalang.js", "/bets.js", "/channels.js", "/encryption.js", "/lightning.js"]);
})();
