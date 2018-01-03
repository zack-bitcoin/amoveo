function miner_main() {
    var work_loop = 30000;//how many times to mine before checking if the user clicked a button.
    var mining_state = false; //set to false to stop mining.
    var blocks_found = 0;
    var button = document.createElement("input");
    button.type = "button";
    stop_mining();
    var div = document.createElement("div");
    div.id = "miner_div";
    document.body.appendChild(div);
    div.innerHTML = "0 ".concat(translate.words("blocks_found"));
    document.body.appendChild(button);
    function start_mining() {
        button.value = "stop mining";
        mining_state = true;
        mine();
        button.onclick = stop_mining;
    }
    function stop_mining() {
        button.value = translate.words("start_mining");
        mining_state = false;
        button.onclick = start_mining;
    }
    function miner_get(cmd, callback) {
        var u = url(get_port() + 5, get_ip());
        var v = getter(cmd, u);
        var_get(v, callback, cmd);
    }
    function increment_nonce(N) {
        for (var i = 0; i < N.length; i++) {
            if (N[i] == 256) {
                N[i] = 0;
            } else {
                N[i] += 1;
                return(N);
            }
        }
    };
    function mine_helper(d_hash, nonce, diff, times) {
        if (times < 1) { return(mine()); }
        if (mining_state == false) {
            console.log("stopped mining");
            return(0);
        }
        setTimeout(function() {
            for(var i = 0; i < work_loop; i++) {
                var d1 = diff % 256;
                var d2 = Math.floor( diff / 256) % 256;
                var y = d_hash.concat([d2, d1]).concat(nonce);
                var work = hash2integer(hash(y));
                if (work > diff) {
                    console.log("found a block");
                    console.log("=====================================================================");
                    blocks_found += 1;
                    div.innerHTML = (blocks_found).toString().concat(" blocks found.");
                    miner_get(["work", btoa(array_to_string(nonce)), keys.pub()],
                              function() {});
                    return(mine());
                }
                nonce = increment_nonce(nonce);
            }
            return mine_helper(d_hash, nonce, diff, times - work_loop);
        }, 0);
    }
    function random_bytes(N) {
        if (N == 0) {
            return [];
        }
        var r = Math.random();
        var r2 = Math.floor(r*1000000000000) % 256;
        return ([r2]).concat(random_bytes(N-1));
    }
    function mine() {
        miner_get(["mining_data"], function(x) {
            console.log("mining.");
            var d_hash = string_to_array(atob(x[1]));
            var d_diff = x[3];
            var d_nonce = random_bytes(32);
            mine_helper(d_hash, d_nonce, d_diff, 1000000);//after 1 million tries, it checks to see if the thing we are working on changed.
        });
    }
    return({"mine": mine, "mining_state": mining_state});
};

var miner_object = miner_main();

    
