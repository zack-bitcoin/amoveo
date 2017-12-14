

function miner_main() {

    var work_loop = 30000;//how many times to mine before checking if the user clicked a button.
    var mining_state = false; //set to false to stop mining.
    function miner_get(cmd, callback) {
        //var u = url(get_port() + 5, get_ip());
        var u = url(get_port() + 5, "localhost");
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
    function mine(d_hash, nonce, diff, times) {
        //maybe every few thousand times should be grouped using a time-sleep-async thing. That way the interface will still work.
        console.log("big mining loop");
        if (times < 1) {
            console.log("failed to find a block");
            return(test());
        }
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
                    miner_get(["work", btoa(array_to_string(nonce)), pubkey_64()],
                              function() {});
                    return(0);
                }
                nonce = increment_nonce(nonce);
            }
            return mine(d_hash, nonce, diff, times - work_loop);
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
    function test() {
        miner_get(["mining_data"], function(x) {
            console.log(x);
            var d_hash = string_to_array(atob(x[1]));
            //var d_nonce = string_to_array(atob(x[2]));
            var d_diff = x[3];
            //console.log(JSON.stringify([d_hash, d_nonce, d_diff]));
            var d_nonce = random_bytes(32);
            mine(d_hash, d_nonce, d_diff, 1000000);
        });
    }
    return({"test": test, "mine": mine, "mining_state": mining_state});
};

var miner_object = miner_main();
//miner_object.test();

    
