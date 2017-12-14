

function miner_main() {

    function miner_get(cmd, callback) {
        //var u = url(get_port() + 5, get_ip());
        var u = url(get_port() + 5, "localhost");
        var v = getter(cmd, u);
        var_get(v, callback, cmd);
    }
    function test() {
        miner_get(["mining_data"], function(x) {
            console.log(x);
            var d_hash = string_to_array(atob(x[1]));
            var d_nonce = string_to_array(atob(x[2]));
            var d_diff = x[3];
            console.log(JSON.stringify([d_hash, d_nonce, d_diff]));
        });
    }
    return({"test": test});
};

var miner_object = miner_main();
miner_object.test();

    
