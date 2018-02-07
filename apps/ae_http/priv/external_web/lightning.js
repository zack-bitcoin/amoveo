function lightning_main() {
    var secrets = {};
    function read(code) {
        return secrets[code];
    }
    function add(code, ss, amount) {
        //code should be base64 encoded?
        secrets[code] = [ss, amount];
    }

    function make(amount) {
        var s = Array.prototype.slice.call(elliptic.rand(32));
        var sh = hash(s);
        var ss_code = ([2, 0,0,0,32]).concat(s);
        var ss = channels_object.new_ss(ss_code, [], []);
        var code = [20,90,0,0,0,0,0,58,
                    70,
                      0,0,0,0,100,0,0,0,0,1,0,0,0,0,0,11,
                    71,72,20,20,
                    40,31,
                    58,22,20,22,20,
                    70,0,0,0,0,0,0,0,0,0,2,0,0,0,39,16,
                    71,0,0,0,0,100,0,0,0,0,1,0,0,0,0,0,
                    72,11];
        var a = ([2, 0,0,0,32]).concat(
            sh).concat([30]).concat(
                code);
        var contract = btoa(array_to_string(a));
        var codekey = "";
        var meta = [-6];
        return {ss: ss, bet: ["bet", contract, amount, codekey, meta]};
    };
    function test() {
        var m = make(10);
        var ss = m.ss;
        var cid = 1;
        var amount = 27000;
        var delay = 11;
        var spk = ["spk", 1, 2, [-6, m.bet], 10000,10000,cid, amount, 0, delay];
        var height = headers_object.top()[1];
        spk_object.spk_run("fast", [ss], spk, height, 0, 1000, 1000, function(ran) {
            console.log(JSON.stringify(ran));
        });
    };
    return {test: test, make: make, read: read, add: add};
}
var lightning_object = lightning_main();
//setTimeout(function() {
//    JSON.stringify(lightning_object.test());
//}, 500);
