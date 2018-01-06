function lightning_main() {
    function make(amount) {
        var s = Array.prototype.slice.call(elliptic.rand(32));//this encrytion library doesn't seem especially trustworthy. We are using it for generating addresses too.
        console.log("randomness is ");
        console.log(s);
        var sh = hash(s);
        var ss_code = ([2, 32]).concat(s);//binary size 32
        var ss = channels_object.new_ss(ss_code, [], []);
        var code = "FFoAAAAAADpGAAAAAGQAAAAAAQAAAAAAC0dIFBQoAAAAAAF5OhYUFhRGAAAAAAAAAAAAAgAAACcQRwAAAABkAAAAAAEAAAAAAEgL";
        var a = ([2, 32]).concat(
            sh).concat(
                string_to_array(atob(code)));
        var contract = btoa(a);
        var codekey = "";
        var meta = [-6];
        return {ss: ss, bet: ["bet", contract, amount, codekey, meta]};
    };
    function test() {
        var m = make(10);
        var c = m.bet.contract;
        var ss = m.ss;
        var cid = 1;
        var amount = 27000;
        var delay = 11;
        var spk = ["spk", 1, 2, [-6, m.bet], 10000,10000,cid, amount, 0, delay];
        var height = headers_object.top()[1];
        console.log("lightning spk run start");
        chalang_object.spk_run("fast", [ss], spk, height, 0, 1000, 1000, function(ran) {
            console.log("lightning spk run finished");
            console.log(JSON.stringify(ran));
        });
    };
    return {test: test, make: make};
}
var lightning_object = lightning_main();
JSON.stringify(lightning_object.test());
