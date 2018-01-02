function encryption_main() {
    function aes_ctr(key) {
        return(new aesjs.ModeOfOperation.ctr(key, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]));
    };
    function bin_enc(key, bin) {
        return aes_ctr(key).encrypt(bin);
    };
    function bin_dec(key, bin) {
        return aes_ctr(key).decrypt(bin);
    };
    function shared(key, pub) {
        return string_to_array(fromHex(key.derive(pub).toString(16)));
    }
    function send_msg(m, to_pub, fromkey) {
        var from_pub = btoa(fromHex(fromkey.getPublic("hex")));
        var newkey = new_keys();
        var eph_pub = string_to_array(fromHex(newkey.getPublic("hex")));
        var eph_priv = string_to_array(fromHex(newkey.getPrivate("hex")));
        var msg = ["msg", sign(eph_pub, fromkey), m, from_pub];
        var ss = shared(newkey, to_pub);
        var emsg = bin_enc(ss, string_to_array(JSON.stringify(msg)));
        return ["emsg", btoa(array_to_string(eph_pub)), btoa(array_to_string(emsg))];
    };
    function get_msg(emsg, my_key) {
        var eph_pub = atob(emsg[1]);
        eph_key = ec.keyFromPublic(toHex(eph_pub), 'hex').getPublic();
        var ss = shared(my_key, eph_key);
        var msg = JSON.parse(array_to_string(bin_dec(ss, string_to_array(atob(emsg[2])))));
        fromkey = ec.keyFromPublic(toHex(atob(msg[3])), 'hex').getPublic(); 
        console.log("verify");
        console.log(msg);
        console.log(eph_pub);
        console.log(string_to_array(eph_pub));
        console.log(msg[1]);
        console.log(fromkey);
        var b = verify(string_to_array(eph_pub), msg[1], fromkey);//data sig id
        if (b) {
            return msg[2];
        } else {
            throw("encryption get_msg error");
        }
    }
    function test() {
        var key = hash([1]);
        console.log(key);//good. same as hash:doit(<<1>>).
        var textBytes = [1,2,3];
        var eb = bin_enc(key, textBytes);
        console.log(eb); // good. [100, 131, 24]
        var fromKey = new_keys();
        var toKey = new_keys();
        var sm = send_msg([1,2,3],
                          toKey.getPublic(),
                          //toKey,
                          fromKey);
        return get_msg(sm, toKey);
    }
    test();
    return {bin_enc: bin_enc, bin_dec: bin_dec};
}
//var encryption_object = encryption_main();
