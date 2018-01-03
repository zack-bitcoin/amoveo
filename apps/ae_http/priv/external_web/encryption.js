function encryption_main() {
    function aes_ctr(key) {
        return(new aesjs.ModeOfOperation.ctr(key, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]));
    };
    function bin_enc(key,bin){
        return Array.from(aes_ctr(key).encrypt(bin));
    };
    function bin_dec(key, bin){
        return Array.from(aes_ctr(key).decrypt(bin));
    };
    function shared(key, pub) {
        return hex2array(key.derive(pub).toString(16));
    }
    function hex2array(x) {
        return string_to_array(fromHex(x));
    }
    function send(m, to_pub, fromkey) {
        var from_pub = btoa(fromHex(fromkey.getPublic("hex")));
        var newkey = keys.make();
        var eph_pub = hex2array(newkey.getPublic("hex"));
        var eph_priv = hex2array(newkey.getPrivate("hex"));
        var msg = ["msg", btoa(array_to_string(sign(eph_pub, fromkey))), m, from_pub];
        var ss = shared(newkey, to_pub);
        var emsg = bin_enc(ss, string_to_array(JSON.stringify(msg)));
        return ["emsg", btoa(array_to_string(eph_pub)), btoa(array_to_string(emsg))];
    };
    function get(emsg, my_key) {
        var eph_pub = string_to_array(atob(emsg[1]));
        eph_key = keys.ec().keyFromPublic(toHex(array_to_string(eph_pub)), 'hex').getPublic();
        var ss = shared(my_key, eph_key);
        var msg = JSON.parse(array_to_string(bin_dec(ss, string_to_array(atob(emsg[2])))));
        fromkey = keys.ec().keyFromPublic(toHex(atob(msg[3])), 'hex'); 
        var b = verify(eph_pub, btoa(msg[1]), fromkey);
        //eph_pub is the data that is signed.
        if (b) { return msg[2];
        } else { throw("encryption get error");
        }
    }
    function assert_eq(x, y) {
        if (!(JSON.stringify(x) == JSON.stringify(y))) {
            console.log("failed assert_eq");
            console.log(JSON.stringify(x));
            console.log(JSON.stringify(y));
            throw("failed assert_eq");
        };
    }
    function test() {
        var key = hash([1]);
        console.log(key);//same as hash:doit(<<1>>) from erlang.
        var textBytes = [1,2,3];
        var eb = bin_enc(key, textBytes);
        assert_eq(eb, [100, 131, 24]);
        assert_eq(bin_dec(key, eb), [1, 2, 3]);
        var fromKey = keys.make();
        var toKey = keys.make();
        var sm = send([1,2,3], toKey.getPublic(), fromKey);
        assert_eq(get(sm, toKey), [1, 2, 3]);
        var masterPub64 = "BLDdkEzI6L8qmIFcSdnH5pfNAjEU11S9pHXFzY4U0JMgfvIMnwMxDOA85t6DKArhzbPJ1QaNBFHO7nRguf3El3I=";
        var master = keys.ec().keyFromPublic(toHex(atob(masterPub64)), 'hex');
        console.log(JSON.stringify(send([1,2,3], master.getPublic(), fromKey)));
        console.log("encryption test passed.");
    }
    test();
    return {get: get, send: send};
}
var encryption_object = encryption_main();
