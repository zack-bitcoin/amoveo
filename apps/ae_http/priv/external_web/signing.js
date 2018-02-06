
//https://github.com/indutny/elliptic/blob/master/test/ecdsa-test.js

//var key = ec.genKeyPair();
//var ec = new elliptic.ec('secp256k1');

//var msg = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
//var signature = key.sign(msg);


function serialize(data) {
    if (Number.isInteger(data)) {
        //console.log("serialize integer");
        //<<3:8, X:512>>;
        var x = integer_to_array(3, 1).concat(
            integer_to_array(data, 64));
        return x;
    } else if (Array.isArray(data)) {
        if (data[0] == -6) { //its a list.
            //console.log("serialize array");
            //<<1:8, S:32, A/binary>>;
            var d0 = data.slice(1);
            var rest = serialize_list(d0);
            return integer_to_array(1, 1).concat(
                integer_to_array(rest.length, 4)).concat(
                    rest);

        } else if (data[0] == -7) { //it is a tuple
            //console.log("serialize tuple 1");
            //<<2:8, S:32, A/binary>>;
            var d0 = data.slice(1);
            var rest = serialize_list(d0);
            return integer_to_array(2, 1).concat(
                integer_to_array(rest.length, 4)).concat(
                    rest);
        } else if (typeof(data[0]) == "string") { //assume it is a record. a tuple where the first element is an atom. This is the only place that atoms can occur.
            //console.log("serialize tuple 2");
            var h = data[0];
            var d0 = data.slice(1);
            //<<4:8, S:32, A/binary>>;
            var atom_size = h.length;
            var first = integer_to_array(4, 1).concat(
                integer_to_array(atom_size, 4)).concat(
                    string_to_array(h));
            //console.log(JSON.stringify(first));
            var rest = first.concat(serialize_list(d0));
            return integer_to_array(2, 1).concat(
                integer_to_array(rest.length, 4)).concat(
                    rest);
        }
    }
    //assume it is a binary
    //console.log("serialize binary");
    //<<0:8, S:32, X/binary>>;
    if (typeof(data) == "string") {
        var rest = string_to_array(atob(data));
        return integer_to_array(0, 1).concat(
            integer_to_array(rest.length, 4)).concat(
                rest);
    } else {
        return integer_to_array(0, 1).concat(
            integer_to_array(data.length, 4)).concat(
                data);
    }
    function serialize_list(l) {
        var m = [];
        for (var i = 0; i < l.length; i++) {
            m = m.concat(serialize(l[i]));
        }
        return m;
    }
}
function sign(data, key) {
    //ecdsa, sha356
    var d2 = serialize(data);
    var h = hash(d2);
    var sig = key.sign(h);
    return sig.toDER();
}
function verify(data, sig0, key) {
    var sig = bin2rs(atob(sig0));
    var d2 = serialize(data);
    var h = hash(d2);
    return key.verify(h, sig, "hex");
}
function verify1(tx) {
    return verify(tx[1], tx[2], keys.ec().keyFromPublic(toHex(atob(tx[1][1])), "hex"));
}
function verify2(tx) {
    return verify(tx[1], tx[3], keys.ec().keyFromPublic(toHex(atob(tx[1][2])), "hex"));
}
function verify_both(tx) {
    return (verify1(tx) && verify2(tx));
}
function bin2rs(x) {
    /*
      0x30 b1 0x02 b2 (vr) 0x02 b3 (vs)
      where:
      
      b1 is a single byte value, equal to the length, in bytes, of the remaining list of bytes (from the first 0x02 to the end of the encoding);
      b2 is a single byte value, equal to the length, in bytes, of (vr);
      b3 is a single byte value, equal to the length, in bytes, of (vs);
      (vr) is the signed big-endian encoding of the value "r", of minimal length;
      (vs) is the signed big-endian encoding of the value "s", of minimal length.
    */
    var h = toHex(x);
    var a2 = x.charCodeAt(3);
    var r = h.slice(8, 8+(a2*2));
    var s = h.slice(12+(a2*2));
    return {"r": r, "s": s};
}


//signing_test();
function signing_test() {

    //priv1 = atob("2kYbRu2TECMJzZy55fxdILBvM5wJM482lKLTRu2e42U=");
    //var key1 = keys.ec().genKeyPair({entropy: priv1});
    //var sig1 = sign([-6, 1], key1);
    //console.log(verify([-6, 1], sig1, key1));

    var stx = ["signed",["nc","BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4=","BCPuHeZ7sRjKAlnJjEqz3JOgc+OX/M0hRhA6IcQp+/KYSHrbP6wT+ei5VPzPaabU6eS3AE+4DbcgQj/eMaGRglQ=",50050,2,29999,29998,100,1],"MEUCIHc0anEc4ujgGkN5h8dUgoyCPFZ7dW5kh2MjCFn2O6NeAiEAtIg83JJLtk13i3jqgPdio8EE1lcQPBhy/9HWNKy3x7w=","MEQCIAXKdfeRxhtUHTx602gEqFA7xic+48La3Ju1pR43ZxWXAiAiaLPpTK5JoJ6sj3BltNm4pofrWN3r2XOGksA17XVKyg=="];
    //var stx = ["signed",["create_acc_tx","BHuqX6EKohvveqkcbyGgE247jQ5O0i2YKO27Yx50cXd+8J/dCVTnMz8QWUUS9L5oGWUx5CPtseeHddZcygmGVaM=",1,20,"BJh+CRhyKiDRSJfjUFMwUVdC/3+Ahj644HWxbLzlddhggWg+2c+h1/i9u8ono9v3l7Vb4E5WSEZouDUUH2XDI58=",150000000000],"TUVVQ0lRRDRVUjVwV1M4bWM2U1dvK2EzWDY3WlBrRnk4Mlg3cW9qNkxXTTFaUzJ1MGdJZ2JGTmlkWFdYNDJ0V2dEcUZ5aUo4NnhqWnVTMlZKNGwxTGJvcjdWeFVXckU9",[-6]];

    var data0 = stx[1];
    var sig0 = stx[2];
    var key0 = keys.ec().keyFromPublic(toHex(atob(stx[1][1])), "hex");

    var foo = verify(data0, sig0, key0);
    console.log(foo);
    console.log(verify1(stx));
}

//setTimeout(signing_test2(), 1000);
//signing_test2();
function signing_test2() {
    //var d = ["record", [-6, 4], [-7, 8000], -50];
    var d = ["record", "BAr8BCYGo1WwDoB1KXU7xvdqRetLJbyEyRgT7NyBggkYUVW5oalfek1imezEb00Ww+61aiXNrkkBC8EEKsGjumw=", [-6, ["a", -2000]]];
    console.log("signing test");
    console.log(JSON.stringify(serialize(d)));
    var stx = keys.sign(d);
    var key0 = keys.ec().keyFromPublic(toHex(atob(keys.pub())), "hex");
    var b = verify(stx[1], stx[2], key0);
    console.log(b);
}
//signing_test3();
function signing_test3() {
    //ingredients
    var k = keys.make();
    var data = [];
    //signing 
    var d = hash(serialize(data));
    var sig2 = btoa(array_to_string(k.sign(d).toDER()));
    //verifying
    var sig3 = bin2rs(atob(sig2));
    var b = k.verify(d, sig3, "hex");
    console.log(b);
}
//signing_test4();
function signing_test4() {
    var k = keys.make();
    var data = [];
    var sig = btoa(array_to_string(sign(data, k)));
    console.log(verify(data, sig, k));
}
