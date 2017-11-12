wallet_doit1();
function wallet_doit1() {
    var button = document.createElement("input");
    button.type = "button";
    button.value = "test";
    button.id = "wallet_button_test";
    button.onclick = header_test;
    //button.onclick = test;
    document.body.appendChild(button);
    var db = {};
    function hash(input) {//array of bytes -- array of bytes
        var b = sjcl.codec.bytes.toBits(input);
        var x = sjcl.hash.sha256.hash(b);
        return sjcl.codec.bytes.fromBits(x);
    }
    function list_to_uint8(l) {
        var array = new Uint8Array(l.length);
        for (var i=0; i<l.length; i++) {
            a[i] = l[i];
        }
        return array;
    }
    function test() {
        console.log(hash([1,4,6,1,2,3,4,4]));
        //var array = new Uint8Array(4);
        var z = integer_to_array(1000, 4);
        var s = array_to_string(z);
        var a = atob("AAAD6A==");

        var g = string_to_array(a);
        var f = string_to_array(s);
        console.log(JSON.stringify(a));
        console.log(JSON.stringify(s));
        console.log(JSON.stringify(g));
        console.log(JSON.stringify(f));
        console.log(JSON.stringify(hash(g)));
        console.log(JSON.stringify(hash(f)));
    }
    function i2s(i, a) {
        return integer_to_array(i, a);
    }
    function string_to_array(x) {
        var a = new Uint8Array(x.length);
        for (var i=0; i<x.length; i++) {
            a[i] = x.charCodeAt(i);
        }
        return Array.from(a);
    }
    function array_to_string(x) {
        var a = "";
        for (var i=0; i<x.length ; i++) {
            a += String.fromCharCode(x[i]);
        }
        return a;
    }
    function integer_to_array(i, size) {
        var a = [];
        for ( var b = 0; b < size ; b++ ) {
            var c = a.length - b - 1;
            a.push(i % 256);
            i = Math.floor(i/256);
        }
        return a.reverse();
    }
    function header_list_to_binary(x) {
        //convert the header to binary form.

        //Array [ "header", 0, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA…", "vvbP//eI8ByxXmJKP7/0hN32AUaOPjY/xnC…", "oZlvyvtE4uKNxKmQYuIZqQTRib+b5qh0u8Q…", 0, 1, 6, 0, 0 ]
        var height = x[1]; //4 bytes
        var prev_hash = atob(x[2]); //bin
        var trees_hash = atob(x[3]); //bin
        var txs_proof_hash = atob(x[4]); //bin
        var time = x[5]; //4 bytes
        var difficulty = x[6]; // 3 bytes
        var version = x[7]; // 2 bytes
        var nonce = atob(x[8]); // 32 bytes
        //var accumulative_difficulty = x[9]; //don't include
        console.log("prev hash is");
        console.log(prev_hash);
        console.log("nonce is");
        console.log(nonce);
        var y = string_to_array(prev_hash);
        console.log("prev hash to array is");
        console.log(y);
        //var y = [];
        return y.concat(
            i2s(height, 4)).concat(
                i2s(time, 4)).concat(
                    i2s(version, 2).concat(
                        string_to_array(trees_hash).concat(
                            string_to_array(txs_proof_hash).concat(
                                i2s(difficulty, 2).concat(
                                    string_to_array(nonce))))));
    }
    function header_test() {
        variable_public_get(["headers", 1, 1], header_test2);
    }
    function header_test2(hl) {
        var header = hl[1];
        console.log(header);
        var d = header_list_to_binary(header);
        //console.log(d);
        console.log(JSON.stringify(d));
        var c = hash(d);
        console.log(c);
        //calculate hash of header
        //check pow on header is valid for difficulty on header.
        //check that the difficulty on header is high enough.
        //store the hash/header pair into the db.
    }
}

