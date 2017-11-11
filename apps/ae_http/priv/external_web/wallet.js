wallet_doit1();
function wallet_doit1() {
    var button = document.createElement("input");
    button.type = "button";
    button.value = "test";
    button.id = "wallet_button_test";
    button.onclick = test;
    document.body.appendChild(button);
    var db = {};
    function hash(a) {
        var x = sjcl.hash.sha256.hash(a);
        return sjcl.codec.hex.fromBits(x);
    }
    function test() {
        var array = new Uint8Array(4);
        //var z = array_to_string(array);
        var z = integer_into_array(1000, array);
        console.log(z);
    }
    function array_to_string(x) {
        a = "";
        for (var i=0; i<x.length ; i++) {
            a += String.fromCharCode(x[i]);
        }
        return a;
    }
    function integer_into_array(i, a) {
        for ( var b = 0; b < a.length ; b++ ) {
            var c = a.length - b - 1;
            a[c] = i % 256;
            i = Math.floor(i/256);
        }
        return a;
    }
    function header_list_to_binary(x) {
        //convert the header to binary form.

        //var array = new Uint8Array100;

        //Array [ "header", 0, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA…", "vvbP//eI8ByxXmJKP7/0hN32AUaOPjY/xnC…", "oZlvyvtE4uKNxKmQYuIZqQTRib+b5qh0u8Q…", 0, 1, 6, 0, 0 ]

        //height, prev_hash, trees_hash, txs_proof_hash, time, difficulty, version, nonce, accumulative_difficulty
        var height = x[1]; //4 bytes
        var prev_hash = x[2]; //bin
        var trees_hash = x[3]; //bin
        var txs_proof_hash = x[4]; //bin
        var time = x[5]; //4 bytes
        var difficulty = x[6]; // 3 bytes
        var version = x[7]; // 2 bytes
        var nonce = x[8]; // 32 bytes
        var accumulative_difficulty = x[9]; 

      //(H#header.prev_hash)/binary, 256
      //(H#header.height):HtB, height_bits
      //(H#header.time):TB, time_bits
      //(H#header.version):VB, version_bits
      //(H#header.trees_hash)/binary, 256
      //(H#header.txs_proof_hash)/binary, 256
      //(H#header.difficulty):DB, 15
        //(H#header.nonce):HB 256
        console.log(x);
        return 0;
    }
    function header_test() {
        variable_public_get(["headers", 1, 0], header_test2);
    }
    function header_test2(hl) {
        var header = hl[1];
        console.log(header);
        var b = header_list_to_binary(header);
        //calculate hash of header
        //check pow on header is valid for difficulty on header.
        //check that the difficulty on header is high enough.
        //store the hash/header pair into the db.
    }
    function binary_hash_test() {
        x = atob("AQID"),
        console.log(hash(x));
        //hash of <<1, 2, 3>>
        //from erlang <<3,144,88,198,242,192,203,73,44,83,59,10,77,20,239,
        //from js 039058c6f2c0cb492c533b0a4d14ef77cc0f78a...
    }
}

