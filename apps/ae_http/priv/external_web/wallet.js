console.log("wallet 0");
wallet_doit1();
function wallet_doit1() {
    console.log("wallet 1");
    var button = document.createElement("input");
    button.type = "button";
    button.value = "test";
    button.id = "wallet_button_test";
    button.onclick = header_test;
    document.body.appendChild(button);
    console.log("wallet 2");
    var db = {};
    function hash(a) {
        var x = sjcl.hash.sha256.hash(a);
        return sjcl.codec.hex.fromBits(x);
    }
    function header_list_to_binary(x) {
        //convert the header to binary form.

      //var array = new Uint8Array100;

      //(H#header.prev_hash)/binary, 256
      //(H#header.height):HtB, height_bits
      //(H#header.time):TB, time_bits
      //(H#header.version):VB, version_bits
      //(H#header.trees_hash)/binary, 256
      //(H#header.txs_proof_hash)/binary, 256
      //(H#header.difficulty):DB, 15
      //(H#header.nonce):HB 256
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

