var top_header = 0;//stores the valid header with the most accumulated work.

function array_to_string(x) {
    var a = "";
    for (var i=0; i<x.length ; i++) {
        a += String.fromCharCode(x[i]);
    }
    return a;
}
function serialize_header(x) {
    //Array [ "header", 0, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA…", "vvbP//eI8ByxXmJKP7/0hN32AUaOPjY/xnC…", "oZlvyvtE4uKNxKmQYuIZqQTRib+b5qh0u8Q…", 0, 1, 6, 0, 0 ]
    var height = x[1]; //4 bytes
    var prev_hash = atob(x[2]); //bin
    var trees_hash = atob(x[3]); //bin
    var txs_proof_hash = atob(x[4]); //bin
    var time = x[5]; //4 bytes
    var difficulty = x[6]; // 3 bytes
    var version = x[7]; // 2 bytes
    var nonce = atob(x[8]); // 32 bytes
    var period = x[10];
    //var accumulative_difficulty = x[9]; //don't include
    var y = string_to_array(prev_hash);
    return y.concat(
        integer_to_array(height, 4)).concat(
            integer_to_array(time, 4)).concat(
                integer_to_array(version, 2)).concat(
                    string_to_array(trees_hash)).concat(
                        string_to_array(txs_proof_hash)).concat(
                            integer_to_array(difficulty, 2)).concat(
                                string_to_array(nonce)).concat(
                                    integer_to_array(period, 2));
}

function hash2integer(h) {
    return hash2integer2(h.concat([255]), 0, 0);
}
function hash2integer2(h, i, n) {
    var x = h[i];
    if  ( x == 0 ) {
        return hash2integer2(h, i+1, n+(256*8));
    } else {
        return n + hash2integer3(x, h[i+1]);
    }
}
function dec2bin(dec){
    n = (dec).toString(2);
    n="00000000".substr(n.length)+n;
    return n;
}
function hash2integer3(byte1, byte2) {
    var x = dec2bin(byte1).concat(dec2bin(byte2));
    return hash2integer4(x, 0, 0);
}
function hash2integer4(binary, i, n) {
    var x = binary[i];
    if ( x == "0" ) { return hash2integer4(binary, i+1, n+256) }
    else {
        var b2 = binary.slice(i, i+8);
        var y = hash2integer5(b2) + n;
        return y;
    }
}
function hash2integer5(bin) {
    var x = 0;
    for (var i=0; i < bin.length; i++) {
        var y = bin[i];
        if ( y == "0" ) { x = x * 2; }
        else { x = 1 + (x * 2) }
    }
    return x;
}

wallet_doit1();
function wallet_doit1() {
    var retarget_frequency = 2000;
    var headers_db = {};//store valid headers by hash
    var top_diff = 0;//accumulative difficulty of top
    var button = document.createElement("input");
    button.type = "button";
    button.value = "get more headers";
    button.id = "wallet_button_test";
    button.onclick = more_headers;
    //button.onclick = test;
    document.body.appendChild(button);
    wallet_text = document.createElement("p");
    wallet_text.innerHTML = JSON.stringify({"height": 0, "total_work": 0});
    document.body.appendChild(wallet_text);
    function write_header(header) {
        console.log("write header");
        var acc_difficulty = header[9];
        console.log("acc_difficulty");
        console.log(acc_difficulty);
        console.log(top_diff);
        if (acc_difficulty > top_diff) {
            top_diff = acc_difficulty;
            top_header = header;
            wallet_text.innerHTML = JSON.stringify({"height": header[1], "total_work":(Math.floor(header[9]/10000000000000))});
        }
        h = hash(serialize_header(header));
        headers_db[h] = header;
    }
    function list_to_uint8(l) {
        var array = new Uint8Array(l.length);
        for (var i=0; i<l.length; i++) {
            a[i] = l[i];
        }
        return array;
    }
    function pair2sci(x, b) {
        return (256 * x) + b;
    }


        //calculate X. ad 1 for every zero bit starting from the beginning of the h. Stop as soon as you reach a non-zero bit.
        // calculate B. take the next 8 bits from h after calculating x, and interpret it as an integer.
        //return pair2sci(X, B);
    function difficulty_should_be(hash) {
        var header = headers_db[hash];
        if ( header == undefined ) {
            console.log(headers_db);
            console.log(hash);
            console.log("received an orphan header");
            return "unknown parent";
        } else {
            var Diff = header[6];
            var RF = retarget_frequency; //constants:retarget_frequency();
            var height = header[1];
            var x = height % RF;
            //console.log("x height");
            //console.log(JSON.stringify([x, height]));
            if ( ( x == 0 ) && (! (height < 10) )) {
                return difficulty_should_be2(header);
            } else { return Diff; }
        }
    }
    function retarget(header, n) {
        return retarget2(header, n, []);
    }
    function retarget2(header, n, ts) {
        var t = header[5];
        ts.push(t);
        var height = header[1];
        if ((height == 0) || (n == 0)) {
            return {"header":header, "times":ts};
        }
        else {
            var prev_hash = string_to_array(atob(header[2]));
            var prev_header = headers_db[prev_hash];
            return retarget2(prev_header, n-1, ts);
        }
    }
    function median(l) {
        l.sort(function(a, b) {return a - b;});
        var half = Math.floor(l.length / 2);
        return l[half];
    }
    function difficulty_should_be2(header) {
        console.log("difficulty_should_be2");
        var period = header[10];
        var f = Math.floor(retarget_frequency / 2); //constants:retarget frequencey is 2000
        var a1 = retarget(header, f, []);
        var times1 = a1.times;
        var header2000 = a1.header;
        var a2 = retarget(header2000, f, []);
        var times2 = a2.times;
        var m1 = median(times1);
        var m2 = median(times2);
        console.log("medians");
        console.log(m1);
        console.log(m2);
        var tbig = m1 - m2;
        console.log("tbig");
        console.log(tbig);
        var t = Math.floor(tbig / f);
        console.log("t");
        console.log(t);
        var nt = pow_recalculate(header2000[6],//old difficulty
                                 //6000,//goal block time()
                                 period,
                                 Math.max(1, t));//current estimated block time
        console.log("nt");
        console.log(nt);
        var done = Math.max(nt, 6452);
        return done;//initial difficulty
        
    }
    function pow_recalculate(oldDiff, t, bottom) {
        var old = sci2int(oldDiff);
        var n = Math.max(1, Math.floor(( old * t ) / bottom));
        var d = int2sci(n);
        return Math.max(1, d);
    }
    function log2(x) {
        if (x == 1) { return 1; }
        else { return 1 + log2(Math.floor(x / 2))}
    }
    function exponent(a, b) {
        if (b == 0) { return 1; }
        else if (b == 1) { return a; }
        else if ((b % 2) == 0) {return exponent(a*a, Math.floor(b / 2)); }
        else {return a*exponent(a, b-1); }
    }
    function sci2int(x) {
        function pair2int(l) {
            var b = l.pop();
            var a = l.pop();
            var c = exponent(2, a);
            return Math.floor((c * (256 + b)) / 256);
        }
        function sci2pair(i) {
            var a = Math.floor(i / 256);
            var b = i % 256;
            return [a, b];
        }
        return pair2int(sci2pair(x));
    }
    function int2sci(x) {
        function pair2sci(l) {
            var b = l.pop();
            var a = l.pop();
            return (256 * a) + b;
        }
        function int2pair(x) {
            var a = log2(x) - 1;
            var c = exponent(2, a);
            var b = Math.floor((x * 256) / c) - 256;
            return [a, b];
        }
        return pair2sci(int2pair(x));
    }
    function check_pow(header) {
        //calculate Data, a serialized version of this header where the nonce is 0.
        var height = header[1];
        console.log("height is ");
        console.log(height);
        if (height < 1) { return true; }
        else {
            var prev_hash = string_to_array(atob(header[2]));
            var diff0 = difficulty_should_be(prev_hash);
            var diff = header[6];
            console.log("diff0, diff1");
            console.log([diff0, diff]);
            if (diff == diff0) {
                var nonce = atob(header[8]);
                var data = JSON.parse(JSON.stringify(header));
                data[8] = btoa(array_to_string(integer_to_array(0, 32)));
                var s1 = serialize_header(data);
                var h1 = hash(hash(s1));
                var foo = h1.concat(
                    integer_to_array(diff, 2)).concat(
                        string_to_array(nonce));
                var h2 = hash(foo);
                var I = hash2integer(h2);
                return I > diff;
            } else {
                console.log("bad diff");
                console.log(diff);
                console.log(diff0);
                return false;
                
            }
        }
    }
    function absorb_headers(h) {
        var get_more = false;
        for (var i = 1; i < h.length; i++ ) {
            console.log("absorb header 1");
            var b =check_pow(h[i]);
            if ( b ) {
                console.log("absorb header 2");
                var header = h[i];
                var height = header[1];
                if ( height == 0 ) {
                    header[9] = 0;//accumulative difficulty
                } else {
                    var prev_hash = string_to_array(atob(header[2]));
                    var prev_header = headers_db[prev_hash];
                    prev_ac = prev_header[9];
                    diff = header[6];
                    var ac = sci2int(diff);
                    header[9] = prev_ac + ac - 1;
                }
                console.log("absorb header 3");
                var header_hash = hash(serialize_header(header));
                if (!(header_hash in headers_db)) {
                    get_more = true;
                }
                write_header(header);}
            else {
                console.log("bad header");
                console.log(h[i]); }
        }
        if (get_more) {
            more_headers();
        }
    }
    function more_headers() {
        var n;
        if ( top_header == 0 ) {
            n = 0;
        } else {
            n = top_header[1];
        }
        //variable_public_get(["headers", 101, n], absorb_headers);
        variable_public_get(["headers", 101, n], absorb_headers);
    }
    function hash_test() {
        console.log(hash([1,4,6,1,2,3,4,4]));
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
    function header_test() {
        variable_public_get(["headers", 10, 0], header_test2);
    }
    function header_test2(hl) {
        console.log(hl);
        absorb_headers(hl);
    }
    //test();
    function test() {
        console.log(sci2int(2000));//should be 232
        console.log(int2sci(2000));//should be 2804
        console.log(sci2int(int2sci(2000)));// should be 2000

    }
}

