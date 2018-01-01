function s2c(x) { return x / 100000000; }
function c2s(x) {
    return Math.floor(parseFloat(x.value, 10) * 100000000);
}
function array_to_int(l) {
    var x = 0;
    for (var i = 0; i < l.length; i++) {
        x = (256 * x) + l[i];
    }
    return x;
}
function toHex(str) {
    var hex = '';
    for(var i=0;i<str.length;i++) {
        l = str.charCodeAt(i).toString(16);
        var z = "";
        if (l.length < 2) { z = "0"; }
        hex += z;
	hex += ''+str.charCodeAt(i).toString(16);
    }
    return hex;
}
function fromHex(h) {
    var s = '';
    for(var i = 0; (2*i) < h.length;i++) {
        var m = h.slice((2*i), (2*(i+1)));
        var n = parseInt(m, 16);
        var l = String.fromCharCode(n);
        s = s.concat(l);
    }
    return s;
}
function string_to_array(x) {
    var a = new Uint8Array(x.length);
    for (var i=0; i<x.length; i++) {
        a[i] = x.charCodeAt(i);
    }
    return Array.from(a);
}
function integer_to_array(i, size) {
    var a = [];
    for ( var b = 0; b < size ; b++ ) {
        a.push(((i % 256) + 256) % 256);
        i = Math.floor(i/256);
    }
    return a.reverse();
}
function array_to_string(x) {
    var a = "";
    for (var i=0; i<x.length ; i++) {
        a += String.fromCharCode(x[i]);
    }
    return a;
}
function hash2integer(h) {
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
    return hash2integer2(h.concat([255]), 0, 0);
}
function button_maker(val, fun) {
    var button = document.createElement("input");
    button.type = "button";
    button.value = val;
    button.onclick = fun;
    return button;
}


