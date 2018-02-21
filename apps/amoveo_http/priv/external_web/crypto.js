function hash(input) {//array of bytes -- array of bytes
    var b = sjcl.codec.bytes.toBits(input);
    var x = sjcl.hash.sha256.hash(b);
    return sjcl.codec.bytes.fromBits(x);
}
