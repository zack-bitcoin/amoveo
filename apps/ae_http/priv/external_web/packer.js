function json2erlb(x) {
    return Erl.bufferToArray(Erl.encode(x));
}
function erlb2json(x) {
    return Erl.decode(Erl.toArrayBuffer(x));
}

