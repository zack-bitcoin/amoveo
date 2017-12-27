function s2c(x) {
    return x / 100000000;
}
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
