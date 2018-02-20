function s2c(x) {
    return x / 100000000;
}
function c2s(x) {
    return Math.floor(parseFloat(x.value, 10) * 100000000);
}
