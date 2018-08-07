(function() {
    var A = bigInt(100);
    var D = bigInt(6);//can also be 5.
    var B = A.times(D);
    var C = B.toJSNumber();
    console.log(C);
})();
