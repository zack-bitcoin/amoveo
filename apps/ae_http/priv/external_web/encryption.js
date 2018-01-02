function encryption_main() {
    function bin_encrypt(key, bin) {
        var aesCtr = new aesjs.ModeOfOperation.ctr(key, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]);
        return aesCtr.encrypt(bin);
    };
    function bin_dec(key, bin) {
        var aesCtr = new aesjs.ModeOfOperation.ctr(key, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]);
        return aesCtr.decrypt(bin);
    };
    function test() {
        var key = hash([1]);
        console.log(key);//good. same as hash:doit(<<1>>).
        var textBytes = [1,2,3];
        var eb = bin_encrypt(key, textBytes);
        console.log(eb); // should be [100, 131, 24]
    }
    test();
    return {bin_encrypt: bin_encrypt, bin_dec: bin_dec};
}
