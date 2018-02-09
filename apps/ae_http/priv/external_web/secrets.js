var secrets = (function () {
    var db = {};
    function add(code, ss, amount){
	db[code] = [ss, amount];
    }
    function dump() {
	db = {};
    }
    return {add: add, dump: dump};
})();
