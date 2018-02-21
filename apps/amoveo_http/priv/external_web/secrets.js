var secrets = (function () {
    var db = {};
    function add(code, ss, amount){
	db[code] = [ss, amount];
    }
    function dump() {
	db = {};
    }
    function read() {
	return db[code];
    }
    return {add: add, dump: dump, read: read};
})();
