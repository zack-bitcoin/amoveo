
function keys_function1() {
    var ec = new elliptic.ec('secp256k1');
    var keys = new_keys();
    var account_title = document.createElement("h3");
    account_title.innerHTML = "account ";
    var div = document.createElement("div");
    var save_name = document.createElement("input");
    save_name.type = "text";
    save_name.value = "Amoveo private key";
    var save_button = button_maker2("save private key to file", save_keys);
    var file_selector = document.createElement("input");
    file_selector.type = "file";
    file_selector.onchange = load_keys;
    var load_text = document.createTextNode("get key from file");
    var watch_only_instructions = document.createTextNode("put your pubkey here to make a watch-only wallet that is unable to spend money.");
    var watch_only_pubkey = document.createElement("input");
    watch_only_pubkey.type = "text";
    var watch_only_button = button_maker2("load pubkey", watch_only_func); 
    var pub_div = document.createElement("div");
    var new_pubkey_button = button_maker2("generate new keys", new_keys_check);
    var new_pubkey_div = document.createElement("div");
    var balance_button = button_maker2("check balance ", update_balance);
    var bal_div = document.createElement("div");
    document.body.appendChild(account_title);
    document.body.appendChild(div);

    append_children(div, [load_text, file_selector, br(), pub_div, br(), save_name, save_button, br(), watch_only_instructions, watch_only_pubkey, watch_only_button, br(), new_pubkey_button, new_pubkey_div, br(), bal_div, balance_button]);

    update_pubkey();
    function input_maker(val) {
        var x = document.createElement("input");
        x.type = "text";
        x.value = val;
        return x;
    }
    function new_keys_watch(x) {
	return ec.keyFromPublic(x);
    }
    function new_keys_entropy(x) {
        return ec.genKeyPair({entropy: hash(serialize([x]))});
    }
    function new_keys() {
        return ec.genKeyPair();
    }
    function pubkey_64() {
        var pubPoint = keys.getPublic("hex");
        return btoa(fromHex(pubPoint));
    }
    function sign_tx(tx) {
	if (tx[0] == "signed") {
	    console.log(JSON.stringify(tx));
	    var sig = btoa(array_to_string(sign(tx[1], keys)));
	    var pub = pubkey_64();
	    if (pub == tx[1][1]) {
		tx[2] = sig;
	    } else if (pub == tx[1][2]) {
		tx[3] = sig;
	    } else {
		console.log(JSON.stringify(tx));
		throw("sign error");
	    }
	    return tx;
	} else {
            var sig = btoa(array_to_string(sign(tx, keys)));
            return ["signed", tx, sig, [-6]];
	}
    }
    function update_pubkey() {
        pub_div.innerHTML = ("your pubkey ").concat(pubkey_64());
    }
    function watch_only_func() {
	var v = watch_only_pubkey.value;
	keys = new_keys_watch(string_to_array(atob(v)));
	update_pubkey();
    }
    function new_keys_check() {
        //alert("this will delete your old keys. If you havemoney secured by this key, and you haven't saved your key, then this money will be destroyed.");
        var warning = document.createElement("h3");
        warning.innerHTML = "This will delete your old keys from the browser. Save your keys before doing this.";
        var button = button_maker2("cancel ", cancel);
        var button2 = button_maker2("continue", doit);
	var entropy_txt = document.createElement("h3");
	entropy_txt.innerHTML = "put random text here to make keys from";
	var entropy = document.createElement("input");
	entropy.type = "text";
        append_children(new_pubkey_div, [warning, button, br(), button2, entropy_txt, entropy]);
	// add interface for optional entropy 
        function cancel() {
            new_pubkey_div.innerHTML = "";
        }
        function doit() {
            new_pubkey_div.innerHTML = "";
	    var x = entropy.value;
	    if (x == '') {//If you don't provide entropy, then it uses a built in random number generator.
		keys = new_keys();
		set_balance(0);
	    } else {
		keys = new_keys_entropy(x);
	    }
            update_pubkey();
        }
    }
    function check_balance(Callback) {
        var trie_key = pubkey_64();
        var top_hash = hash(headers_object.serialize(headers_object.top()));
        merkle.request_proof("accounts", trie_key, function(x) {
	    Callback(x[1]);
        });
    }
    function update_balance() {
        var trie_key = pubkey_64();
        var top_hash = hash(headers_object.serialize(headers_object.top()));
        merkle.request_proof("accounts", trie_key, function(x) {
            set_balance(x[1] / token_units());
        });
    }
    function set_balance(n) {
        bal_div.innerHTML = ("your balance ").concat((n).toString()) + " VEO";
    }
    function save_keys() {
        download(keys.getPrivate("hex"), save_name.value, "text/plain");
    }
    function load_keys() {
        var file = (file_selector.files)[0];
        var reader = new FileReader();
        reader.onload = function(e) {
            keys = ec.keyFromPrivate(reader.result, "hex");
            update_pubkey();
            update_balance();
        }
        reader.readAsText(file);
    }
    function encrypt(val, to) {
        return encryption_object.send(val, to, keys);
    }
    function decrypt(val) {
	return encryption_object.get(val, keys);
    }
    return {make: new_keys, pub: pubkey_64, sign: sign_tx, ec: (function() { return ec; }), encrypt: encrypt, decrypt: decrypt, check_balance: check_balance};
}
var keys = keys_function1();
