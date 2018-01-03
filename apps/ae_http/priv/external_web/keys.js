
function keys_function1() {
    var ec = new elliptic.ec('secp256k1');
    var keys = new_keys();
    var account_title = document.createElement("h3");
    account_title.innerHTML = translate.words("account");
    var div = document.createElement("div");
    var save_name = input_maker("Amoveo ".concat(translate.words("private_key")));
    var save_button = button_maker(translate.words("save_key"), save_keys);
    var file_selector = document.createElement("input");
    file_selector.type = "file";
    file_selector.onchange = load_keys;
    var load_text = document.createTextNode(translate.words("get_key"));
    var pub_div = document.createElement("div");
    var new_pubkey_button = button_maker(translate.words("make_key"), new_keys_check);
    var new_pubkey_div = document.createElement("div");
    var balance_button = button_maker(translate.words("check_balance"), update_balance);
    var bal_div = document.createElement("div");
    document.body.appendChild(account_title);
    document.body.appendChild(div);

    append_children(div, [load_text, file_selector, br(), pub_div, br(), save_name, save_button, br(), new_pubkey_button, new_pubkey_div, br(), bal_div, balance_button]);

    update_pubkey();
    function input_maker(val) {
        var x = document.createElement("input");
        x.type = "text";
        x.value = val;
        return x;
    }
    function new_keys() {
        return ec.genKeyPair();
    }
    function pubkey_64() {
        var pubPoint = keys.getPublic("hex");
        return btoa(fromHex(pubPoint));
    }
    function sign_tx(tx) {
        sig = btoa(array_to_string(sign(tx, keys)));
        return ["signed", tx, sig, [-6]];
    }
    function update_pubkey() {
        pub_div.innerHTML = translate.words("your_pubkey").concat(" ").concat(pubkey_64());
    }
    function new_keys_check() {
        //alert("this will delete your old keys. If you have money secured by this key, and you haven't saved your key, then this money will be destroyed.");
        var warning = document.createElement("h3");
        warning.innerHTML = translate.words("key_warning");
        var button = button_maker(translate.words("cancel"), cancel);
        var button2 = button_maker(translate.words("continue"), doit);
        append_children(new_pubkey_div, [warning, button, button2]);
        function cancel() {
            new_pubkey_div.innerHTML = "";
        }
        function doit() {
            new_pubkey_div.innerHTML = "";
            keys = new_keys();
            update_pubkey();
            set_balance(0);
        }
    }
    function update_balance() {
        var trie_key = pubkey_64();
        console.log("top header is "),
        console.log(headers_object.top()),
        console.log("serialized "),
        console.log(headers_object.serialize(headers_object.top()));
        var top_hash = hash(headers_object.serialize(headers_object.top()));
        console.log("top hash is "),
        console.log(top_hash),
        verify_callback("accounts", trie_key, function(x) {
            set_balance(x[1] / 100000000);
        });
    }
    function set_balance(n) {
        bal_div.innerHTML = translate.words("your_balance").concat((n).toString());
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
            //update_balance();
        }
        reader.readAsText(file);
    }
    return {make: new_keys, pub: pubkey_64, sign: sign_tx, ec: (function() { return ec; }) };
}
var keys = keys_function1();
