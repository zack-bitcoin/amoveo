function keys_function1() {
    var tab_id = "account";
    var ec = new elliptic.ec('secp256k1');
    var keys = new_keys();
    var account_title = document.createElement("h3");
    account_title.className = "tabs__nav-item";
    account_title.innerHTML = "account";
    account_title.dataset.tab = tab_id;
    var div = document.createElement("div");
    div.className = "tabs__content-item " + tab_id;
    div.id = tab_id;
    var save_name = document.createElement("input");
    save_name.type = "text";
    save_name.value = "Amoveo private key";
    save_name.className = "wide";
    var save_button = button_maker2("Save private key to file", save_keys);
    var file_selector = document.createElement("input");
    file_selector.type = "file";
    file_selector.onchange = load_keys;
    file_selector.id = "file-key";
    var file_selector_btn = document.createElement("label");
    file_selector_btn.className = "btn";
    //file_selector_btn.htmlFor = "file-key";
    file_selector_btn.innerHTML = "Get key from file";
    var load_text = document.createTextNode("Your pubkey");
    var watch_only_instructions = document.createElement("p");
    watch_only_instructions.innerHTML = "Put your pubkey here to make a watch-only wallet that is unable to spend money.";
    var watch_only_pubkey = document.createElement("input");
    watch_only_pubkey.type = "text";
    var watch_only_button = button_maker2("Load pubkey", watch_only_func);
    var pub_div = document.createElement("div");
    var new_pubkey_button = button_maker2("Generate new keys", new_keys_check);
    var new_pubkey_div = document.createElement("div");
    new_pubkey_div.className = "wrng hidden";
    var balance_button = button_maker2("Check balance", update_balance);
    var bal_div = document.createElement("div");

    var pub_div_wr = wrapper("tabs__box hidden", [pub_div, bal_div, balance_button]);
    var put = wrapper("tabs__box", [watch_only_instructions, watch_only_pubkey, br(), br(), watch_only_button]);

    var wrap = document.createElement("div");
    wrap.className = "tabs__col";
    wrap.id = "account_pubkey";

    var wrap_right = document.createElement("div");
    wrap_right.className = "tabs__col";

    if (!nav.hasChildNodes()) {
        account_title.className += " active";
        div.className += " active"
    }

    tabs.appendChild(div);
    nav.appendChild(account_title);

    var get_wr = wrapper("fieldset fieldset_sb", [file_selector, file_selector_btn, new_pubkey_button, new_pubkey_div]);
    var save_wr = wrapper("fieldset fieldset_2col hidden", [save_name, save_button]);
    var transaction_wrap = document.createElement("div");
    transaction_wrap.id = "transaction_wrap";
    var spoiler = document.createElement("div");
    spoiler.id = "account_spoiler";
    spoiler.className = "spoiler";
    var sp_title = document.createElement("button");
    sp_title.innerHTML = "Advanced features";
    sp_title.className = "spoiler__button";
    var sp_left = wrapper("tabs__col", [transaction_wrap]);
    var sp_right = wrapper("tabs__col", [put]);
    sp_left.id = "account_spoiler_left";
    sp_right.id = "account_spoiler_right";
    var sp_wr = document.createElement("div");
    sp_wr.className = "spoiler__content";

    append_children(sp_wr, [sp_left, sp_right]);
    append_children(spoiler, [sp_title, sp_wr]);
    append_children(wrap, []);
    append_children(wrap_right, [pub_div_wr, get_wr, save_wr]);
    append_children(div, [wrap, wrap_right, spoiler]);

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
        return ec.genKeyPair({entropy: hash(x)});
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
    function unblock_btn(){
        pub_div_wr.classList.remove("hidden");
        save_wr.classList.remove("hidden");
        spend_button.disabled = false;
        sign_button.disabled = false;
        push_button.disabled = false;
    }

    function update_pubkey() {
        pub_div.innerHTML = ("<p>Your pubkey</p>").concat("<code>" + pubkey_64() + "</code>");
    }
    function watch_only_func() {
	var v = watch_only_pubkey.value;
	keys = new_keys_watch(string_to_array(atob(v)));
	update_pubkey();
    }
    function new_keys_check() {
        //alert("this will delete your old keys. If you havemoney secured by this key, and you haven't saved your key, then this money will be destroyed.");
        var warning = document.createElement("p");
        warning.innerHTML = "This will delete your old keys from the browser. Save your keys before doing this.";
        var button = input_maker2("cancel", cancel);
        var button2 = input_maker2("continue", doit);
        new_pubkey_button.classList.add("btn_loading");
        new_pubkey_div.classList.remove("hidden");
        warning.className = "msg";

	var entropy_txt = document.createElement("p");
	entropy_txt.innerHTML = "Put random text here to make keys from";
	var entropy = document.createElement("input");
	entropy.type = "text";
	entropy_txt.className = "msg";
        append_children(new_pubkey_div, [warning, button, button2, entropy_txt, entropy]);
	// add interface for optional entropy
        function cancel() {
            new_pubkey_div.innerHTML = "";
            new_pubkey_div.classList.add("hidden");
            new_pubkey_button.classList.remove("btn_loading");
        }
        function doit() {
            new_pubkey_div.innerHTML = "";
            new_pubkey_div.classList.add("hidden");
            new_pubkey_button.classList.remove("btn_loading");

	    var x = entropy.value;
	    if (x == '') {//If you don't provide entropy, then it uses a built in random number generator.
		keys = new_keys();
		set_balance(0);
	    } else {
		keys = new_keys_entropy(x);
	    }
            update_pubkey();
            unblock_btn();
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
        bal_div.innerHTML = ("Your balance ").concat((n).toString()) + " mVEO";
        bal_div.className = "msg";
    }
    function save_keys() {
        download(keys.getPrivate("hex"), save_name.value, "text/plain");
    }
	// check balance every 20sec
	setInterval(function() {
		update_balance();
	}, 20000);
	file_selector_btn.addEventListener("click", function(e) {
		this.classList.add("btn_loading");
		file_selector.click();
	});
    function load_keys() {
        file_selector_btn.classList.add("btn_loading");
        var file = (file_selector.files)[0];
        var reader = new FileReader();
        reader.onload = function(e) {
            keys = ec.keyFromPrivate(reader.result, "hex");
            update_pubkey();
            update_balance();
            unblock_btn();
            file_selector_btn.classList.remove("btn_loading");
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
