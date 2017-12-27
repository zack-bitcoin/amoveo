
function new_keys() {
    return ec.genKeyPair();
}
var keys = new_keys();
function pubkey_64() {
    var pubPoint = keys.getPublic("hex");
    return btoa(fromHex(pubPoint));
}
function sign_tx(tx) {
    console.log("absout to sign tx");
    console.log(JSON.stringify(tx));
    sig = btoa(btoa(array_to_string(sign(tx, keys))));
    return ["signed", tx, sig, [-6]];
}
function download(data, filename, type) {
    var file = new Blob([data], {type: type});
    if (window.navigator.msSaveOrOpenBlob) // IE10+
        window.navigator.msSaveOrOpenBlob(file, filename);
    else { // Others
        var a = document.createElement("a"),
            url = URL.createObjectURL(file);
        a.href = url;
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        setTimeout(function() {
            document.body.removeChild(a);
            window.URL.revokeObjectURL(url);
        }, 0);
    }
}
keys_function1();
function keys_function1() {
    var account_title = document.createElement("h3");
    account_title.innerHTML = get_words("account");
    document.body.appendChild(account_title);

    var div = document.createElement("div");
    document.body.appendChild(div);
    
    var save_name = document.createElement("INPUT");
    save_name.type = "text";
    save_name.id = "save_name";
    save_name.value = "Amoveo ".concat(get_words("private_key"));
    var save_button = document.createElement("input");
    save_button.type = "button";
    save_button.value = get_words("save_key");
    save_button.onclick = save_keys;
    var file_selector = document.createElement("input");
    file_selector.type = "file";
    file_selector.onchange = load_keys;

    var load_text = document.createTextNode(get_words("get_key"));
    div.appendChild(load_text);
    div.appendChild(file_selector);
    div.appendChild(document.createElement("br"));
    var pub_div = document.createElement("div");
    div.appendChild(pub_div);

    div.appendChild(document.createElement("br"));
    div.appendChild(save_name);
    div.appendChild(save_button);


    div.appendChild(document.createElement("br"));
    var new_pubkey_button = document.createElement("input");
    new_pubkey_button.type = "button";
    new_pubkey_button.value = get_words("make_key");
    new_pubkey_button.onclick = new_keys_check;
    div.appendChild(new_pubkey_button);

    var new_pubkey_div = document.createElement("div");
    div.appendChild(new_pubkey_div);
    
    div.appendChild(document.createElement("br"));
    var balance_button = document.createElement("input");
    balance_button.type = "button";
    balance_button.value = get_words("check_balance");
    balance_button.onclick = update_balance;
    var bal_div = document.createElement("div");
    div.appendChild(bal_div);
    div.appendChild(balance_button);
    
    update_pubkey();

    //console.log(fromHex(toHex("abc")));
    function update_pubkey() {
        pub_div.innerHTML = get_words("your_pubkey").concat(pubkey_64());
    }
    function new_keys_check() {
        //alert("this will delete your old keys. If you have money secured by this key, and you haven't saved your key, then this money will be destroyed.");
        var warning = document.createElement("h3");
        warning.innerHTML = get_words("key_warning");
        new_pubkey_div.append(warning);
        
        var button = document.createElement("input");
        button.type = "button";
        button.value = get_words("cancel");
        button.onclick = cancel;
        new_pubkey_div.appendChild(button);

        var button2 = document.createElement("input");
        button2.type = "button";
        button2.value = get_words("continue");
        button2.onclick = doit;
        new_pubkey_div.appendChild(button2);

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
        var top_hash = hash(serialize_header(top_header));
        verify_callback("accounts", trie_key, function(x) {
            set_balance(x[1] / 100000000);
        });
    }
    function set_balance(n) {
        bal_div.innerHTML = get_words("your_balance").concat((n).toString());
    }
    function save_keys() {
        var save_name = document.getElementById("save_name");
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
}
