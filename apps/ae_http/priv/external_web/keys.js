
keys_function1();
function keys_function1() {
    var div = document.createElement("div");
    document.body.appendChild(div);
    var keys = new_keys();
    function new_keys() {
        var crypto_keys = ec.genKeyPair();
        return crypto_keys;
    }
    
    //button to update displayed pubkey.
    var pubkey_button = document.createElement("input");
    pubkey_button.type = "button";
    pubkey_button.value = "check pubkey value";
    pubkey_button.onclick = update_pubkey;
    //div.appendChild(pubkey_button);

    var pub_div = document.createElement("div");
    div.appendChild(pub_div);

    var save_button = document.createElement("input");
    save_button.type = "button";
    save_button.value = "save keys to file";
    save_button.onclick = save_keys;
    div.appendChild(save_button);

    div.appendChild(document.createElement("br"));
    var file_selector = document.createElement("input");
    file_selector.type = "file";
    file_selector.onchange = load_keys;

    var load_text = document.createTextNode("get key from file ");
    div.appendChild(load_text);
    div.appendChild(file_selector);

    div.appendChild(document.createElement("br"));
    var new_pubkey_button = document.createElement("input");
    new_pubkey_button.type = "button";
    new_pubkey_button.value = "generate new keys";
    new_pubkey_button.onclick = new_keys_check;
    div.appendChild(new_pubkey_button);

    var new_pubkey_div = document.createElement("div");
    div.appendChild(new_pubkey_div);

    div.appendChild(document.createElement("br"));
    var balance_button = document.createElement("input");
    balance_button.type = "button";
    balance_button.value = "check balance";
    balance_button.onclick = update_balance;
    var bal_div = document.createElement("div");
    div.appendChild(bal_div);
    div.appendChild(balance_button);

    update_pubkey();

    //console.log(fromHex(toHex("abc")));
    function pubkey_64() {
        var pubPoint = keys.getPublic("hex");
        return btoa(fromHex(pubPoint));
    }
    function update_pubkey() {
        pub_div.innerHTML = ("your pubkey: ").concat(pubkey_64());
    }
    function new_keys_check() {
        //alert("this will delete your old keys. If you have money secured by this key, and you haven't saved your key, then this money will be destroyed.");
        var warning = document.createElement("h3");
        warning.innerHTML = "This will delete your old keys. If money is sent to them, it will be deleted.";
        new_pubkey_div.append(warning);
        
        var button = document.createElement("input");
        button.type = "button";
        button.value = "cancel";
        button.onclick = cancel;
        new_pubkey_div.appendChild(button);

        var button2 = document.createElement("input");
        button2.type = "button";
        button2.value = "continue";
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
        //variable_public_get(["proof", btoa("accounts"), pubkey_64()], update_balance2);
        variable_public_get(["proof", btoa("governance"), 5], update_balance2);
    }
    function update_balance2(x) {
        console.log(JSON.stringify(x));
        console.log("header");
        console.log(JSON.stringify(top_header));
        //x is {return tree_roots, tree_root, value, proof_chain}
        var tree_roots = string_to_array(atob(x[1]));
        console.log("tree roots");
        console.log(JSON.stringify(tree_roots));
        var header_trees_hash = string_to_array(atob(top_header[3]));
        var hash_tree_roots = hash(tree_roots);
        var check = check_equal(header_trees_hash, hash_tree_roots);
        //verify that the hash of tree_roots matches the hash in our top header.
        if (check) {
            var tree_root = string_to_array(atob(x[2]));
            //set_balance(0);
            console.log("tree root");
            console.log(JSON.stringify(tree_root));
            var check2 = hash_member(tree_root, tree_roots);
            //verify that tree root is one of the roots in tree_roots.
            if (check2) {
                var chain = x[4].slice(1);
                chain.reverse();
                var h = link_hash(chain[0]);
                var check3 = check_equal(h, tree_root);
                //verify that the first link of the proof_chain is linked to tree root.
                if (check3) {
                    console.log("proof chain2");
                    var check4 = chain_links(chain);
                    console.log(check4);
                    //verify that every link of the proof_chain is linked.
                    if (check4) {
                        console.log("check4");
                        //verify that the value is linked to the last link of the proof chain.
                        //if value is empty, return 0, otherwise grab the balance from the account and return that.
                    } else {
                        console.log("the proof chain has a broken link");
                    }
                } else {
                    console.log("the proof chain doesn't link to the tree root");
                }
            } else {
                console.log("the tree root doesen't link to the tree hashes");
            }
        } else {
            console.log("the tree hashes don't match the hash in the header.");
        }
    }
    function hash_member(hash, members) {
        for (var i = 0; i < 6; i++) {
            var h2 = members.slice(32*i, 32*(i+1));
            var b = check_equal(hash, h2);
            if (b) { return true; }
        }
        return false;
    }
    function set_balance(n) {
        bal_div.innerHTML = ("balance: ").concat((n).toString());
        
    }
    function save_keys() {
        download(keys.getPrivate("hex"), "amoveo_light_keys", "text/plain");
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
    function check_equal(a, b) {
        for (var i = 0; i < a.length; i++) {
            if (!(a[i] == b[i])) {
                return false
            }
        }
        return true;
    }
    function link_hash(l) {
        var h = [];
        for (var i = 1; i < l.length; i++) {
            //console.log(link[i]);
            var x = string_to_array(atob(l[i]));
            h = x.concat(h);
        }
        return hash(h);
    }
    function chain_links(chain) {
        var out = true;
        for (var i = 1; i < chain.length; i++) {
            var parent = chain[i-1];
            var child = chain[i];
            var lh = link_hash(child);
            console.log("chain links parent lh");
            console.log(parent);
            console.log(lh);
            out = out && chain_links_array_member(parent, lh);
        }
        return out;
    }
    function chain_links_array_member(parent, h) {
        for (var i = 1; i < parent.length; i++) {
            var x = parent[i];
            var p = string_to_array(atob(x));
            console.log(p);
            var b = check_equal(p, h);
            if (b) { return true; }
        }
        return false;
    }
}
