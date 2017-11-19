
function verify_merkle(trie_key, x) {
    function hash_member(hash, members) {
        for (var i = 0; i < 6; i++) {
            var h2 = members.slice(32*i, 32*(i+1));
            console.log("check that hash is a member");
            var b = check_equal(hash, h2);
            if (b) { return true; }
        }
        return false;
    }
    function check_equal(a, check_b) {
        for (var i = 0; i < a.length; i++) {
            if (!(a[i] == check_b[i])) {
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
            var chain_links_b = chain_links_array_member(parent, lh);
            if (chain_links_b == false) {
                return false;
            }
            //out = out && chain_links_array_member(parent, lh);
        }
        return true;
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
    function leaf_hash(v, trie_key) {
        var t = v[0];
        console.log("leaf_hash");
        console.log(t);
        if ( t == "gov" ) {
            /* <<(Gov#gov.id):8,
               (Gov#gov.value):16,
               (Gov#gov.lock):8>>. */
            var id = integer_to_array(v[1], 1);
            var value = integer_to_array(v[2], 2);
            var lock = integer_to_array(v[3], 1);
            var serialized =  integer_to_array(trie_key, 8).concat(
                id).concat(value).concat(serialized);
            return hash(serialized);
        } else if ( t == "acc" ) {
        /* <<(Account#acc.balance):BalanceSize, balance bits
          (Account#acc.nonce):NonceSize, account_nonce_bits
          (Account#acc.height):HeightSize, height_bits
          (Account#acc.pubkey)/binary, pubkey_size
          BetsRoot/binary>>,  32 bytes */
            var balance = integer_to_array(v[1], 6);
            var nonce = integer_to_array(v[2], 3);
            var height = integer_to_array(v[3], 4);
            var pubkey = string_to_array(atob(v[4]));
            var bets = string_to_array(atob(v[6]));
            //The key is the hash of the pubkey.
            var serialized = integer_to_array(0, 32*7).concat(
                hash(pubkey)).concat(
                    balance).concat(nonce).concat(
                        height).concat(
                            pubkey).concat(
                                bets);
            return hash(serialized);
            
        } else if ( t == "channel" ) {
            var cid = integer_to_array(v[1], 32);
            var acc1 = v[2];
            var acc2 = v[3];
            var bal1 = integer_to_array(v[4], 6);//balance bits
            var bal2 = integer_to_array(v[5], 6);
            var hb = Math.floor(Math.pow(2, 47));
            var amount = integer_to_array(v[6]+hb, 6);
            var nonce = integer_to_array(v[7], );
            var timeout_height = integer_to_array(v[8], );
            var last_modified = integer_to_array(v[9], );
            var entropy = integer_to_array(v[10], );
            var delay = integer_to_array(v[11], );
            var slasher = integer_to_array(v[12], );
            var closed = integer_to_array(v[13], );

            

            console.log("working here");

        } else {
            console.log("cannot decode type ");
            console.log(t);
        }
    }




    
    //x is {return tree_roots, tree_root, value, proof_chain}
    var tree_roots = string_to_array(atob(x[1]));
    var header_trees_hash = string_to_array(atob(top_header[3]));
    var hash_tree_roots = hash(tree_roots);
    var check = check_equal(header_trees_hash, hash_tree_roots);
    //verify that the hash of tree_roots matches the hash in our top header.
    if (check) {
        var tree_root = string_to_array(atob(x[2]));
        //set_balance(0);
        var check2 = hash_member(tree_root, tree_roots);
        //verify that tree root is one of the roots in tree_roots.
        if (check2) {
            var chain = x[4].slice(1);
            chain.reverse();
            var h = link_hash(chain[0]);
            var check3 = check_equal(h, tree_root);
            //verify that the first link of the proof_chain is linked to tree root.
            if (check3) {
                var check4 = chain_links(chain);
                //verify that every link of the proof_chain is linked.
                if (check4) {
                    var last = chain[chain.length - 1];
                    var value = x[3];
                    var lh = leaf_hash(value, trie_key);
                    var check5 = chain_links_array_member(last, lh);
                    //verify that the value is linked to the last link of the proof chain.
                    if (check5) {
                        return value;
                        //if value is empty, return 0, otherwise grab the balance from the account and return that.
                    } else {
                        console.log("the proof chain doesn't point to that value");
                        return 0
                    } 
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
