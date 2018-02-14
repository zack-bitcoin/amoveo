function merkle_proofs_main() {
    function verify_callback(tree, key, callback) {
	var top_hash = hash(headers_object.serialize(headers_object.top()));
	variable_public_get(["proof", btoa(tree), key, btoa(array_to_string(top_hash))], function(proof){
	    var val = verify_merkle(key, proof);
	    return callback(val);
	    
	});
    }
    function hash_member(hash, members) {
        for (var i = 0; i < 6; i++) {
            var h2 = members.slice(32*i, 32*(i+1));
            //console.log("check that hash is a member");
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
            var b = check_equal(p, h);
            if (b) { return true; }
        }
        return false;
    }
    function leaf_hash(v, trie_key) {
        var serialized =
            serialize_key(v, trie_key).concat(
                serialize_tree_element(v, trie_key));
	//console.log("hashed leaf");
	//console.log(JSON.stringify(serialized));
        return hash(serialized);
    }
    function verify_merkle(trie_key, x) {
    //x is {return tree_roots, tree_root, value, proof_chain}
	var tree_roots = string_to_array(atob(x[1]));
	var header_trees_hash = string_to_array(atob(headers_object.top()[3]));
	var hash_tree_roots = hash(tree_roots);
	var check = check_equal(header_trees_hash, hash_tree_roots);
	if (!(check)) {
            console.log("the hash of tree roots doesn't match the hash in the header.");
	} else {
            var tree_root = string_to_array(atob(x[2]));
            var check2 = hash_member(tree_root, tree_roots);
            if (!(check2)) {
		console.log("that tree root is not one of the valid tree roots.");
            } else {
		var chain = x[4].slice(1);
		chain.reverse();
		var h = link_hash(chain[0]);
		var check3 = check_equal(h, tree_root);
		var check4 = chain_links(chain);
		if (!(check3)) {
                    console.log("the proof chain doesn't link to the tree root");
		} else if (!(check4)){
                    console.log("the proof chain has a broken link");
		} else {
                    var last = chain[chain.length - 1];
                    var value = x[3];
                    var lh = leaf_hash(value, trie_key);
                    var check5 = chain_links_array_member(last, lh);
                    if (check5) {
			return value;
			//we should learn to deal with proofs of empty data.
                    } else {
			console.log("the value doesn't match the proof");
			console.log(x);
			console.log(trie_key);
			throw("bad");
                    }
		}
            }
	}
    }
    function serialize_key(v, trie_key) {
	var t = v[0];
	if ( t == "gov" ) {
            return integer_to_array(trie_key, 8);
	} else if ( t == "acc" ) {
            //console.log("v is ");
            //console.log(v);
            var pubkey = string_to_array(atob(v[3]));
            return hash(pubkey);
	} else if ( t == "channel" ) {
            //return hash(integer_to_array(v[1], 32));
            return hash(string_to_array(atob(v[1])));
	} else if (t == "oracle") {
            //return hash(integer_to_array(v[1], 32));
            return hash(string_to_array(atob(v[1])));
	} else {
            //console.log("type is ");
            //console.log(t);
            //console.log(v);
            throw("serialize trie bad trie type");
	}
    }
    function serialize_tree_element(v, trie_key) {
	//console.log("serialize tree element");
	//console.log(JSON.stringify(v));
	//console.log(trie_key);
	var t = v[0];
	if ( t == "gov" ) {
            var id = integer_to_array(v[1], 1);
            var value = integer_to_array(v[2], 2);
            var lock = integer_to_array(v[3], 1);
            var serialized = ([]).concat(
		id).concat(
                    value).concat(
			lock);
            return serialized;
	} else if ( t == "acc" ) {
            var balance = integer_to_array(v[1], 6);
            var nonce = integer_to_array(v[2], 3);
            var pubkey = string_to_array(atob(v[3]));
            var bets = string_to_array(atob(v[5]));
            var serialized = ([]).concat(
		balance).concat(
                    nonce).concat(
			pubkey).concat(
                            bets);
            return serialized;
	} else if ( t == "channel" ) {
            //var cid = integer_to_array(v[1], 32);
            var cid = string_to_array(atob(v[1]));
            var acc1 = string_to_array(atob(v[2]));
            var acc2 = string_to_array(atob(v[3]));
            var bal1 = integer_to_array(v[4], 6);
            var bal2 = integer_to_array(v[5], 6);
            var amount = integer_to_array(128, 1).concat(
		integer_to_array(v[6], 5));
            var nonce = integer_to_array(v[7], 4);
            var last_modified = integer_to_array(v[8], 4);
            var delay = integer_to_array(v[9], 4);
            var closed = integer_to_array(v[11], 1);
            var serialized = ([]).concat(
		cid).concat(
                    bal1).concat(
			bal2).concat(
                            amount).concat(
				nonce).concat(
                                    last_modified).concat(
					delay).concat(
                                            closed).concat(
						acc1).concat(
                                                    acc2);
            return serialized;
	} else if (t == "oracle") {
            //var id = integer_to_array(v[1], 32);
            //var id = string_to_array(v[1], 32);
	    //console.log("serialize oracle ");
	    //console.log(JSON.stringify(v));
            var id = string_to_array(atob(v[1]));
            var result = integer_to_array(v[2], 1);
            var type = integer_to_array(v[5], 1);
            var starts = integer_to_array(v[4], 4); 
            var done_timer = integer_to_array(v[9], 4); //height_bits/8 bytes
            var governance = integer_to_array(v[10], 1); //one byte
            var governance_amount = integer_to_array(v[11], 1); //one byte
            var creator = string_to_array(atob(v[8])); //pubkey size
            var question = string_to_array(atob(v[3])); //32 bytes size
            var orders = string_to_array(atob(v[7])); //32 bytes
            //var serialized = integer_to_array(v[1], 256).concat(
            var serialized = ([]).concat(
		id).concat(
                    result).concat(
			type).concat(
                            starts).concat(
				done_timer).concat(
                                    governance).concat(
					governance_amount).concat(
                                            creator).concat(
						question).concat(
                                                    orders);
	    //console.log("serialized oracle");
	    //console.log(JSON.stringify(serialized));
            return serialized;
	} else {
            console.log("cannot decode type ");
            console.log(t);
	}
    }
    function test() {
	verify_callback("governance", 14, function(fun_limit) {
	    console.log("merkle proof test result is: ");
	    console.log(fun_limit);
	});
	verify_callback("oracles", "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=", function(fun_limit) {
	    console.log("merkle proof test result is: ");
	    console.log(fun_limit);
	});
    }
    return {request_proof: verify_callback,
	    verify: verify_merkle,
	    serialize: serialize_tree_element,
	    serialize_key: serialize_key,
	    test: test};
}
var merkle = merkle_proofs_main();
