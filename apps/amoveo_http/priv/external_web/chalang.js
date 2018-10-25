function chalang_main() {
    const word_size = 4294967296,
          hash_size = 32;
    const ops =
          {int_op: 0,
           binary_op: 2,
           print: 10,
           finish: 11, //because 'return' is reserved.
           nop: 12,
           fail: 13,
           drop: 20,
           dup: 21,
           swap: 22,
           tuck: 23,
           rot: 24,
           ddup: 25,
           tuckn: 26,
           pickn: 27,
           to_r: 30,
           from_r: 31,
           r_fetch: 32,
           hash_op: 40,
           verify_sig: 41,
           add: 50,
           subtract: 51,
           mul: 52,
           div: 53,
           gt: 54,
           lt: 55,
           pow: 56,
           rem: 57,
           eq: 58,
           caseif: 70,
           caseelse: 71,
           casethen: 72,
           bool_flip: 80,
           bool_and: 81,
           bool_or: 82,
           bool_xor: 83,
           bin_and: 84,
           bin_or: 85,
           bin_xor: 86,
           stack_size: 90,
           height: 94,
           gas: 96,
           ram: 97,
           many_vars: 100,
           many_funs: 101,
           define: 110,
           fun_end: 111,
           recurse: 112,
           call: 113,
           set: 120,
           fetch: 121,
           cons: 130,
           car: 131,
           empty_list: 132,
           append: 134,
           split: 135,
           reverse: 136,
           is_list: 137};
    function memory(x) {
        if (JSON.stringify(x) == "[]") {
            return 1;
        } else if (Number.isInteger(x)) {
            return 4;
        } else if (x[0] == "binary") {
            return x.length - 1;
        } else {
            var y = x.slice(1);
            return memory(x[0]) + memory(y);
        }
    }
    function underflow_check(d, min_size, op_name) {
        if (d.stack.length < min_size) {
            throw(JSON.stringify(["error", "stack underflow", op_name]));
        }
    }
    function exponential(b, a) {
        if (b == 0) { return 0; }
        else if (a == 0) { return 1; }
        var n = 1;
        while (a > 1) {
            if ((a % 2) == 0) {
                b = b*b;
                a = Math.floor(a / 2);
            } else {
                a = a - 1;
                n = n * b;
            }
        }
        return b * n;
    }
    function arithmetic_chalang(op, a, b) { //returns a list to concat with stack.
        var x;
        var d = {"stack":[]};
        var i = 0;
        if (op == ops.add) {
            op_print(d, i, "add op");
            x = a + b;
        } else if (op == ops.subtract) {
            op_print(d, i, "subtract op");
            x = b - a;
        } else if (op == ops.mul) {
            op_print(d, i, "mul op");
            x = b * a;
        } else if (op == ops.div) {
            op_print(d, i, "div op");
            x = Math.floor(b / a);
        } else if (op == ops.pow) {
            op_print(d, i, "pow op");
            x = exponential(b, a);
        } else if (op == ops.rem) {
            op_print(d, i, "rem op");
            x = b % a;
        } else if (op == ops.gt) {
            op_print(d, i, "gt op");
            if (b > a) {
                x = 1;
            } else {
                x = 0;
            }
        } else if (op == ops.lt) {
            op_print(d, i, "lt op");
            if (b < a) {
                x = 1;
            } else {
                x = 0;
            }
        }
        x = ((x % word_size) + word_size) % word_size;
        return [x];
    }
    function small_hash(l) {
        var h = hash(l);
        return h.slice(0, hash_size);
    }
    function split_if(opcode, code) {
        var a = 0;
        for (var i = 0; i < code.length; i++) {
            if ((code[i]) == ops.int_op) {
                i += 4;
            } else if (code[i] == ops.binary_op) {
                var h = array_to_int(code.slice(i+1, i+5));
                i += (4 + h);
            } else if ((code[i] == ops.caseif)){
                var k = count_till(code, i+1, ops.casethen);
                i += (k);
            } else if (opcode == code[i]) {
                return {"rest": code.slice(i),
                        "code": code.slice(0, i),
                        "n": i};
            }
        }
        throw("split if error");
    }
    function count_till(code, i, opcode) {
        for (var j = 0; (j + i) < code.length; j++) {
            if ((code[i+j]) == ops.int_op) {
                j += 4;
            } else if (opcode == code[i+j]) {
                return j;
            } else if (code[i+j] == ops.binary_op) {
                var h = array_to_int(code.slice(i+j+1, i+j+5));
                j += (4 + h);
            } else if ((code[i+j] == ops.caseif)){
                var k = count_till(code, i+j+1, ops.casethen);
                j += (k + 1);
            }
        }
        console.log(opcode);
        throw("count till reached end without finding goal");
    }
    function replace(old_character, new_code, binary) {
        for (var i = 0; i < binary.length; i++) {
            if (binary[i] == old_character) {
                var r2 = replace(old_character, new_code, binary.slice(i+1));
                return binary.slice(0,i).concat(new_code).concat(r2);
            } else if (binary[i] == ops.int_op) {
                i += 4;
            } else if (binary[i] == ops.binary_op) {
                var h = array_to_int(binary.slice(i+1, i+5));
                i += (4 + h);
            }
        }
        return binary;
    }
    var verbose = false;
    var stack_verbose = false;
    function op_print(d, i, x) {
        if (verbose) {
            console.log(("# ").concat(
                (i).toString()).concat(
                    " ").concat(x));
        }
        if (stack_verbose) {
            console.log(JSON.stringify(d.stack));
        }
    }
    var op_code = {};
    op_code[ops.int_op] = function(i, code, d) {
        var int_array = code.slice(i+1, i+5);
        var new_int = array_to_int(int_array);
        d.stack = ([new_int]).concat(d.stack);
        return {i: i+4, d: d, g: 1, s: "int op", r: 1};
    };
    op_code[ops.binary_op] = function(i, code, d) {
        var int_array = code.slice(i+1, i+5);
        var new_int = array_to_int(int_array);
        var bin_array = code.slice(i+5, i+5+new_int);
        var bin1 = (["binary"]).concat(bin_array);
        d.stack = ([bin1]).concat(
            d.stack);
        return {i: i+4+new_int, d: d, g: new_int, s: "bin op", r: 1};
    }
    op_code[ops.caseif] = function(i, code, d) {
        var b = d.stack[0];
        var size_case1 = count_till(code, i + 1, ops.caseelse);
        if (b == 0) {
            i += (size_case1 + 1);
        }
        d.stack = d.stack.slice(1);
        return {i: i, d: d, g: 0, s: "if op"};
    }
    op_code[ops.caseelse] = function(i, code, d) {
        var skipped_size = count_till(code, i + 1, ops.casethen);
        i += (skipped_size + 0);
        return {i: i, d: d, g: 0, s: "else op"};
    }
    op_code[ops.casethen] = function(i, code, d) {
        // do nothing.
        return {i: i, d: d, g: 0, s: "then op"};
    }
    op_code[ops.call] = function(i, code, d) {
        //non-optimized function call.
        var code_hash=btoa(array_to_string(d.stack[0].slice(1)));
        var definition = d.funs[code_hash];
	if (definition == undefined) {
	    console.log("undefined function");
	    console.log(code_hash);
	} else {
	    console.log("function named ");
	    console.log(code_hash);
	}
        var s = definition.length;
        d.stack = d.stack.slice(1);
        d = run2(definition, d);
	console.log("d after ops call ");
	console.log(JSON.stringify(d));
        return {i: i, d: d, g: (s + 10), s: "slow call op", r: (s - 1)};
    }
    op_code[ops.define] = function(i, code, d) {
        var skipped_size = count_till(code, i, ops.fun_end);
        var definition = code.slice(i+1, i+skipped_size);
	//console.log("chalang define definition is ");
	//console.log(definition);
	//console.log(JSON.stringify(definition));
        i += skipped_size;
        //var hash_array = small_hash(definition);
        var hash_array = hash(definition);
        var b = btoa(array_to_string(hash_array));
	console.log("new function hash is ");
	console.log(b);
        var definition2 = replace(ops.recurse, ([ops.binary_op]).concat(integer_to_array(hash_size, 4)).concat(hash_array), definition);
        d.funs[b] = definition2;
        var s = definition2.length + 4;
        var mf = d.many_funs + 1;
        if (mf > d.fun_limit) {
            throw("too many functions error");
        } else {
            d.many_funs = mf;
        }
        return {i: i, d: d, g: (s + 30), s: "define op", r: (s+s)};
    }
    op_code[ops.print] = function(i, code, d) {
        console.log(JSON.stringify(d.stack));
        return {i: i, d: d, g: 0, s: "print op"};
    };
    op_code[ops.drop] = function(i, code, d) {
        underflow_check(d, 1, "drop");
	var m = memory(d.stack[0]);
        d.stack = d.stack.slice(1);
        return {i: i, d: d, g: 1, s: "drop op", r: (-2 - m)};
    };
    op_code[ops.dup] = function(i, code, d) {
        underflow_check(d, 1, "dup");
        d.stack = ([d.stack[0]]).concat(d.stack);
        return {i: i, d: d, g: 1, s: "dup op", r: memory(d.stack[0])};
    };
    op_code[ops.swap] = function(i, code, d) {
        underflow_check(d, 2, "swap");
        d.stack = ([d.stack[1]]).concat(
            [d.stack[0]]).concat(
                d.stack.slice(2));
        return {i: i, d: d, g: 1, s: "swap op"};
    };
    op_code[ops.tuck] = function(i, code, d) {
        underflow_check(d, 3, "tuck");
        d.stack = ([d.stack[1]]).concat(
            [d.stack[2]]).concat(
                [d.stack[0]]).concat(
                    d.stack.slice(3));
        return {i: i, d: d, g: 1, s: "tuck op"};
    }
    op_code[ops.rot] = function(i, code, d) {
        underflow_check(d, 3, "rot");
        d.stack = ([d.stack[2]]).concat(
            [d.stack[0]]).concat(
                [d.stack[1]]).concat(
                    d.stack.slice(3));
        return {i: i, d: d, g: 1, s: "rot op"};
    }
    op_code[ops.ddup] = function(i, code, d) {
        underflow_check(d, 2, "ddup");
        d.stack = d.stack.slice(0, 2).concat(d.stack);
        return {i: i, d: d, g: 1, s: "ddup op", r: (memory(d.stack[0]) + memory(d.stack[1]))};
    }
    op_code[ops.tuckn] = function(i, code, d) {
        if (d.stack.length < 2) {
            throw("tuckn stack underflow");
        } else {
            var n = d.stack[0];
            underflow_check(d, 2+n,"tuckn");
            d.stack = d.stack.slice(2, 2+n).concat(
                [d.stack[1]]).concat(
                    d.stack.slice(3+n));
        }
        return {i: i, d: d, g: 1, s: "tuckn op"};
    }
    op_code[ops.pickn] = function(i, code, d) {
        var n = d.stack[0];
        if (d.stack.length < (n + 1)) {
            throw("pickn stack underflow");
        } else {
            d.stack = ([d.stack[n]]).concat(
                d.stack.slice(1, 1+n)).concat(
                    d.stack.slice(2+n));
        }
        return {i: i, d: d, g: 1, s: "pickn op"};
    }
    op_code[ops.to_r] = function(i, code, d) {
        underflow_check(d, 1, "to_r");
        d.alt = ([d.stack[0]]).concat(d.alt);
        d.stack = d.stack.slice(1);
        return {i: i, d: d, g: 1, s: ">r op"};
    }
    op_code[ops.from_r] = function(i, code, d) {
        if (d.alt.length < 1) {
            throw(">r alt stack underflow");
        } else {
            d.stack = ([d.alt[0]]).concat(d.stack);
            d.alt = d.alt.slice(1);
        }
        return {i: i, d: d, g: 1, s: "r> op"};
    }
    op_code[ops.r_fetch] = function(i, code, d) {
        if (d.alt.length < 1) {
            throw("alt stack underflow");
        } else {
            d.stack = ([d.alt[0]]).concat(d.stack);
        }
        return {i: i, d: d, g: 1, s: "r@ op"};
    }
    op_code[ops.hash_op] = function(i, code, d) {
        underflow_check(d, 1, "hash");
        d.stack = ([["binary"].concat(hash(d.stack[0].slice(1)))]).concat(
            d.stack.slice(1));
        return {i: i, d: d, g: 20, s: "hash op"};
    }
    op_code[ops.verify_sig] = function(i, code, d) {
        underflow_check(d, 3, "verify_sig");
        //data, sig, key
        var pub1 = d.stack[0].slice(1);//internal format puts "binary" at the front of each binary.
        var data1 = d.stack[1].slice(1);
        var sig1 = d.stack[2].slice(1);
	var ec = keys.ec(),
        temp_key = ec.keyFromPublic(toHex(array_to_string(pub1)), "hex");
        var sig2 = bin2rs(array_to_string(sig1));
        var b = temp_key.verify(hash(serialize(data1)), sig2, "hex")
        var c;
        if (b) { c = 1; }
        else { c = 0; }
        d.stack = ([c]).concat(
            d.stack.slice(3));
        return {i: i, d: d, g: 20, s: "verify_sig op"};
    }
    op_code[ops.eq] = function(i, code, d) {
        underflow_check(d, 2, "eq");
        if (JSON.stringify(d.stack[0]) == JSON.stringify(d.stack[1])) {
            d.stack = ([1]).concat(d.stack);
        } else {
            d.stack = ([0]).concat(d.stack);
        }
        return {i: i, d: d, g: 1, s: "eq op", r: 1};
    }
    op_code[ops.bool_flip] = function(i, code, d) {
        underflow_check(d, 1, "bool_flip");
        if (d.stack[0] == 0) {
            d.stack = ([1]).concat(d.stack.slice(1));
        } else {
            d.stack = ([0]).concat(d.stack.slice(1));
        }
        return {i: i, d: d, g: 1, s: "bool flip op"};
    }
    op_code[ops.bool_and] = function(i, code, d) {
        underflow_check(d, 2, "bool_and");
        if ((d.stack[0] == 0) || (d.stack[1] == 0)) {
            d.stack = ([0]).concat(d.stack.slice(2));
        } else {
            d.stack = ([1]).concat(d.stack.slice(2));
        }
        return {i: i, d: d, g: 1, s: "bool and op", r: (-2)};
    }
    op_code[ops.bool_or] = function(i, code, d) {
        underflow_check(d, 2, "bool_or");
        if ((d.stack[0] == 0) && (d.stack[1] == 0)) {
            d.stack = ([0]).concat(d.stack.slice(2));
        } else {
            d.stack = ([1]).concat(d.stack.slice(2));
        }
        return {i: i, d: d, g: 1, s: "bool or op", r: (-2)};
    }
    op_code[ops.bool_xor] = function(i, code, d) {
        underflow_check(d, 2, "bool_xor");
        var j = 0;
        if ((d.stack[0] == 0) && (d.stack[1] == 0)) {
            j = 0;
        } else if ((d.stack[0] == 0) || (d.stack[1] == 0)) {
            j=1;
        }
        d.stack = ([j]).concat(d.stack.slice(2));
        return {i: i, d: d, g: 1, s: "bool xor op", r: (-2)};
    }
    op_code[ops.stack_size] = function(i, code, d) {
        d.stack = ([d.stack.length]).concat(d.stack);
        return {i: i, d: d, g: 1, s: "stack_size op", r: 2};
    }
    op_code[ops.height] = function(i, code, d) {
        d.stack = ([d.state.height]).concat(d.stack);
        return {i: i, d: d, g: 1, s: "height op", r: 2};
    }
    op_code[ops.gas] = function(i, code, d) {
        d.stack = ([d.op_gas]).concat(d.stack);
        return {i: i, d: d, g: 1, s: "gas op", r: 2};
    }
    op_code[ops.many_vars] = function(i, code, d) {
        d.stack = ([d.vars.length]).concat(d.stack);
        return {i: i, d: d, g: 1, s: "many vars op", r: 2};
    }
    op_code[ops.many_funs] = function(i, code, d) {
        d.stack = (d.many_funs).concat(d.stack);
        return {i: i, d: d, g: 1, s: "many funs op", r: 2};
    }
    op_code[ops.fun_end] = function(i, code, d) {
        return {i: i, d: d, g: 1, s: "fun end op"};
    }
    op_code[ops.set] = function(i, code, d) {
        underflow_check(d, 2, "set");
        d.vars[d.stack[0]] = d.stack[1];
        d.stack = d.stack.slice(2);
        return {i: i, d: d, g: 1, s: "set op"};
    }
    op_code[ops.fetch] = function(i, code, d) {
        underflow_check(d, 1, "fetch");
        var val;
        var foo = d.vars[d.stack[0]];
        if (foo == undefined) {
            val = [];
        } else {
            val = foo;
        }
        d.stack = ([val]).concat(d.stack.slice(1));
        return {i: i, d: d, g: 1, s: "fetch op", r: (1+memory(val))};
    }
    op_code[ops.cons] = function(i, code, d) {
        underflow_check(d, 2, "cons");
        var l = ([d.stack[1]]).concat(
            d.stack[0]);
        d.stack = ([l]).concat(
            d.stack.slice(2));
        return {i: i, d: d, g: 1, s: "cons op", r: 1};
    }
    op_code[ops.car] = function(i, code, d) {
        underflow_check(d, 1, "car");
        if (!(Array.isArray(d.stack[0]))) {
            console.log(JSON.stringify(d.stack));
            throw("car op error");
        } else {
            d.stack = ([d.stack[0].slice(1)]).concat(
                ([d.stack[0][0]])).concat(
                    d.stack.slice(1));
        }
        return {i: i, d: d, g: 1, s: "car op", r: (-1)};
    }
    op_code[ops.empty_list] = function(i, code, d) {
        d.stack = ([[]]).concat(d.stack);
        return {i: i, d: d, g: 1, s: "empty list op", r: 1};
    }
    op_code[ops.append] = function(i, code, d) {
	//console.log("append");
	//console.log(JSON.stringify(d));
        underflow_check(d, 2, "append");
        var a;
	if (Number.isInteger(d.stack[0])) {
	    d.stack[0] = (["binary"]).concat(integer_to_array(d.stack[0], 4));
	}
	if (Number.isInteger(d.stack[1])) {
	    d.stack[1] = (["binary"]).concat(integer_to_array(d.stack[1], 4));
	}
        if (("binary" == d.stack[0][0]) &&
            ("binary" == d.stack[1][0])) {
            a = (d.stack[1]).concat(d.stack[0].slice(1));
            if (a.length == 5) {
                a = array_to_int(a.slice(1));
            }
        } else if (!("binary" == d.stack[0][0]) &&
                   !("binary" == d.stack[1][0])) {
            a = (d.stack[1]).concat(d.stack[0]);
        } else {
            return ["error", "cannot append binary and list together", "append"];
        }
        d.stack = ([a]).concat(
            d.stack.slice(2));
        return {i: i, d: d, g: 1, s: "append op", r: 1};
    }
    op_code[ops.split] = function(i, code, d) {
        underflow_check(d, 2, "split");
        if (!(Array.isArray(d.stack[1]))) {
            //treat the integer like a 4 byte binary
            var n = d.stack[0];
            var bin0 = integer_to_array(d.stack[1], 4);
            var bin1 = bin0.slice(0, n);
            var bin2 = bin0.slice(n, 4);
            d.stack = ([(["binary"]).concat(bin1)]).concat(
                ([(["binary"]).concat(bin2)]).concat(d.stack.slice(2)));
            
        } else if (!(d.stack[1][0] == "binary")) {
            throw("cannot split a list");
        } else {
            var n = d.stack[0];
            var bin1;
            if (n == 4) {
                bin1 = array_to_int(d.stack[1].slice(1, n+1));
            } else {
                bin1 = d.stack[1].slice(0, n+1);
            }
            var bin2;
            if ((d.stack[1].length - n - 1) == 4) {
                bin2 = array_to_int(d.stack[1].slice(n+1));
            } else {
                bin2 = (["binary"]).concat(d.stack[1].slice(n+1));
            }
            d.stack = ([bin1]).concat(
                [bin2]).concat(
                    d.stack.slice(2));
        }
        return {i: i, d: d, g: 1, s: "split op", r: (-1)};
    };
    op_code[ops.reverse] = function(i, code, d) {
        underflow_check(d, 1, "reverse");
        if (d.stack[0][0] == "binary") {
            return ["error", "cannot reverse a binary", "reverse"];
        } else {
            d.stack = ([d.stack[0].reverse()]).concat(
                d.stack.slice(1));
        }
        return {i: i, d: d, g: d.stack[0].length, s: "reverse op"};
    };
    op_code[ops.is_list] = function(i, code, d) {
        var j;
        underflow_check(d, 1, "is_list");
        if (!(d.stack[0].is_array())) {
            j = 0;
        } else if (d.stack[0][0] == "binary") {
            j = 0;
        } else {
            j = 1;
        }
        d.stack = ([j]).concat(d.stack);
        return {i: i, d: d, g: 1, s: "is_list op", r: (-1)};
    };
    op_code[ops.nop] = function(i, code, d) {
        return {i: i, d: d, g: 0, s: "nop op"};
    };
    op_code[ops.fail] = function(i, code, d) {
        op_print(d, i, "fail op");
        op_print(d, i, JSON.stringify(d.stack));
        throw("fail error");
    };
    function run2(code, d) {
        console.log("run 2");
        for (var i = 0; i<code.length; i++) {
            //console.log("run cycle");
            //console.log(i);
            if (d.ram_current > d.ram_most) {
                d.ram_most = d.ram_current;
            }
            if (d.op_gas < 0) {
                console.log(JSON.stringify(d));
                console.log("out of time");
                return ["error", "out of time"];
            } else if (d.ram_current > d.ram_limit) {
                console.log("out of space. limit was: ");
                console.log(d.ram_limit);
                return ["error", "out of space"];
            } else if ((code[i] == ops.call) && (code[i+1] == ops.fun_end)){
                //tail call optimized function call
                //console.log("tail call optimized function call op");
                //console.log(d.stack[0]);
                definition = d.funs[d.stack[0]];
                var s = definition.length;
                d.op_gas = d.op_gas - s - 10;
                d.ram_current = d.ram_current + s - 1;
                d.stack = d.stack.slice(1);
                code = definition.concat(code.slice(i+1));
                i = 0;
                op_print(d, i, "optimized call op");
                //return run2(definition.concat(rest), d);
            } else if (code[i] == ops.finish) {
                op_print(d, i, "return op");
                return d;
            } else if ((!(code[i] < ops.add)) && (code[i] < ops.eq)) {
                //console.log("arithmetic");
                underflow_check(d, 2, "arithmetic");
                d.op_gas = d.op_gas - 1;
                d.ram_current = d.ram_current - 2;
                var a = arithmetic_chalang(code[i], d.stack[0], d.stack[1]);
                d.stack = a.concat(d.stack.slice(2));
                op_print(d, i, ("math ").concat((code[i]).toString()));
            } else {
                var y = op_code[code[i]](i, code, d);
		//console.log("return after calling function definition ");
		//console.log(JSON.stringify(y));
                i = y.i;
                d = y.d;
                d.op_gas -= y.g;
                if (!(y.r == undefined)) {
                    d.ram_current += y.r;
                }
                op_print(d, i, y.s);
            }
        }
        return d;
    }
    function is_balanced_f(code) {
        var x = 0;
        for (var i = 0; i<code.length; i++) {
            if (code[i] == ops.int_op) {
                i += 4;
            } else if (code[i] == ops.binary_op) {
                n = array_to_int(code.slice(i+1, i+5));
                i += (4 + n);
            } else if ((code[i] == ops.define) && (x == 0)){
                x = 1;
            } else if ((code[i] == ops.fun_end) && (x == 1)) {
                x = 0;
            } else if ((code[i] == ops.define) || (code[i] == ops.fun_end)) {
                return false;
            }
        }
        return true;
    }
    function run5(code, d) {
	//console.log("run5 ");
	//console.log(JSON.stringify(code));
        if (is_balanced_f(code)) {
            return run2(code, d);
        } else {
            throw("misformed function. : ; ");
        }
    }
        //these are some compiled contracts from chalang/src/forth/. the chalang repository.
        //each of these test contracts should return a stack like this: [1]
    var hashlock_contract =
        [2,0,0,0,32,169,243,219,139,234,91,46,239,146,55,229,72,9,221,164,63,12,33,143,128,208,211,40,163,63,91,76,255,255,51,72,230,40,10,
         2,0,0,0,32,67,235,55,16,65,154,38,188,176,22,150,20,54,17,182,74,255,87,231,241,254,236,126,177,29,146,149,153,232,73,80,204,
         ops.print,ops.eq,ops.swap,ops.drop,ops.swap,ops.drop];
    
    var verify_signature_contract =
        [2,0,0,0,71,48,69,2,32,112,134,203,180,124,166,163,247,
         94,210,211,101,253,157,198,109,165,100,230,213,193,22,
         236,82,240,187,161,163,143,174,252,77,2,33,0,252,160,42,
         76,157,218,69,96,18,53,9,86,91,223,194,87,4,167,121,112,
         117,103,139,226,37,133,252,41,247,43,137,118, //this is the signature.
         2,0,0,0,3,1,2,3, //this is the data
         2,0,0,0,65,4,133,89,134,205,122,130,218,16,254,
         229,12,186,57,121,105,43,173,164,137,130,226,246,188,49,
         236,32,10,247,161,232,193,46,14,58,3,190,212,42,97,158,
         69,121,135,20,133,143,208,46,58,66,6,181,227,170,244,
         237,22,35,120,150,45,13,134,58, //this is the pubkey
         ops.print,ops.verify_sig];
    var function_contract =
        [ops.define,ops.dup,ops.mul,ops.fun_end, //square
         ops.define, //quad
         2,0,0,0,12,239,24,7,129,222,179,141,148,74,245,17,98, 
         ops.call, //square
         2,0,0,0,12,239,24,7,129,222,179,141,148,74,245,17,98,
         ops.call, //square
         ops.fun_end,
         0,0,0,0,2,
         2,0,0,0,12,248,21,87,89,106,92,199,6,67,69,197,184,
         ops.call, //quad
         0,0,0,0,16,
         ops.eq,ops.swap,ops.drop,ops.swap,ops.drop];
    var function_contract2 =
	[
	    ops.define,ops.dup,ops.fun_end,2,0,0,0,12,
	    47,15,209,232,155,141,225,213,114,146,116,46,
	    0,0,0,0,1,ops.swap,
	    ops.call,

	    ops.define, ops.empty_list, ops.eqs,
	    ops.caseif,
	    ops.drop, ops.drop, 0,0,0,0,0,
	    ops.caseelse, ops.drop,
	    ops.car, ops.swap, 0,0,0,0,0, ops.eqs,
	    ops.caseif,
	    ops.drop, ops.drop, 0,0,0,0,1,
	    ops.caseelse,
	    ops.drop, ops.drop, ops.recurse, ops.call,
	    ops.casethen,
	    ops.casethen,
	    ops.fun_end //gItO8iDvFskzdl9c
	];
    var variable_contract =
        [0,0,0,0,12,
         0,0,0,0,1,
         ops.set,
         0,0,0,0,11,
         0,0,0,0,2,
         ops.set,
         0,0,0,0,1,
         ops.fetch,
         ops.print,
         0,0,0,0,1,
         ops.fetch,
         0,0,0,0,10,
         0,0,0,0,1,
         ops.set,
         0,0,0,0,1,
         ops.fetch,
         0,0,0,0,2,
         ops.fetch,
         0,0,0,0,11,
         ops.eq,ops.to_r,ops.drop,ops.drop,
         0,0,0,0,10,
         ops.eq,ops.to_r,ops.drop,ops.drop,
         0,0,0,0,12,
         ops.eq,ops.to_r,ops.drop,ops.drop,
         0,0,0,0,12,
         ops.eq,ops.to_r,ops.drop,ops.drop,
         ops.from_r,ops.from_r,ops.from_r,ops.from_r,
         ops.bool_and,ops.bool_and,ops.bool_and
        ];
    var map_contract =
        [ops.define,ops.dup,ops.mul,ops.fun_end, //square
         ops.define,//map2
         ops.car,ops.swap,
         0,0,0,0,1,
         ops.fetch,ops.call,ops.rot,ops.cons,ops.swap,
         ops.empty_list,ops.eq,ops.caseif,
         ops.drop,ops.drop,ops.reverse,
         ops.caseelse,
         ops.drop,ops.recurse,ops.call,
         ops.casethen,
         ops.fun_end,
         ops.define, //map
         0,0,0,0,1,
         ops.set,ops.empty_list,ops.swap,
         ops.binary_op,0,0,0,12,
         71,192,142,101,22,36,27,88,17,55,152,169,
         ops.call,
         ops.fun_end,
         ops.empty_list,
         0,0,0,0,5,
         ops.swap,ops.cons,
         0,0,0,0,6,ops.swap,ops.cons,
         0,0,0,0,7,
         ops.swap,ops.cons,ops.reverse,
         2,0,0,0,12,239,24,7,129,222,179,141,148,74,245,17,98,
         2,0,0,0,12,53,181,176,16,58,242,45,201,243,134,253,139,
         ops.call,
         ops.empty_list,
         0,0,0,0,25,
         ops.swap,ops.cons,
         0,0,0,0,36,
         ops.swap,ops.cons,
         0,0,0,0,49,
         ops.swap,ops.cons,ops.reverse, ops.print,
         ops.eq,ops.to_r,ops.drop,ops.drop,ops.from_r];
    var recursion_contract =
        [ops.define,
         0,0,0,0,0,ops.eq,ops.bool_flip,
         ops.caseif,
         ops.drop,
         0,0,0,0,1,
         ops.subtract,
         0,0,0,0,0,
         ops.swap,ops.recurse,
         ops.call,
         ops.caseelse,
         20,20,
         ops.casethen,
         ops.fun_end,
         0,0,0,0,5,
         2,0,0,0,12,95,171,14,87,107,52,162,208,56,196,48,154,
         ops.call,
         0,0,0,0,0,
         ops.eq,ops.to_r,ops.drop,ops.drop,
         0,0,0,0,0,
         ops.eq,ops.to_r,ops.drop,ops.drop,
         0,0,0,0,0,
         ops.eq,ops.to_r,ops.drop,ops.drop,
         0,0,0,0,0,
         ops.eq,ops.to_r,ops.drop,ops.drop,
         0,0,0,0,0,
         ops.eq,ops.to_r,ops.drop,ops.drop,
         ops.from_r,ops.from_r,ops.from_r,ops.from_r,ops.from_r,
         ops.bool_and,ops.bool_and,ops.bool_and,ops.bool_and
        ];
    var case_contract = [
        0,0,0,0,0,
        ops.caseif,
        0,0,0,0,3,
        ops.caseif, 0,0,0,0,7,
        ops.caseelse, 0,0,0,0,8, ops.casethen,
        ops.caseif, ops.caseelse, 0,0,0,0,0,
        ops.caseif, ops.caseelse, ops.casethen,
        ops.casethen,
        ops.caseelse,
        0,0,0,0,0,
        ops.caseif,
        0,0,0,0,3,
        ops.caseelse,
        0,0,0,0,4,
        ops.casethen,
        0,0,0,0,27,
        ops.casethen
    ];
    var split_append_contract = [
        //should return <<2,3,1>>
        ops.binary_op, 0,0,0,3, 1,2,3,
        ops.int_op, 0,0,0,1,
        ops.split, ops.append
    ];
    function chalang_test() {
        var d = data_maker(1000, 1000, 50, 1000, [], [], new_state(0, 0));
        console.log("chalang test");
        //var x = run5(verify_signature_contract, d);
        //var x = run5(case_contract, d);
        //var x = run5(hashlock_contract, d);
        //var x = run5(split_append_contract, d);
        //var x = run5(recursion_contract, d);
        //var x = run5(variable_contract, d);
        //var x = run5(function_contract, d);
        var x = run5(function_contract2, d);
        //var x = run5(map_contract, d);
        console.log(JSON.stringify(x.stack));
        return x.stack;
    }
    function new_state(height, slash) {
        return{"name": "state", "height": height, "slash": slash};
    }
    function data_maker(op_gas, ram_gas, many_vs, many_funs, script_sig, code, state) {
	console.log("data maker vars ");
	console.log(many_vs);
        var arr = [];
        arr.length = Math.min(200, many_vs);
        return {"name": "d", "op_gas":op_gas, "stack": [], "alt": [], "ram_most": 0, "ram_limit":ram_gas, "vars": arr, "funs":{}, "many_funs": 0, "fun_limit":many_funs, "ram_current":(script_sig.length + code.length), "state":state};
    }
    return {run5: run5,
            test: chalang_test,
            ops: function() {return(ops);},
            new_state: new_state,
            data_maker: data_maker};
}

var chalang_object = chalang_main();
//var foo = chalang_object.test();//this is how you make the test run.
//console.log(JSON.stringify(foo));

