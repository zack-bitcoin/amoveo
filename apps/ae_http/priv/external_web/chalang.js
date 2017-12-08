function chalang_new_state(height, slash) {
    return {"name": "state", "height": height, "slash": slash};
}
function chalang_data_maker(op_gas, ram_gas, many_vs, many_funs, script_sig, code, state) {
    var arr = [];
    arr.length = many_vs;
    return {"name": "d", "op_gas":op_gas, "stack": [], "alt": [], "ram_most": 0, "ram_limit":ram_gas, "vars": arr, "funs":{}, "many_funs": 0, "fun_limit":many_funs, "ram_current":(script_sig.length + code.length), "state":state};
}
//function run5(code, d) {
function chalang(command) {
    const int_op = 0,
          binary_op = 2,
          print = 10,
          crash = 11,
          nop = 12,
          fail = 13,
          drop = 20,
          dup = 21,
          swap = 22,
          tuck = 23,
          rot = 24,
          ddup = 25,
          tuckn = 26,
          pickn = 27,
          to_r = 30,
          from_r = 31,
          r_fetch = 32,
          hash_op = 20,
          verify_sig = 41,
          add = 50,
          subtract = 51,
          mul = 52,
          div = 53,
          gt = 54,
          lt = 55,
          pow = 56,
          rem = 57,
          eq = 58,
          caseif = 70,
          caseelse = 71,
          casethen = 72,
          bool_flip = 80,
          bool_and = 81,
          bool_or = 82,
          bool_xor = 83,
          bin_and = 84,
          bin_or = 85,
          bin_xor = 86,
          stack_size = 80,
          height = 94,
          gas = 96,
          ram = 97,
          many_vars = 100,
          many_funs = 101,
          define = 110,
          fun_end = 111,
          recurse = 112,
          call = 113,
          set = 120,
          fetch = 121,
          cons = 130,
          car = 131,
          empty_list = 132,
          append = 134,
          split = 135,
          reverse = 136,
          is_list = 137,
          word_size = 4294967296,
          hash_size = 12;
    function run2(code, d) {
        //the stuff inside this function is from the chalang repository.
        function memory(x) {
            //console.log("in memory function");
            //console.log(JSON.stringify(x));
            if (JSON.stringify(x) == JSON.stringify([])) {
                return 1;
            } else if (Number.isInteger(x)) {
                return 4;
            } else if (x[0] == "binary") {
                return x.length - 1;
            } else {
                var a = memory(x[0]);
                var y = x.slice(1, x.length);
                var b = memory(y);
                return a+b;
            }
        }
        function underflow_check(d, min_size, op_name, continuation) {
            if (d.stack.length < min_size) {
                throw(JSON.stringify(["error", "stack underflow", op_name]));
            } else {
                continuation();
                return 0;
            }
        }
        function arithmetic_chalang(op, a, b) { //returns a list to concat with stack.
            function exponential(b, a) {
                if (b == 0) {
                    return 0;
                } else if (a == 0) {
                    return 1;
                }
                var n = 1;
                while (!(a == 1)) {
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
            var x;
            if (op == add) {
                x = a + b;
            } else if (op == subtract) {
                x = b - a;
            } else if (op == mul) {
                x = b * a;
            } else if (op == div) {
                x = Math.floor(b / a);
            } else if (op == pow) {
                x = exponential(b, a);
            } else if (op == rem) {
                x = b % a;
            } else if (op == gt) {
                if (a < b) {
                x = 1;
                }
            } else if (op == lt) {
                if (a > b) {
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
            return h.slice(0, 12);
        }
        function count_till(code, i, opcode) {
            for (var j = 0; (j + i) < code.length; j++) {
                if ((code[i+j]) == int_op) {
                    j += 4;
                } else if (code[i+j] == binary_op) {
                    var h = array_to_int(code.slice(i+j+1, i+j+5));
                    j += (4 + h);
                    //}
                    /*
                    // we don't need this part because chalang doesn't support if-statements inside of if-statements. 
                    } else if ((code[i+j] == caseif) && (1 == 2)){
                    console.log("count till caseif");
                    var k = count_till(code, i+j+1, casethen);
                    console.log("k is ");
                    console.log(k);
                    j += (k);
                    */
                } else if (opcode == code[i+j]) {
                    return j;
                }
            }
            console.log("count till reached end without finding goal");
            console.log(opcode);
            throw(error);
        }
        function replace(old_character, new_code, binary) {
            for (var i = 0; i < binary.length; i++) {
                if (binary[i] == old_character) {
                    var r2 = replace(old_character, new_code, binary.slice(i+1, binary.length));
                    return binary.slice(0,i).concat(new_code).concat(r2);
                } else if (binary[i] == int_op) {
                    i += 4;
                } else if (binary[i] == binary_op) {
                    var h = array_to_int(binary.slice(i+1, i+5));
                    i += (4 + h);
                }
            }
            return binary;
        }
        var error_check = 0;
        for (var i = 0; i<code.length; i++) {
            //console.log"run cycle");
            //console.logi);
            //console.log"opcode: ");
            //console.logcode[i]);
            //console.logJSON.stringify(code));
            //console.logJSON.stringify(d.stack));
            if (!(error_check == 0)) {
                console.log("error_check");
                console.log(error_check);
                return error_check;
            }
            if (d.ram_current > d.ram_most) {
                d.ram_most = d.ram_current;
            }
            if (d.op_gas < 0) {
                console.log("out of time");
                return ["error", "out of time"];
            } else if (d.ram_current > d.ram_limit) {
                console.log("out of space. limit was: ");
                console.log(d.ram_limit);
                return ["error", "out of space"];
            } else if (code[i] == int_op) {
                //console.log("int op");
                var int_array = code.slice(i+1, i+5);
                var new_int = array_to_int(int_array);
                d.stack = ([new_int]).concat(d.stack);
                d.ram_current = d.ram_current + 1;
                d.op_gas = d.op_gas - 1;
                i = i + 4;
            } else if (code[i] == binary_op) {
                //console.log("bin op");
                var int_array = code.slice(i+1, i+5);
                var new_int = array_to_int(int_array);
                var bin_array = code.slice(i+5, i+5+new_int);
                d.stack = ([(["binary"]).concat(
                    bin_array)]).concat(
                        d.stack);
                d.ram_current += 1;
                d.op_gas -= new_int;
                i = i + 4 + new_int;
            } else if (code[i] == caseif) {
                //console.log("if op");
                var b = d.stack[0];
                var skipped_size;
                var size_case1 = count_till(code, i, caseelse);
                var size_case2 = count_till(code, i + size_case1, casethen);
                if (b == 0) {
                    skipped_size = size_case1;
                    i += skipped_size;
                    //maybe we should remove the case_then from code, that way we can do tail optimized recursion after a conditional.
                } else {
                    var skipped_size = size_case2;
                }
                d.stack = d.stack.slice(1, d.stack.length);
                d.ram_current -= (skipped_size + 1);
                d.op_gas -= (size_case1 + size_case2);
            } else if (code[i] == caseelse) {
                //console.log("else op");
                var skipped_size = count_till(code, i, casethen);
                i += skipped_size;
            } else if (code[i] == casethen) {
                //console.log("then op");
                // do nothing.
            } else if ((code[i] == call) && (code[i+1] == fun_end)){
                //tail call optimized function call
                //console.log("tail call optimized function call op");
                //console.log(d.stack[0]);
                definition = d.funs[d.stack[0]];
                var s = definition.length;
                d.op_gas = d.op_gas - s - 10;
                d.ram_current = d.ram_current + s - 1;
                d.stack = d.stack.slice(1, d.stack.length);
                code = definition.concat(code.slice(i+1, code.length));
                i = 0;
                //return run2(definition.concat(rest), d);
            } else if (code[i] == call) {
                //non-optimized function call.
                //console.log("function call op");
                //console.log(d.stack[0]);
                //console.log(JSON.stringify(d.stack));
                //console.log(d.funs);
                var code_hash = btoa(array_to_string(d.stack[0].slice(1, d.stack[0].length)));
                //console.log(code_hash);
                definition = d.funs[code_hash];
                var s = definition.length;
                d.op_gas = d.op_gas - s - 10;
                d.ram_current = d.ram_current + s - 1;
                d.stack = d.stack.slice(1, d.stack.length);
                d = run2(definition, d);
            } else if (code[i] == define) {
                //console.log("define op");
                var skipped_size = count_till(code, i, fun_end);
                var definition = code.slice(i+1, i+skipped_size);
                i = i + skipped_size;
                var hash_array = small_hash(definition);
                var b = btoa(array_to_string(hash_array));
                var definition2 = replace(recurse, ([binary_op]).concat(integer_to_array(hash_size, 4)).concat(hash_array), definition);
                //console.log("definition 2 of new function");
                //console.log(definition2);
                d.funs[b] = definition2;
                var s = definition2.length + 4;
                var mf = d.many_funs + 1;
                if (mf > d.fun_limit) {
                    return ["error", "too many functions"];
                } else {
                    d.op_gas = d.op_gas - s - 30;
                    d.ram_current = d.ram_current + (2 * s);
                    d.many_funs = mf;
                }
            } else if (code[i] == crash) {
                console.log("crash op");
                return d;
            } else if (code[i] == print) {
                console.log("print op");
                console.log(JSON.stringify(d.stack));
            } else if (code[i] == drop) {
                //console.log("drop op");
                //console.log(JSON.stringify(d.stack));
                error_check = underflow_check(d, 1, "drop", function() {
                    d.ram_current = d.ram_current - memory(d.stack[0]) - 2;
                    d.stack = d.stack.slice(1, d.stack.length);
                    d.op_gas = d.op_gas - 1;
                });
            } else if (code[i] == dup) {
                //console.log("dup op");
                error_check = underflow_check(d, 1, "dup", function() {
                    d.stack = ([d.stack[0]]).concat(d.stack);
                    d.ram_current = d.ram_current + memory(d.stack[0]);
                    d.op_gas = d.op_gs - 1;
                });
            } else if (code[i] == swap) {
                //console.log("swap op");
                error_check = underflow_check(d, 2, "swap", function() {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.stack[1]]).concat(
                        [d.stack[0]]).concat(
                            d.stack.slice(2, d.stack.length));
                });
            } else if (code[i] == tuck) {
                //console.log("tuck op");
                error_check = underflow_check(d, 3, "tuck", function() {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.stack[1]]).concat(
                        [d.stack[2]]).concat(
                            [d.stack[0]]).concat(
                                d.stack.slice(3, d.stack.length));
                });
            } else if (code[i] == rot) {
                //console.log("rot op");
                error_check = underflow_check(d, 3, "rot", function() {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.stack[2]]).concat(
                        [d.stack[0]]).concat(
                            [d.stack[1]]).concat(
                                d.stack.slice(3, d.stack.length));
                });
            } else if (code[i] == ddup) {
                //console.log("ddup op");
                error_check = underflow_check(d, 2, "ddup", function() {
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current + memory(d.stack[0]) + memory(d.stack[1]);
                    d.stack = d.stack.slice(0, 2).concat(d.stack);
                });
            } else if (code[i] == tuckn) {
                //console.log("tuckn op");
                if (d.stack.length < 2) {
                    return ["error", "stack underflow", "tuckn"];
                } else {
                    var n = d.stack[0];
                    error_check = underflow_check(d, 2+n,"tuckn",function(){
                        d.op_gas = d.op_gas - 1;
                        d.stack = d.stack.slice(2, 2+n).concat(
                            [d.stack[1]]).concat(
                                d.stack.slice(3+n, d.stack.length));
                    });
                }
            } else if (code[i] == pickn) {
                //console.log("pickn op");
                var n = d.stack[0];
                if (d.stack.length < (n + 1)) {
                    return ["error", "stack underflow", "pickn"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.stack[n]]).concat(
                        d.stack.slice(1, 1+n)).concat(
                            d.stack.slice(2+n, d.stack.length));
                }
            } else if (code[i] == to_r) {
                //console.log(">r op");
                error_check = underflow_check(d, 1, "to_r", function() {
                    d.op_gas = d.op_gas - 1;
                    d.alt = ([d.stack[0]]).concat(d.alt);
                    d.stack = d.stack.slice(1, d.stack.length);
                });
            } else if (code[i] == from_r) {
                //console.log("r> op");
                if (d.alt.length < 1) {
                    return ["error", "alt stack underflow", "from_r"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.alt[0]]).concat(d.stack);
                    d.atl = d.alt.slice(1, d.alt.length);
                }
            } else if (code[i] == r_fetch) {
                //console.log("r@ op");
                if (d.alt.length < 1) {
                    return ["error", "alt stack underflow", "r_fetch"];
                } else {
                    op_gas = d.op_gas - 1;
                    d.stack = ([d.alt[0]]).concat(d.stack);
                }
            } else if (code[i] == hash_op) {
                //console.log("hash op");
                error_check = underflow_check(d, 1, "hash", function() {
                    d.op_gas = d.op_gas - 20;
                    d.stack = ([hash(d.stack[0])]).concat(
                        d.stack.slice(1, d.stack.length));
                });
            } else if (code[i] == verify_sig) {
                //console.log("verify_sig op");
                error_check = underflow_check(d, 3, "verify_sig", function(){
                    //data, sig, key
                    console.log(JSON.stringify(d.stack));
                    var pub1 = d.stack[0].slice(1, d.stack[0].length);//internal format puts "binary" at the front of each binary.
                    var data1 = d.stack[1].slice(1, d.stack[1].length);
                    var sig1 = d.stack[2].slice(1, d.stack[2].length);
                    temp_key = ec.keyFromPublic(toHex(array_to_string(pub1)), "hex");
                    var sig2 = bin2rs(array_to_string(sig1));
                    var b = temp_key.verify(hash(serialize(data1)), sig2, "hex")
                    var c;
                    if (b) {
                        c = 1;
                    } else {
                        c = 0;
                    }
                    d.op_gas = d.op_gas - 20;
                    d.stack = ([c]).concat(
                        d.stack.slice(3, d.stack.length));
                });
            } else if ((!(code[i] < add)) && (code[i] < eq)) {
                //console.log("arithmetic");
                error_check = underflow_check(d, 2, "arithmetic", function(){
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current - 2;
                    var a = arithmetic_chalang(code[i], d.stack[0], d.stack[1]);
                    d.stack = a.concat(d.stack.slice(2, d.stack.length));
                });
            } else if (code[i] == eq) {
                //console.log("eq op");
                error_check = underflow_check(d, 2, "eq", function(){
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current + 1;
                    if (JSON.stringify(d.stack[0]) == JSON.stringify(d.stack[1])) {
                        d.stack = ([1]).concat(d.stack);
                    } else {
                        d.stack = ([0]).concat(d.stack);
                    }
                                       
                });
            } else if (code[i] == bool_flip) {
                //console.log("bool flip op");
                error_check = underflow_check(d, 1, "bool_flip", function(){
                    if (d.stack[0] == 0) {
                        d.stack = ([1]).concat(d.stack.slice(1, d.stack.length));
                    } else {
                        d.stack = ([0]).concat(d.stack.slice(1, d.stack.length));
                    }
                    d.op_gas = d.op_gas - 1;
                });
            } else if (code[i] == bool_and) {
                //console.log("bool and op");
                error_check = underflow_check(d, 2, "bool_and", function(){
                    if ((d.stack[0] == 0) || (d.stack[1] == 0)) {
                        d.stack = ([0]).concat(d.stack.slice(2, d.stack.length));
                    } else {
                        d.stack = ([1]).concat(d.stack.slice(2, d.stack.length));
                    }
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current - 2;
                });
            } else if (code[i] == bool_or) {
                //console.log("bool or op");
                error_check = underflow_check(d, 2, "bool_or", function(){
                    if ((d.stack[0] == 0) && (d.stack[1] == 0)) {
                        d.stack = ([0]).concat(d.stack.slice(2, d.stack.length));
                    } else {
                        d.stack = ([1]).concat(d.stack.slice(2, d.stack.length));
                    }
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current - 2;
                });
            } else if (code[i] == bool_xor) {
                //console.log("bool xor op");
                error_check = underflow_check(d, 2, "bool_xor", function(){
                    var j = 0;
                    if ((d.stack[0] == 0) && (d.stack[0] == 0)) {
                        j = 0;
                    } else if ((d.stack[0] == 0) || (d.stack[0] == 0)) {
                        j=1;
                    }
                    d.stack = ([j]).concat(d.stack.slice(2, d.stack.length));
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current - 2;
                });
            } else if (code[i] == stack_size) {
                //console.log("bool stack_size op");
                d.op_gas = d.op_gas - 1;
                d.ram_current = d.ram_current + 2;
                d.stack = ([d.stack.length]).concat(d.stack);
            } else if (code[i] == height) {
                //console.log("bool height op");
                d.op_gas = d.op_gas - 1;
                d.ram_current = d.ram_current + 2;
                d.stack = ([d.state.height]).concat(d.stack);
            } else if (code[i] == gas) {
                //console.log("bool gas op");
                d.op_gas = d.op_gas - 1;
                d.stack = ([d.op_gas]).concat(d.stack);
                d.ram_current += 2;
            } else if (code[i] == many_vars) {
                //console.log("bool many vars op");
                d.op_gas -= 1;
                d.stack = ([d.vars.length]).concat(d.stack);
                d.ram_current += 2;
            } else if (code[i] == many_funs) {
                //console.log("bool many funs op");
                d.op_gas -= 1;
                d.ram_current += 2;
                d.stack = (d.many_funs).concat(d.stack);
            } else if (code[i] == fun_end) {
                //console.log("bool fun end op");
                d.op_gas -= 1;
            } else if (code[i] == set) {
                //console.log("running set");
                error_check = underflow_check(d, 2, "set", function(){
                    //console.log("no underflow error");
                    d.vars[d.stack[0]] = d.stack[1];
                    d.op_gas -= 1;
                    d.stack = d.stack.slice(2, d.stack.length);
                });
            } else if (code[i] == fetch) {
                //console.log("fetch op");
                //console.log(JSON.stringify(d.stack));
                error_check = underflow_check(d, 1, "fetch", function(){
                    var val;
                    var foo = d.vars[d.stack[0]];
                    if (foo == undefined) {
                        val = [];
                    } else {
                        val = foo;
                    }
                    d.op_gas -= 1;
                    d.ram_current += (1 + memory(val));
                    d.stack = ([val]).concat(d.stack.slice(1, d.stack.length));
                });
            } else if (code[i] == cons) {
                //console.log("bool cons op");
                error_check = underflow_check(d, 2, "cons", function(){
                    d.op_gas -= 1;
                    d.ram_current += 1;
                    var l = ([d.stack[1]]).concat(
                        d.stack[0]);
                    d.stack = ([l]).concat(
                        d.stack.slice(2, d.stack.length));
                });
            } else if (code[i] == car) {
                //console.log("car op");
                error_check = underflow_check(d, 1, "car", function(){
                    if (!(Array.isArray(d.stack[0]))) {
                        console.log(JSON.stringify(d.stack));
                        throw("car op error");
                    } else {
                        d.op_gas -= 1;
                        d.ram_current -= 1;
                        d.stack = ([d.stack[0].slice(1, d.stack[0].length)]).concat(
                            ([d.stack[0][0]])).concat(
                                d.stack.slice(1, d.stack.length));
                    }
                });
            } else if (code[i] == empty_list) {
                d.op_gas -= 1;
                d.ram_current += 1;
                d.stack = ([[]]).concat(d.stack);
            } else if (code[i] == append) {
                error_check = underflow_check(d, 2, "append", function(){
                    var a;
                    if (("binary" == d.stack[0][0]) &&
                        ("binary" == d.stack[1][0])) {
                        a = (d.stack[0]).concat(d.stack[1].slice(1, d.stack[1].length));
                    } else if (!("binary" == d.stack[0][0]) &&
                               !("binary" == d.stack[1][0])) {
                        a = (d.stack[0]).concat(d.stack[1]);
                    } else {
                        return ["error", "cannot append binary and list together", "append"];
                    }
                    d.op_gas -= 1;
                    d.ram_current +- 1;
                    d.stack = (a).concat(
                        d.stack.slice(2, d.stack.length));
                });
            } else if (code[i] == split) {
                error_check = underflow_check(d, 2, "split", function(){
                    if (!(d.stack[0][0] == "binary")) {
                        return ["error", "cannot split a list", "split"]; 
                    } else {
                        d.op_gas -= 1;
                        d.ram_current -= 1;
                        var n = d.stack[0];
                        var bin1 = d.stack[1].split(0, n+1);
                        var bin2 = (["binary"]).concat(d.stack[1].split(n+1, d.stack[1].length));
                        d.stack = [bin1].concat(
                            [bin2]).concat(
                                d.stack.slice(2, d.stack.length));
                    }});
            } else if (code[i] == reverse) {
                error_check = underflow_check(d, 1, "reverse", function(){
                    if (d.stack[0][0] == "binary") {
                        return ["error", "cannot reverse a binary", "reverse"];
                    } else {
                        d.op_gas -= d.stack[0].length;
                        d.stack = ([d.stack[0].reverse()]).concat(
                            d.stack.slice(1, d.stack.length));
                    }});
            } else if (code[i] == is_list) {
                var j;
                error_check = underflow_check(d, 1, "is_list", function(){
                    if (!(d.stack[0].is_array())) {
                        j = 0;
                    } else if (d.stack[0][0] == "binary") {
                        j = 0;
                    } else {
                        j = 1;
                    }
                    d.op_gas -= 1;
                    d.ram_current -= 1;
                    d.stack = ([j]).concat(d.stack);
                });
            } else if (code[i] == nop) {
            } else if (code[i] == fail) {
                return ["error", "fail opcode", "fail"];
            }
        }
        return d;
    }
    function run5(code, d) {
        function is_balanced_f(code) {
            var x = 0;
            for (var i = 0; i<code.length; i++) {
                if ((code[i] == define) && (x == 0)){
                    x = 1;
                } else if ((code[i] == fun_end) && (x == 1)) {
                    x = 0;
                } else if ((code[i] == define) || (code[i] == fun_end)) {
                    return false;
                }
            }
            return true;
        }
        if (is_balanced_f(code)) {
            return run2(code, d);
        } else {
            throw("misformed function. : ; ");
        }
    }
    function chalang_test() {
        //these are some compiled contracts from chalang/src/forth/. the chalang repository.
        var d = chalang_data_maker(1000, 1000, 50, 1000, [], [], chalang_new_state(0, 0));
        console.log("chalang test");
        //each of these test contracts should return a stack like this: [1]
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
             print,verify_sig];
        var function_contract =
            [define,dup,mul,fun_end, //square
             define, //quad
               2,0,0,0,12,239,24,7,129,222,179,141,148,74,245,17,98, 
               call, //square
               2,0,0,0,12,239,24,7,129,222,179,141,148,74,245,17,98,
               call, //square
             fun_end,
             0,0,0,0,2,
             2,0,0,0,12,248,21,87,89,106,92,199,6,67,69,197,184,
             call, //quad
             0,0,0,0,16,
             eq,swap,drop,swap,drop];
        var variable_contract =
            [0,0,0,0,12,
             0,0,0,0,1,
             set,
             0,0,0,0,11,
             0,0,0,0,2,
             set,
             0,0,0,0,1,
             fetch,
             print,
             0,0,0,0,1,
             fetch,
             0,0,0,0,10,
             0,0,0,0,1,
             set,
             0,0,0,0,1,
             fetch,
             0,0,0,0,2,
             fetch,
             0,0,0,0,11,
             eq,to_r,drop,drop,
             0,0,0,0,10,
             eq,to_r,drop,drop,
             0,0,0,0,12,
             eq,to_r,drop,drop,
             0,0,0,0,12,
             eq,to_r,drop,drop,
             from_r,from_r,from_r,from_r,
             bool_and,bool_and,bool_and
            ];
        var map_contract =
            [define,dup,mul,fun_end, //square
             define,//map2
               car,swap,
               0,0,0,0,1,
               fetch,call,rot,cons,swap,
               empty_list,eq,caseif,
                 drop,drop,reverse,
               caseelse,
                 drop,recurse,call,
               casethen,
             fun_end,
             define, //map
               0,0,0,0,1,
               set,empty_list,swap,
               binary_op,0,0,0,12,
               71,192,142,101,22,36,27,88,17,55,152,169,
               call,
             fun_end,
             empty_list,
             0,0,0,0,5,
             swap,cons,
             0,0,0,0,6,swap,cons,
             0,0,0,0,7,
             swap,cons,reverse,
             2,0,0,0,12,239,24,7,129,222,179,141,148,74,245,17,98,
             2,0,0,0,12,53,181,176,16,58,242,45,201,243,134,253,139,
             call,
             empty_list,
             0,0,0,0,25,
             swap,cons,
             0,0,0,0,36,
             swap,cons,
             0,0,0,0,49,
             swap,cons,reverse, print,
             eq,to_r,drop,drop,from_r];
        var recursion_contract =
            [
                define,
                  0,0,0,0,0,eq,bool_flip,
                  caseif,
                    drop,
                    0,0,0,0,1,
                    subtract,
                    0,0,0,0,0,
                    swap,recurse,
                    call,
                  caseelse,
                    20,20,
                  casethen,
                fun_end,
                0,0,0,0,5,
                2,0,0,0,12,95,171,14,87,107,52,162,208,56,196,48,154,
                call,
                0,0,0,0,0,
                eq,to_r,drop,drop,
                0,0,0,0,0,
                eq,to_r,drop,drop,
                0,0,0,0,0,
                eq,to_r,drop,drop,
                0,0,0,0,0,
                eq,to_r,drop,drop,
                0,0,0,0,0,
                eq,to_r,drop,drop,
                from_r,from_r,from_r,from_r,from_r,
                bool_and,bool_and,bool_and,bool_and
            ];
        var x = run5(verify_signature_contract, d);
        console.log(JSON.stringify(x.stack));
    }
    function prove_facts(facts, callback) {
        if (JSON.stringify(facts) == JSON.stringify([])) {
            return [empty_list];
        }
        return prove_facts2(facts, 0, [empty_list], callback); // [
    }
    function prove_facts2(facts, i, r, callback) {
        if (i == facts.length) {
            r.concat([reverse]); // converts a , to a ]
            return callback(r);
        }
        var tree = facts[i][1];
        var key = facts [i][2];
        verify_callback(tree, key, function(value) {
            //var value = verify_merkle(key, proof);
            //we are making chalang like this:
            //[ int id, key, binary size serialized_data ]
            // '[', ']', and ',' are macros for making a list.
            r = r.concat([empty_list]); // [
            r = r.concat([0]).concat(integer_to_array(id, 4));
            r = r.concat([swap, cons]); // ,
            if (Integer.isInteger(key)) {
                r = r.concat([0]);
                r = r.concat(integer_to_list(key, 4));
            } else {
                r = r.concat([2]);
                r = r.concat(integer_to_list(key.length, 4));
                r = r.concat(key);
            }
            r = r.concat([swap, cons]); // ,
            var serialized_data;//this is the serialized version of the thing who's existence we are proving. make it from value.
            var s = serialized_data.length;
            r = r.concat([2]).concat([integer_to_list(s, 4)]);
            r = r.concat(serialized_data);
            r = r.concat([swap, cons, reverse]); // ]
            r = r.concat([swap, cons]); // ,
            return prove_facts2(facts, i+1, r, callback);
        });
        return r.concat([reverse]); // converts a , to a ]
    }
    function spk_run(mode, ss, spk, height, slash, fun_limit, var_limit, callback) {
        var state = chalang_new_state(height, slash);
        var key1 = "fun_limit";
        var ret;
        if (!(ss.length == (spk[4].length - 1))) {//spk[4] == bets is formated with a -6 in front for packer.erl
            console.log(JSON.stringify(ss));
            console.log(JSON.stringify(spk[4]));
            throw("ss and bets need to be the same length");
        }
        spk_run2(ss, spk[4], spk[6], spk[5], fun_limit, var_limit, state, spk.delay, 0, 0, 0, function(ret) {
            return callback(ret)
        });
    }
    function spk_run2(ss, bets, opgas, ramgas, funs, vars, state, delay, nonce, amount, i, callback) {
        console.log("spk run 2");
        if (i > ss.length) {
            return callback({"amount": amount, "nonce": nonce, "delay": delay});//, "opgas": opgas});
        }
        spk_run3(ss[i], bets[i], opgas, ramgas, funs, vars, state, function(run_object) {
            return spk_run2(ss, bets, opgas, ramgas, funs, vars, state,
                            Math.max(delay, run_object.delay),
                            nonce + run_object.nonce,
                            amount + run_object.amount,
                            i+1);
        });
    }
    function spk_run3(ss, bet, opgas, ramgas, funs, vars, state, callback) {
        console.log("spk_run3 ss is ");
        console.log(JSON.stringify(ss));
        var script_sig = ss.code;
        if (!(chalang_none_of(script_sig))) {
            throw("error: crash op in the script sig");
        }
        prove_facts(ss.prove, function(f) {
            var c = bet.code;
            var code = f.concat(c);
            console.log("about to make data, var is ");
            console.log(vars);
            var data = chalang_data_maker(opgas, ramgas, vars, funs, script_sig, code, state);
            var data2 = run5(script_sig, data);
            var data3 = run5(code, data2);
            var amount = data3.stack[0];
            var nonce = data3.stack[1];
            var delay = data3.stack[2];
            var cgran;
            if ((amount > cgran) || (amount < -cgran)) {
                throw("you can't spend money you don't have in the channel.");
            }
            var a3 = Math.floor(amount * bet.amount / cgran);
            return callback({"amount": a3, "nonce": nonce, "delay": delay, "opgas": data3.opgas});
        });
    }
    function spk_force_update(spk, ssold, ssnew, fun_limit, var_limit, callback) {
        console.log("force update ss's are ");
        console.log(JSON.stringify([ssold, ssnew]));
        var height = top_header[1];
        var ret;
        spk_run("fast", ssold, spk, height, 0, fun_limit, var_limit, function(ran1) {
            console.log("spk run returned ");
            console.log(JSON.stringify(ran1));
            var nonceOld = ran1.nonce;
            spk_run("fast", ssnew, spk, height, 0, fun_limit, var_limit, function(ran2) {
                var nonceNew = ran2.nonce;
                if (nonceNew > nonceOld) {
                    spk_force_update2(spk[4], ssnew, height, function(updated) {
                        spk[4] = updated.new_bets;
                        spk[8] += updated.amount;
                        spk[9] += updated.nonce;
                        return callback({"spk":spk, "ss":updated.finalss});
                    });
                } else {
                    throw("spk force update error");
                }
            });
        });
    }
    function chalang_none_of(c) {
        var n;
        for (var i = 0; i < c.length; i++) {
            if ( c[i] == crash ) {
                return false;
            } else if ( c[i] == int_op ) {
                i += 4
            } else if ( c[i] == binary_op ) {
                n = array_to_integer(c.slice(i+1, i+5));
                i += (4 + n);
            }
        }
        return true;
    }
    function spk_force_update2(bets, ss, height, callback) {
        var amount = 0;
        var nonce = 0;
        var new_bets = JSON.parse(JSON.stringify(bets));
        var newss = JSON.parse(JSON.stringify(ss));
        var fun_limit = 1000;//config
        var var_limit = 10000;
        var bet_gas_limit = 100000;
        var cgran = 10000; //constants.erl
        spk_force_update22(bets, ss, height, amount, nonce, new_bets, newss, fun_limit, var_limit, bet_gas_limit, bets.length-1, callback);
    }
    function spk_force_update22(bets, ss, height, amount, nonce, new_bets, newss, fun_limit, var_limit, bet_gas_limit, i, callback) {
        if (i < 0) {
            return callback({"new_bets": new_bets, "newss": newss, "amount": amount, "nonce": nonce});
        }
        var b = chalang_none_of(ss[i].code);//ss.code
        if (!(b)) {
            throw("you can't put crash into the ss");
        }
        var state = chalang_new_state(height, 0);
        prove_facts(ss[i].prove, function(f) { //PROBLEM HERE
            var code = f.concat(bets[i].code);
            var data = chalang_data_maker(bet_gas_limit, bet_gas_limit, var_limit, fun_limit, ss.code, code, state);
            var data2 = run5([JSON.parse(JSON.stringify(ss[i].code))], data);
            var data3 = run5([code], data2);
            var s = data3.stack;
            if (!(s[2] > 50)) { //if the delay is long, then don't close the trade.
                if (s[0] > cgran) {
                    throw("you can't spend money that you don't have");
                }
                amount += Math.floor(s[0] * bets[i] / cgran);
                nonce += s[1];
                new_bets = new_bets.slice(0, i).concat(new_bets.slice(i+1, new_bets.length));
                newss = newss.slice(0, i).concat(newss.slice(i+1, newss.length));
            }
            return spk_force_update22(bets, ss, height, amount, nonce, new_bets, newss, fun_limit, var_limit, bet_gas_limit, i-1, callback); 
        });
    }
    function channel_feeder_they_simplify(from, themspk, cd, callback) {
        cd0 = channel_manager[from];
        //true = cd0.live; //verify this is true
        //true = cd.live; //verify this is true
        var spkme = cd0.me;
        var ssme = cd0.ssme;
        console.log("ssme is ");
        console.log(JSON.stringify(ssme));
        //verify that they signed themspk
        var newspk = themspk[1];
        //console.log("spkme is ");
        var newspk2 = cd[1];
        if (!(JSON.stringify(newspk) == JSON.stringify(newspk2))) {
            console.log(JSON.stringify(newspk));
            console.log(JSON.stringify(newspk2));
            throw("spks they gave us do not match");
        }
        var ss = [];
        for (var i = 1; i< cd[3].length; i++) {
            ss = [new_ss(cd[3][i][1], cd[3][i][2])].concat(ss);
        }
        var ss4 = [];
        for (var i = 1; i< cd[4].length; i++) {
            ss4 = [new_ss(string_to_array(atob(cd[4][i][1])), cd[4][i][2])].concat(ss4);
        }
        console.log("ss4 should have a prove");
        console.log(JSON.stringify(ss4));
        console.log("prove should be");
        console.log(JSON.stringify(cd[4][2]));
        console.log("cd is ");
        console.log(JSON.stringify(cd));
        console.log("cd3 is ");
        console.log(JSON.stringify(cd[3]));
        console.log("cd4 is ");
        console.log(JSON.stringify(cd[4]));

        verify_callback("governance", 14, function(fun_limit) {
            verify_callback("governance", 15, function(var_limit) {
                var entropy = cd[7];
                spk_force_update(spkme, ssme, ss4, fun_limit, var_limit, function(b2) {
                    var cid = cd[8];
                    if ( JSON.stringify(b2) == JSON.stringify([newspk, ss])) {
                        var ret = sign_tx(newspk);
                        var newcd = new_cd(newspk, themspk, ss, ss, entropy, cid);
                        channel_manager[from] = newcd;
                        return callback(ret);
                    } else {
                        var b3 = is_improvement(spkme, ssme, newspk, ss, fun_limit, var_limit);
                        if ( b3 ) {
                            ret = sign_tx(newspk);
                            var newcd = new_cd(newspk, themspk, ss, ss, entropy, cid);
                            channel_manager[from] = newcd;
                            return callback(ret);
                        } else {
                            console.log("this part should not happen until we start programming lightning");
                            return false;
                        //this part is only used for lightning.
                        /*
                          var sh=channel_feeder_simplify_helper(From, ss4);
                          var ss5 = sh.ss;
                          var ret = sh.ret;
                          var spk = themspk[1];
                          var spk2 = ret[1];
                          if (!( JSON.stringify(spk) == JSON.stringify(spk2))) {
                          console.log("spks do not match");
                          } else {
                          var data = new_cd(spk, themspk, ss5, ss5, entropy, cid);
                          channel_manager[from] = data;
                          return ret;
                          }
                        */
                        }
                    }
                });
            });
        });
    }
    function is_improvement(old_spk, old_ss, new_spk, new_ss, fun_limit, var_limit) {
        //get height
        //check that space gas and time limit are below or equal to what is in the config file.
        if (new_spk[5] > 100000) {//space gas
            console.log("this contract uses too much space.");
            return false;
        }
        if (new_spk[6] > 100000) {//time gas
            console.log("this contract uses too much time");
            return false;
        }
        new_spk[5];
        new_spk[6];
        spk_run("fast", new_ss, new_spk, height, 0, function(run2) {
            var nonce2 = run2.nonce;
            var delay2 = run2.delay;
            spk_run("fast", old_ss, old_spk, height, 0, function(run1) {
                var nonce1 = run1.nonce;
                if (!(nonce2 > nonce1)) {
                    console.log("the new spk can't produce a lower nonce than the old.");
                    return false;
                }
                var old_bets = old_spk[4];
                var old_amount = old_spk[8];
                old_spk[4] = new_spk[4];
                old_spk[6] = tg;
                old_spk[5] = sg;
                old_spk[8] = new_spk[8];
                old_spk[9] = new_spk[9];
                if (!(JSON.stringify(old_spk) == JSON.stringify(new_spk))) {
                    console.log("spk was changed in unexpected ways");
                    return false;
                }
                var cid = new_spk[7];
                var ret = false;
                verify_callback("channels", cid, function(channel) {
                    //variable_public_get(["proof", btoa("channels"), cid, btoa(array_to_string(top_hash))], function(proof) {
                    var channel = verify_merkle(cid, proof);
                    var acc1 = channel[2]
                    var acc2 = channel[3]
                    var profit;
                    if (pubkey_64() == acc1) {
                        profit = new_spk[8] - old_amount;
                    } else {
                        profit = old_amount - new_spk[8];
                    }
                    var bets2 = new_spk[4];
                    if ((JSON.stringify(old_bets) == JSON.stringify(bets2)) && (profit > 0)) {
                        //if they give us money for no reason, then accept.
                        ret = true;
                        return 0;
                    }
                    if ((!(profit < 0)) && //costs nothing
                        ((new_spk[4].length - old_bets.length) > 0)) { //increases number of bets
	                //if we have the same or greater amount of money, and they make a bet that possibly gives us more money, then accept it.
                        var new_bet = bets2[0];
                        var t = bets2.slice(1, bets2.length);
                        if (!(JSON.stringify(t) == old_bets)) {
                            console.log("we can only absorb one bet at a time this way.");
                            ret = false;
                            return 0;
                        }
                        var betAmount = new_bet.amount;
                        var potentialGain;
                        if (pubkey_64() == acc1) {
                            potentialGain = -betAmount;
                        } else if (pubkey_64() == acc2) {
                            potentialGain = betAmount;
                        } else {
                            console.log("error, this spk isn't for your pubkey")
                            ret = false;
                            return 0;
                        }
                        if (!(potentialGain > 0)) {
                            ret = false;
                            return 0;
                        }
                        var obligations1 = spk_obligations(1, bets2);
                        var obligations2 = spk_obligations(2, bets2);
                        var channelbal1 = channel[4];
                        var channelbal2 = channel[5];
                        if (obligations1 > channelbal1) {
                            console.log("acc1 doesn't have enough money in the channel to make that bet");
                            ret = false;
                            return 0;
                        }
                        if (obligations2 > channelbal2) {
                            console.log("acc2 doesn't have enough money in the channel to make that bet");
                            ret = false;
                            return 0;
                        }
                        ret = true;
                        return 0;
                    }
                    ret = false;
                    return 0;
                });
                return ret;
            });
        });
    }
        function spk_obligations(n, bets) {
            var x = 0;
            for (var i = 0; i < n; i++) {
            var b = bets[i].amount;
            if (b > 0) {
                if (b > 0) {
                    x += b;
                }
            } else if (n == 2) {
                if (b < 0) {
                    x -= b;
                }
            } else {
                throw("spk_obligations error");
            }
        }
        return x;
    }
    function pull_channel_state() {
        //get their pubkey
        variable_public_get(["pubkey"], function(server_pubkey) {
            variable_public_get(["spk", pubkey_64()], function(spk_return) {
                var cd = spk_return[1];
                var them_spk = spk_return[2];
                //returns cd and them_spk
                var cd0 = channel_manager[server_pubkey];
                if (cd0 == undefined) {
                    console.log("you don't have a record of a channel with this server. Did you load your channel data file?");
                    throw("pull channel state error");
                }
                /*
                  if (!(cd0.live == true)) {
                    var s = "this channel has been closed";
                    console.log(s);
                    throw(s);
                }
                */
                channel_feeder_they_simplify(server_pubkey, them_spk, cd, function(ret) {
                    var msg2 = ["channel_sync", my_pubkey, ret];
                    variable_public_get(msg2, function(foo) {});
                });
                // eventually decrypt msgs here, for lightning payments.
                // eventually needed for lightning: api_bet_unlock(ip, port);
            });
        });
    }
    return {"run5": run5, "test": chalang_test, "pull_channel_state": pull_channel_state};
}
/*
//this is an example of calling run5, the chalang library.
var d = chalang_data_maker(1000, 1000, 50, 1000, [], [], chalang_new_state(0, 0));
var code = [
    0,0,0,0,5,
    21, 52
];
var stack = chalang("run5")(code, d).stack
console.log(JSON.stringify(stack));
*/

var chalang_object = chalang();
//var foo = chalang_object.test();//this is how you make the test run.
//console.log(JSON.stringify(foo));
