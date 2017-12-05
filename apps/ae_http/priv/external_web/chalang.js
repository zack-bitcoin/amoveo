function chalang_new_state(height, slash) {
    return {"name": "state", "height": height, "slash": slash};
}
function chalang_data_maker(op_gas, ram_gas, many_vs, many_funs, script_sig, code, state) {
    return {"name": "d", "op_gas":op_gas, "stack": [], "alt": [], "ram_most": 0, "ram_limit":ram_gas, "vars": chalang_make_array(many_vs), "funs":{}, "many_funs": 0, "fun_limit":many_funs, "ram_current":(script_sig.length + code.length), "state":state};
}
function chalang_make_array(m) {
    var arr = [];
    arr.length = m;
    return arr;
}
function array_to_int(l) {
    var x = 0;
    for (var i = 0; i < l.length; i++) {
        x = (256 * x) + l[i];
    }
    return x;
}
function exponential(b, a) {
    if (b == 0) {
        return 0;
    } else if (a == 0) {
        return 1;
    }
    return exponential2(b, a, 1);
}
function exponential2(b, a, n) {
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
function underflow_check(d, min_size, op_name, continuation) {
    if (d.stack.length < min_size) {
        return ["error", "stack underflow", op_name];
    } else {
        continuation();
        return 0;
    }
}
function run5(code, d) {
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
    function count_till(code, i, opcode) {
        for (var j = 0; j < code.length - i; j++) {
            if (opcode == code[i+j]) {
                return j;
            }
        }
        console.log("count till reached end without finding goal");
        console.log(opcode);
        return ["error", "count till"];
    }
    function memory(x) {
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
    function arithmetic_chalang(op, a, b) { //returns a list to concat with stack.
        var x;
        if (op == add) { x = a + b;
        } else if (op == subtract) { x = b - a;
        } else if (op == mul) { x = b * a;
        } else if (op == div) { x = Math.floor(b / a);
        } else if (op == pow) { x = exponential(b, a);
        } else if (op == rem) { x = b % a;
        } else if (op == gt) {
            if (a < b) {
                x = 1;
            } else {
                x = 0;
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
    function run2(code, d) {
        var error_check = 0;
        for (var i = 0; i<code.length; i++) {
            //console.log"run cycle");
            //console.logi);
            //console.log"opcode: ");
            //console.logcode[i]);
            //console.logJSON.stringify(code));
            //console.logJSON.stringify(d.stack));
            if (!(error_check == 0)) {
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
                //console.log"int op");
                var int_array = code.slice(i+1, i+5);
                var new_int = array_to_int(int_array);
                d.stack = ([new_int]).concat(d.stack);
                d.ram_current = d.ram_current + 1;
                d.op_gas = d.op_gas - 1;
                i = i + 4;
            } else if (code[i] == binary_op) {
                //console.log"bin op");
                var int_array = code.slice(i+1, i+5);
                var new_int = array_to_int(int_array);
                var bin_array = code.slice(i+5, i+5+new_int);
                d.stack = ([(["binary"]).concat(
                    bin_array)]).concat(
                        d.stack);
                d.ram_current = d.ram_current + 1;
                d.op_gas = d.op_gas - new_int;
                i = i + 4 + new_int;
            } else if (code[i] == caseif) {
                //console.log"if op");
                var b = d.stack[0];
                var skipped_size;
                var size_case1 = count_till(code, i, caseelse);
                var size_case2 = count_till(code, i + j, casethen);
                if (b == 0) {
                    skipped_size = size_case1;
                    i = i + skipped_size;
                    //maybe we should remove the case_then from code, that way we can do tail optimized recursion after a conditional.
                } else {
                    var j = count_till(code, i, caseelse);
                    var skipped_size = size_case2;
                }
                d.stack = d.stack.slice(1, d.stack.length);
                d.ram_current = d.ram_current - skipped_size - 1;
                d.op_gas = d.op_gas - size_case1 - size_case2;
            } else if (code[i] == caseelse) {
                //console.log"else op");
                var skipped_size = count_till(code, i, casethen);
                i = i + skipped_size;
            } else if (code[i] == casethen) {
                //console.log"then op");
                // do nothing.
            } else if ((code[i] == call) && (code[i+1] == fun_end)){
                //tail call optimized function call
                //console.log"function call op");
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
                console.log("function call op");
                console.log(d.stack[0]);
                console.log(JSON.stringify(d.stack));
                console.log(d.funs);
                var code_hash = btoa(array_to_string(d.stack[0].slice(1, d.stack[0].length)));
                console.log(code_hash);
                definition = d.funs[code_hash];
                var s = definition.length;
                d.op_gas = d.op_gas - s - 10;
                d.ram_current = d.ram_current + s - 1;
                d.stack = d.stack.slice(1, d.stack.length);
                d = run2(definition, d);
            } else if (code[i] == define) {
                //console.log"define op");
                var skipped_size = count_till(code, i, fun_end);
                var definition = code.slice(i+1, i+skipped_size);
                i = i + skipped_size;
                var hash_array = small_hash(definition);
                var b = btoa(array_to_string(hash_array));
                var definition2 = replace(recurse, ([binary_op]).concat(integer_to_array(hash_size, 4)).concat(hash_array), definition);
                console.log("definition 2 of new function");
                console.log(definition2);
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
                //console.log"drop op");
                error_check = underflow_check(d, 1, "drop", function() {
                    d.ram_current = d.ram_current - memory(d.stack[0]) - 2;
                    d.stack = d.stack.slice(1, d.stack.length);
                    d.op_gas = d.op_gas - 1;
                });
            } else if (code[i] == dup) {
                //console.log"dup op");
                error_check = underflow_check(d, 1, "dup", function() {
                    d.stack = ([d.stack[0]]).concat(d.stack);
                    d.ram_current = d.ram_current + memory(d.stack[0]);
                    d.op_gas = d.op_gs - 1;
                });
            } else if (code[i] == swap) {
                //console.log"swap op");
                error_check = underflow_check(d, 2, "swap", function() {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.stack[1]]).concat(
                        [d.stack[0]]).concat(
                            d.stack.slice(2, d.stack.length));
                });
            } else if (code[i] == tuck) {
                //console.log"tuck op");
                error_check = underflow_check(d, 3, "tuck", function() {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.stack[1]]).concat(
                        [d.stack[2]]).concat(
                            [d.stack[0]]).concat(
                                d.stack.slice(3, d.stack.length));
                });
            } else if (code[i] == rot) {
                //console.log"rot op");
                error_check = underflow_check(d, 3, "rot", function() {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.stack[2]]).concat(
                        [d.stack[0]]).concat(
                            [d.stack[1]]).concat(
                                d.stack.slice(3, d.stack.length));
                });
            } else if (code[i] == ddup) {
                //console.log"ddup op");
                error_check = underflow_check(d, 2, "ddup", function() {
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current + memory(d.stack[0]) + memory(d.stack[1]);
                    d.stack = d.stack.slice(0, 2).concat(d.stack);
                });
            } else if (code[i] == tuckn) {
                //console.log"tuckn op");
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
                //console.log"pickn op");
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
                //console.log">r op");
                error_check = underflow_check(d, 1, "to_r", function() {
                    d.op_gas = d.op_gas - 1;
                    d.alt = ([d.stack[0]]).concat(d.alt);
                    d.stack = d.stack.slice(1, d.stack.length);
                });
            } else if (code[i] == from_r) {
                //console.log"r> op");
                if (d.alt.length < 1) {
                    return ["error", "alt stack underflow", "from_r"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.alt[0]]).concat(d.stack);
                    d.atl = d.alt.slice(1, d.alt.length);
                }
            } else if (code[i] == r_fetch) {
                //console.log"r@ op");
                if (d.alt.length < 1) {
                    return ["error", "alt stack underflow", "r_fetch"];
                } else {
                    op_gas = d.op_gas - 1;
                    d.stack = ([d.alt[0]]).concat(d.stack);
                }
            } else if (code[i] == hash_op) {
                //console.log"hash op");
                error_check = underflow_check(d, 1, "hash", function() {
                    d.op_gas = d.op_gas - 20;
                    d.stack = ([hash(d.stack[0])]).concat(
                        d.stack.slice(1, d.stack.length));
                });
            } else if (code[i] == verify_sig) {
                //console.log"verify_sig op");
                error_check = underflow_check(d, 3, "verify_sig", function(){
                    //data, sig, key
                    var b = verify(d.stack[1], d.stack[2], d.stack[0]);
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
                //console.log"arithmetic");
                error_check = underflow_check(d, 2, "arithmetic", function(){
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current - 2;
                    var a = arithmetic_chalang(code[i], d.stack[0], d.stack[1]);
                    d.stack = a.concat(d.stack.slice(2, d.stack.length));
                });
            } else if (code[i] == eq) {
                //console.log"eq op");
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
                //console.log"bool flip op");
                error_check = underflow_check(d, 1, "bool_flip", function(){
                    if (d.stack[0] == 0) {
                        d.stack = ([1]).concat(d.stack.slice(1, d.stack.length));
                    } else {
                        d.stack = ([0]).concat(d.stack.slice(1, d.stack.length));
                    }
                    d.op_gas = d.op_gas - 1;
                });
            } else if (code[i] == bool_and) {
                //console.log"bool and op");
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
                //console.log"bool or op");
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
                //console.log"bool xor op");
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
                //console.log"bool stack_size op");
                d.op_gas = d.op_gas - 1;
                d.ram_current = d.ram_current + 2;
                d.stack = ([d.stack.length]).concat(d.stack);
            } else if (code[i] == height) {
                //console.log"bool height op");
                d.op_gas = d.op_gas - 1;
                d.ram_current = d.ram_current + 2;
                d.stack = ([d.state.height]).concat(d.stack);
            } else if (code[i] == gas) {
                //console.log"bool gas op");
                d.op_gas = d.op_gas - 1;
                d.stack = ([d.op_gas]).concat(d.stack);
                d.ram_current += 2;
            } else if (code[i] == many_vars) {
                //console.log"bool many vars op");
                d.op_gas -= 1;
                d.stack = ([d.vars.length]).concat(d.stack);
                d.ram_current += 2;
            } else if (code[i] == many_funs) {
                //console.log"bool many funs op");
                d.op_gas -= 1;
                d.ram_current += 2;
                d.stack = (d.many_funs).concat(d.stack);
            } else if (code[i] == fun_end) {
                //console.log"bool fun end op");
                d.op_gas -= 1;
            } else if (code[i] == set) {
                //console.log("running set");
                error_check = underflow_check(d, 2, "set", function(){
                    //console.log"no underflow error");
                    d.vars[d.stack[0]] = d.stack[1];
                    d.op_gas -= 1;
                    d.stack = d.stack.slice(2, d.stack.length);
                });
            } else if (code[i] == fetch) {
                console.log("fetch op");
                console.log(JSON.stringify(d.stack));
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
                    d.stack = ([val]).concat(d.stack);
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
                //console.log("bool car op");
                error_check = underflow_check(d, 1, "car", function(){
                    d.op_gas -= 1;
                    d.ram_current -= 1;
                    d.stack = ([d.stack[0].slice(1, d.stack[0].length)]).concat(
                        ([d.stack[0][0]])).concat(
                            d.stack.slice(1, d.stack.length));
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
    function main(code, d) {
        var b = is_balanced_f(code);
        if (b) {
            var x = run2(code, d);
            return x;
        } else {
            console.log("misformed function. : ; ");
            return ["error", "mismatched function"];
        }
    }
    function chalang_test() {
        var d = chalang_data_maker(1000, 1000, 50, 1000, [], [], chalang_new_state(0, 0));
        console.log("chalang test");
        var function_contract =
            [define,21,52,fun_end,
             define,
               2,0,0,0,12,239,24,7,129,222,179,141,
               148,74,245,17,98,113,2,0,0,0,12,239,24,7,129,222,179,
               141,148,74,245,17,98,113,
             fun_end,
             0,0,0,0,2,
             2,0,0,0,12,
             248,21,87,89,106,92,199,6,67,69,197,184,
             call,
             0,0,0,0,16,
             eq,swap,drop,swap,drop];
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
             print,
             empty_list,
             0,0,0,0,5,
             swap,cons,
             0,0,0,0,6,swap,cons,
             0,0,0,0,7,
             swap,cons,reverse,
             print,
             2,0,0,0,12,
             239,24,7,129,222,179,141,148,74,245,17,98,
             print,
             2,0,0,0,12,
             53,181,176,16,58,242,45,201,243,134,253,139,
             print,
             call,
             print,
             empty_list,
             0,0,0,0,25,
             swap,cons,
             0,0,0,0,36,
             swap,cons,
             0,0,0,0,49,
             swap,cons,reverse, print,
             eq,to_r,drop,drop,from_r];
        var contract =
            [
                int_op, 0,0,0,0,
                0,0,0,0,3,
                0,0,0,0,1, set, //fetch,
                0,0,0,0,1, fetch,
                print,
                //bool_flip,
                print,
                caseif, 0,0,0,0,5,
                caseelse, 0,0,0,0,6,
                casethen,
                //0,0,0,0,1, fetch,
                print];
        var x = main(map_contract, d);
        console.log(JSON.stringify(x.stack));
    }


    if (code == undefined) {
        return chalang_test();
    } else {
        return main(code, d);
    }
}
run5();//this is how you make the test run.
