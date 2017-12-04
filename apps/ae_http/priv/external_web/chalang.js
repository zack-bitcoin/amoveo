chalang_test();
function chalang_test() {
    var d = chalang_data_maker(1000, 1000, 50, 1000, [], [], chalang_new_state(0, 0));
    console.log("chalang test");
    var x = run5([0,0,0,0,7,
                  0,0,0,0,8,
                  52
                  ], d);
    console.log(JSON.stringify(x.stack));
}
function chalang_new_state(height, slash) {
    return {"name": "state", "height": height, "slash": slash};
}
function chalang_data_maker(op_gas, ram_gas, many_vars, many_funs, script_sig, code, state) {
    return {"name": "d", "op_gas":op_gas, "stack": [], "alt": [], "ram_most": 0, "ram_limit":ram_gas, "vars": chalang_make_array(many_vars), "funs":{}, "many_funs": 0, "fun_limit":many_funs, "ram_current":(script_sig.length + code.length), "state":state};
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
          hash = 20,
          verify_sig = 41,
          add_op = 50,
          subtract = 51,
          mul = 52,
          divide = 53,
          gt = 54,
          lt = 55,
          pow = 56,
          remainder = 57,
          eq_op = 58,
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
          nil = 132,
          append = 134,
          split = 135,
          reverse = 136,
          is_list = 137,
          word_size = 4294967296;
    function is_balanced_f(code) {
        var x = 0;
        for (var i; i<code.length; i++) {
            if ((code[i] == define) && (x == 0)){
                x = 1;
            } else if ((code[i] == fun_endnd) && (x == 1)) {
                x = 0;
            } else if (code[i] == define) {
                return false;
            } else if (code[i] == fun_end) {
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
        } else if (x[0] == "binary") {
            return x.length - 1;
        } else {
            var a = memory(x[0]);
            var y = x.slice(1, x.length);
            var b = memory(y);
            return a+b;
        }
    }
    function arithmetic_chalang(op, a, b) { //returns a list to concat with stack.
        var x;
        if (op == add_op) {
            x = a + b;
        } else if (op == subtract) {
            x = b - a;
        } else if (op == mul) {
            x = b * a;
        } else if (op == divide) {
            x = Math.floor(b / a);
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
        } else if (op == pow) {
            x = exponential(b, a);
        } else if (op == remainder) {
            x = b % a;
        }
        x = ((x % word_size) + word_size) % word_size;
        return [x];

    }
    function run2(code, d) {
        for (var i = 0; i<code.length; i++) {
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
                var int_array = code.slice(i+1, i+5);
                var new_int = array_to_int(int_array);
                d.stack = ([new_int]).concat(d.stack);
                d.ram_current = d.ram_current + 1;
                d.op_gas = d.op_gas - 1;
                i = i + 4;
            } else if (code[i] == binary_op) {
                var int_array = code.slice(i+1, i+5);
                var new_int = array_to_int(int_array);
                var bin_array = code.slice(i+5, i+5+new_int);
                d.stack = (["binary"]).concat([bin_array]).concat(d.stack);
                d.ram_current = d.ram_current + 1;
                d.op_gas = d.op_gas - new_int;
                i = i + 4 + new_int;
            } else if (code[i] == caseif) {
                var b = d.stack[0];
                var skipped_size;
                if (b == 0) {
                    skipped_size = count_till(code, i, caseelse);
                    i = i + skipped_size;
                } else {
                    var j = count_till(code, i, caseelse);
                    var skipped_size = count_till(code, i + j, casethen);
                }
                d.stack = d.stack.slice(1, d.stack.length);
                d.ram_current = d.ram_current - skipped_size - 1;
                d.op_gas = d.op_gas - steps
            } else if (code[i] == caseelse) {
                var skipped_size = count_till(code, i, casethen);
                i = i + skipped_size;
            } else if (code[i] == casethen) {
                // do nothing.
            } else if ((code[i] == call) && (code[i+1] == fun_end)){
                //tail call optimized function call
                definition = d.funs[d.stack[0]];
                var s = definition.length;
                d.op_gas = d.op_gas - s - 10;
                d.ram_current = d.ram_current + s - 1;
                d.stack = d.stack.slice(1, d.stack.length);
                var rest = code.slice(i+1, code.length);
                return run2(definition.concat(rest), d);
            } else if (code[i] == call) {
                //non-optimized function call.
                definition = d.funs[d.stack[0]];
                var s = definition.length;
                d.op_gas = d.op_gas - s - 10;
                d.ram_current = d.ram_current + s - 1;
                d.stack = d.stack.slice(1, d.stack.length);
                d = run2(definition, d);
            } else if (code[i] == define) {
                var skipped_size = count_till(code, i, fun_end);
                var definition = code.slice(i+1, i+1+skipped_size);
                i = i + skipped_size;
                var b = hash(definition);
                var definition2 = replace(recurse, ([2]).concat(integer_to_array(32)), definition);
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
                return d;
            } else if (code[i] == print) {
                console.log(JSON.stringify(d.stack));
            } else if (code[i] == drop) {
                if (d.stack.length == 0) {
                    return ["error", "stack underflow - drop"];
                } else {
                    d.ram_current = d.ram_current - memory(d.stack[0]) - 2;
                    d.stack = d.stack.slice(1, d.stack.length);
                    d.op_gas = d.op_gas - 1;
                }
            } else if (code[i] == dup) {
                if (d.stack.length == 0) {
                    return ["error", "stack underflow - dup"];
                } else {
                    d.stack = ([d.stack[0]]).concat(d.stack);
                    d.ram_current = d.ram_current + memory(d.stack[0]);
                    d.op_gas = d.op_gs - 1;
                }
            } else if (code[i] == swap) {
                if (d.stack.length < 2) {
                    return ["error", "stack underflow - swap"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.stack[1]]).concat(
                        [d.stack[0]]).concat(
                            d.stack.slice(2, d.stack.length));
                }
            } else if (code[i] == tuck) {
                if (d.stack.length < 3) {
                    return ["error", "stack underflow - tuck"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.stack[1]]).concat(
                        [d.stack[2]]).concat(
                            [d.stack[0]]).concat(
                                d.stack.slice(3, d.stack.length));
                }
            } else if (code[i] == rot) {
                if (d.stack.length < 3) {
                    return ["error", "stack underflow", "rot"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.stack[2]]).concat(
                        [d.stack[0]]).concat(
                            [d.stack[1]]).concat(
                                d.stack.slice(3, d.stack.length));
                }
            } else if (code[i] == ddup) {
                if (d.stack.length < 2) {
                    return ["error", "stack underflow", "ddup"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current + memory(d.stack[0]) + memory(d.stack[1]);
                    d.stack = d.stack.slice(0, 2).concat(d.stack);
                }
            } else if (code[i] == tuckn) {
                var n = d.stack[0];
                if (d.stack.length < (n + 2)) {
                    return ["error", "stack underflow", "tuckn"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.stack = d.stack.slice(2, 2+n).concat(
                        [d.stack[1]]).concat(
                            d.stack.slice(3+n, d.stack.length));
                }
            } else if (code[i] == pickn) {
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
                if (d.stack.length < 1) {
                    return ["error", "stack underflow", "to_r"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.alt = ([d.stack[0]]).concat(d.alt);
                    d.stack = d.stack.slice(1, d.stack.length);
                }
            } else if (code[i] == from_r) {
                if (d.alt.length < 1) {
                    return ["error", "alt stack underflow", "from_r"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.stack = ([d.alt[0]]).concat(d.stack);
                    d.atl = d.alt.slice(1, d.alt.length);
                }
            } else if (code[i] == r_fetch) {
                if (d.alt.length < 1) {
                    return ["error", "alt stack underflow", "r_fetch"];
                } else {
                    op_gas = d.op_gas - 1;
                    d.stack = ([d.alt[0]]).concat(d.stack);
                }
            } else if (code[i] == hash) {
                if (d.stack.length < 1) {
                    return ["error", "stack underflow", "hash"];
                } else {
                    d.op_gas = d.op_gas - 20;
                    d.stack = ([hash(d.stack[0])]).concat(
                        d.stack.slice(1, d.stack.length));
                }
            } else if (code[i] == verify_sig) {
                if (d.stack.length < 3) {
                    return ["error", "stack underflow", "verify_sig"];
                } else {
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
                }
            } else if ((!(code[i] < add_op)) && (code[i] < eq_op)) {
                d.op_gas = d.op_gas - 1;
                d.ram_current = d.ram_current - 2;
                var a = arithmetic_chalang(code[i], d.stack[0], d.stack[1]);
                d.stack = a.concat(d.stack.slice(2, d.stack.length));
            } else if (code[i] == eq_op) {
                if (d.stack.length < 2) {
                    return ["error", "stack underflow", "eq_op"];
                } else {
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current + 1;
                    if (JSON.stringify(d.stack[0]) == JSON.stringify(d.stack[1])) {
                        d.stack = ([1]).concat(d.stack);
                    } else {
                        d.stack = ([0]).concat(d.stack);
                    }
                                       
                }
            } else if (code[i] == bool_flip) {
                if (d.stack.length < 1) {
                    return ["error", "stack underflow", "bool_flip"];
                } else {
                    if (d.stack[0] == 0) {
                        d.stack = ([1]).concat(d.stack.slice(1, d.stack.length));
                    } else {
                        d.stack = ([0]).concat(d.stack.slice(1, d.stack.length));
                    }
                    d.op_gas = d.op_gas - 1;
                }
            } else if (code[i] == bool_and) {
                if (d.stack.length < 2) {
                    return ["error", "stack underflow", "bool_and"];
                } else {
                    if ((d.stack[0] == 0) || (d.stack[1] == 0)) {
                        d.stack = ([0]).concat(d.stack.slice(2, d.stack.length));
                    } else {
                        d.stack = ([1]).concat(d.stack.slice(2, d.stack.length));
                    }
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current - 2;
                }
            } else if (code[i] == bool_or) {
                if (d.stack.length < 2) {
                    return ["error", "stack underflow", "bool_or"];
                } else {
                    if ((d.stack[0] == 0) && (d.stack[1] == 0)) {
                        d.stack = ([0]).concat(d.stack.slice(2, d.stack.length));
                    } else {
                        d.stack = ([1]).concat(d.stack.slice(2, d.stack.length));
                    }
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current - 2;
                }
            } else if (code[i] == bool_xor) {
                if (d.stack.length < 2) {
                    return ["error", "stack underflow", "bool_xor"];
                } else {
                    var j = 0;
                    if ((d.stack[0] == 0) && (d.stack[0] == 0)) {
                        j = 0;
                    } else if ((d.stack[0] == 0) || (d.stack[0] == 0)) {
                        j=1;
                    }
                    d.stack = ([j]).concat(d.stack.slice(2, d.stack.length));
                    d.op_gas = d.op_gas - 1;
                    d.ram_current = d.ram_current - 2;
                }
            } else if (code[i] == stack_size) {
                d.op_gas = d.op_gas - 1;
                d.ram_current = d.ram_current + 2;
                d.stack = ([d.stack.length]).concat(d.stack);
            } else if (code[i] == height) {
                d.op_gas = d.op_gas - 1;
                d.ram_current = d.ram_current + 2;
                d.stack = ([d.state.height]).concat(d.stack);
            } else if (code[i] == gas) {
                d.op_gas = d.op_gas - 1;
                d.stack = ([d.op_gas]).concat(d.stack);
                d.ram_current += 2;
            } else if (code[i] = many_vars) {
                d.op_gas -= 1;
                d.stack = ([d.vars.length]).concat(d.stack);
                d.ram_current += 2;
            } else if (code[i] == many_funs) {
                d.op_gas -= 1;
                d.ram_current += 2;
                d.stack = (d.many_funs).concat(d.stack);
            } else if (code[i] == fun_end) {
                d.op_gas -= 1;
            } else if (code[i] == set) {
                if (d.stack.length < 2) {
                    return ["error", "stack underflow", "set"];
                } else {
                    d.vars[d.stack[0]] = d.stack[1];
                    d.op_gas -= 1;
                    d.stack = d.stack.slice(2, d.stack.length);
                }
            } else if (code[i] = fetch) {
                if (d.stack.length < 1) {
                    return ["error", "stack underflow", "fetch"];
                } else {
                    var val;
                    var foo = d.vars[d.stack[0]];
                    if (foo == undefined) {
                        val = [];
                    } else {
                        val = foo;
                    }
                    d.op_gas -= 1;
                    d.ram_current += (1 + memory(val));
                    d.stack = (val).concat(d.stack);
                }
            } else if (code[i] == cons) {
                if (d.stack.length < 2) {
                    return ["error", "stack underflow", "cons"];
                } else {
                    d.op_gas -= 1;
                    d.ram_current += 1;
                    var l = ([d.stack[0]]).concat(
                        [d.stack[1]]);
                    d.stack = ([l]).concat(
                        d.stack.slice(2, d.stack.length));
                }
            } else if (code[i] == car) {
                if (d.stack.length < 1 ) {
                    return ["error", "stack underflow", "car"];
                } else {
                    d.op_gas -= 1;
                    d.ram_current -= 1;
                    d.stack = ([d.stack[0].slice(1, d.stack[0].length)]).concat(
                        ([d.stack[0][0]])).concat(
                            d.stack.slice(1, d.stack.length));
                }
            } else if (code[i] == nil) {
                d.op_gas -= 1;
                d.ram_current += 1;
                d.stack = ([[]]).concat(d.stack);
            } else if (code[i] == append) {
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
            } else if (code[i] == split) {
                if (d.stack.length < 2) {
                    return ["error", "stack underflow", "split"];
                } else if (!(d.stack[0][0] == "binary")) {
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
                    
                }
            } else if (code[i] == reverse) {
                if (d.stack.length < 1) {
                    return ["error", "stack underflow", "reverse"];
                } else if (d.stack[0][0] == "binary") {
                    return ["error", "cannot reverse a binary", "reverse"];
                } else {
                    d.op_gas -= d.stack[0].length;
                    d.stack = ([d.stack[0].reverse()]).concat(
                        d.stack.slice(1, d.stack.length));
                }
            } else if (code[i] == is_list) {
                var j;
                if (d.stack.length < 1) {
                    return ["error", "stack underflow", "is_list"];
                } else if (!(d.stack[0].is_array())) {
                    j = 0;
                } else if (d.stack[0][0] == "binary") {
                    j = 0;
                } else {
                    j = 1;
                }
                d.op_gas -= 1;
                d.ram_current -= 1;
                d.stack = ([j]).concat(d.stack);
            } else if (code[i] == nop) {
            } else if (code[i] == fail) {
                return ["error", "fail opcode", "fail"];
            }
        }
        return d;
    }

    var b = is_balanced_f(code);
    if (b) {
        var x = run2(code, d);
        return x;
    } else {
        console.log("misformed function. : ; ");
        return ["error", "mismatched function"];
    }
}
