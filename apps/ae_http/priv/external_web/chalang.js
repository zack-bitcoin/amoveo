chalang_test();
function chalang_test() {
    var d = chalang_data_maker(1000, 1000, 50, 1000, [], [], chalang_new_state(0, 0));
    console.log("chalang test");
    var x = run5([0,0,0,0,7], d);
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
          add = 50,
          remainder = 57,
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
          nil = 132,
          append = 134,
          split = 135,
          reverse = 136,
          is_list = 137;
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
                i = i + 4;
                var new_int = array_to_int(int_array);
                d.stack = [new_int].concat(d.stack);
                d.ram_current = d.ram_current + 1;
                d.op_gas = d.op_gas - 1;
            }
            
                //working here;
    
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
