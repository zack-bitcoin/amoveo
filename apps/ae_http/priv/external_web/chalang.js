chalang_test();
function chalang_test() {
    var d = chalang_data_maker(1000, 1000, 50, 1000, [], [], chalang_new_state(0, 0));
    console.log("chalang test");
    var x = run5([], d);
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
            //check if we are out of time.
            //check if we are out of space.
            
            //working here;
        }
        return d;
    }
    var b = is_balanced_f(code);
    if (b) {
        return run2(code, d);
    } else {
        console.log("misformed function. : ; ");
    }
}
