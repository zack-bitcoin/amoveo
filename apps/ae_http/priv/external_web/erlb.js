// erlb.js
// =======
// Copyright (c) 2013 Serge Aleynikov <saleyn@gmail.com>
// See BSD for licensing information.
// This project originated from Bert (https://github.com/rustyio/BERT-JS)
// but ended up being a rewrite.

// erlb.js is a Javascript implementation of Erlang Binary External Term Format.
// http://github.com/saleyn/erlb.js
//
// For future integration:
// BigInteger: https://github.com/silentmatt/javascript-biginteger

//-----------------------------------------------------------------------------
// - CLASSES -
//-----------------------------------------------------------------------------

function Erl() {}

function ErlObject() {}
ErlObject.prototype.type        = 'erl';
ErlObject.prototype.toString    = function() { return this.type; };
ErlObject.prototype.extend      = function(Child, Type) {
    var F = function(){};
    F.prototype = ErlObject.prototype;
    Child.prototype = new F();
    Child.prototype.constructor = Child;
    Child.prototype.type = Type;
}


function ErlAtom(S) {
    this.value = S;
}
ErlObject.prototype.extend(ErlAtom, 'atom');
ErlAtom.prototype.equals        = function(a) {
        return a instanceof ErlAtom && this.value === a.value;
    };
ErlAtom.prototype.encodeSize    = function() { return 3 + Math.min(255, this.value.length); };
ErlAtom.prototype.toString      = function() {
        var v = this.value;
        return (!v.length || v[0] < "a" || v[0] > "z")
            ? "'" + this.value + "'"
            : this.value;
    };


function ErlBinary(Arr) {
    if (!(Arr instanceof Array))
        throw new Error("Unsupported binary data type: " + Erl.getClassName(Arr));
    this.value = Arr;
}
ErlObject.prototype.extend(ErlBinary, 'binary');
ErlBinary.prototype.equals      = function(a) {
        return a instanceof ErlBinary && this.value.equals(a.value);
    };
ErlBinary.prototype.encodeSize  = function() { return 1 + 4 + this.value.length; };
ErlBinary.prototype.toString    = function() {
        var a = this.value;
        var s = "<<";
        var printable = a.length > 0 && a.every(function(i) { return i > 30 && i < 127; });
        return printable
            ? '<<"' + a.map(function(i) { return String.fromCharCode(i); }).join('') + '">>'
            : '<<' + a.join(',') + '>>';
    };

function ErlTuple(Arr) {
    this.value  = Arr;
    this.length = Arr === undefined ? 0 : Arr.length;
}
ErlObject.prototype.extend(ErlTuple, 'tuple');
ErlTuple.prototype.equals       = function(a) {
        return a instanceof ErlTuple && this.value.equals(a.value);
    };
ErlTuple.prototype.encodeSize   = function() {
        return this.value.reduce(
            function(s,i) { return s + Erl.encode_size(i); },
            1 + (this.length < 256 ? 1 : 4));
    };
ErlTuple.prototype.toString     = function() {
        var s = "{" + this.value.map(function(e) { return Erl.toString(e);}).join(',');
        return s + "}";
    };
ErlTuple.prototype.toDate       = function() { return new Date(this.toTimestamp()); };
ErlTuple.prototype.toTimestamp  = function() {
        if (length !== 3) return -1;
        var n = value[0] * 1000000000 + value[1] * 1000 + value[2] / 1000;
        return isNaN(n) ? -1 : n;
    };


function ErlPid(Node, Id, Serial, Creation) {
    if (typeof(Node) === 'string')
        Node = new ErlAtom(Node);
    else if (!(Node instanceof ErlAtom))
        throw new Error("Node argument must be an atom!");

    this.node = Node;
    this.num  = (((Id & 0x7fff) << 15)
              | ((Serial & 0x1fff) << 2)
              | (Creation & 0x3)) & 0x3fffFFFF;
}
ErlObject.prototype.extend(ErlPid, 'pid');
ErlPid.prototype.equals         = function(a) {
        return a instanceof ErlPid && this.node.equals(a.node) && this.num === a.num;
    };
ErlPid.prototype.encodeSize     = function() { return 1 + this.node.encodeSize() + 9; };
ErlPid.prototype.toString       = function() {
        return "#pid{" + this.node + "," +
               (this.num >> 15) + "," +
               ((this.num >> 2) & 0x1fff) + "}";
    };


function ErlRef(Node, Creation, IDs) {
    if (typeof(Node) === 'string')
        Node = new ErlAtom(Node);
    else if (!(Node instanceof ErlAtom))
        throw new Error("Node argument must be an atom!");
    if (!(IDs instanceof Array) || IDs.length > 3)
        throw new Error("Reference IDs must be an array of length <= 3!");
    this.node = Node;
    this.creation = Creation & 0x3;
    this.ids = IDs;
}
ErlObject.prototype.extend(ErlRef, 'ref');
ErlRef.prototype.equals         = function(a) {
        return a instanceof ErlRef && this.node.equals(a.node)
            && this.creation === a.creation
            && this.ids.equals(a.ids);
    };
ErlRef.prototype.encodeSize     = function() {
        return 1 + 2 + this.node.encodeSize() + 4*this.ids.length + 1;
    }
ErlRef.prototype.toString       = function() {
        var s = "#ref{" + this.node.toString() + ", ";
        return s + (this.ids.length ? this.ids.join(',') : '') + '}';
    };

function ErlVar(Name, Type) {
    this.valueType = Type;
    this.name = Name;
}
ErlObject.prototype.extend(ErlVar, 'binary');
ErlVar.prototype.equals         = function(a) { return false; };
ErlVar.prototype.encodeSize     = function()  { throw new Error("Cannot encode variables!"); };
ErlVar.prototype.toString       = function() {
        var tp;
        switch (this.valueType) {
            case Erl.Enum.ATOM:        tp = "::atom()";    break;
            case Erl.Enum.BINARY:      tp = "::binary()";  break;
            case Erl.Enum.ErlBoolean:  tp = "::bool()";    break;
            case Erl.Enum.ErlByte:     tp = "::byte()";    break;
            case Erl.Enum.ErlDouble:   tp = "::double()";  break;
            case Erl.Enum.ErlLong:     tp = "::int()";     break;
            case Erl.Enum.ErlList:     tp = "::list()";    break;
            case Erl.Enum.ErlPid:      tp = "::pid()";     break;
            case Erl.Enum.ErlPort:     tp = "::port()";    break;
            case Erl.Enum.ErlRef:      tp = "::ref()";     break;
            case Erl.Enum.ErlString:   tp = "::string()";  break;
            case Erl.Enum.ErlTuple:    tp = "::tuple()";   break;
            case Erl.Enum.ErlVar:      tp = "::var()";     break;
            default:                   tp = "";            break;
        }
        return this.name + tp;
    };

//-----------------------------------------------------------------------------
// - INTERFACE -
//-----------------------------------------------------------------------------

Erl.prototype.encode = function (Obj) {
    var n = 1 + this.encode_size(Obj);
    var b = new ArrayBuffer(n);
    var d = new DataView(b)
    d.setUint8(0, this.Enum.VERSION);
    var v = this.encode_inner(Obj, d, 1);
    if (v.offset !== n)
        throw new Error("Invalid size of encoded buffer: " + v.offset + " expected: " + n);
    return b;
};

Erl.prototype.decode = function (buffer) {
    var DV = new DataView(buffer, 0);
    if (DV.getUint8(0) !== this.Enum.VERSION) {
        throw new Error("Not a valid Erlang term.");
    }
    var Obj = this.decode_inner({data: DV, offset: 1});
    if (Obj.offset !== buffer.byteLength) {
        throw new Error("Erlang term buffer has unused " +
                        buffer.byteLength - Obj.offset + " bytes");
    }
    return Obj.value;
};

Erl.prototype.equals = function () {
    var a = arguments[0];
    var b = arguments.length > 1 ? arguments[1] : this;
    if (a === b)
        return true;
    if (ErlObject.prototype.isPrototypeOf(a))
        return a.equals(b)
            || (a instanceof ErlTuple && b instanceof Date && a.toTimestamp() === b.getTime());

    if (a instanceof Date && b instanceof ErlTuple)
        return b.toTimestamp() === a.getTime();
    if (a instanceof Array)
        return b instanceof Array && a.equals(b);

    // Compare two objects for equality
    if (a instanceof Object != b instanceof Object)
        return false;

    for (var k in a) if (!(k in b)) return false;
    for (var k in b) if (!(k in a)) return false;
    for (var k in a) {
        var av = a[k];
        var bv = b[k];
        if (!Erl.equals(av, bv))
            return false;
    }
    return true;
}

Erl.prototype.toString = function(Obj) {
    if (Obj === undefined)  return "undefined";
    if (Obj === null)       return "null";

    switch (typeof(Obj)) {
        case 'number':  return Obj.toString();
        case 'boolean': return Obj.toString();
        case 'string':  return '"' + Obj.toString() + '"';
    }
    if (ErlObject.prototype.isPrototypeOf(Obj))
        return Obj.toString();
    if (Obj instanceof Array) {
        return '[' + Obj.map(function(e) { return Erl.toString(e); }).join(",") + ']';
    }

    return '['
        + Object.keys(Obj).map(function(k) {
                return '{' + k + ',' + Erl.toString(Obj[k]) + '}';
            }).join(",")
        + ']';
}

Erl.prototype.atom = function (Obj) {
    return new ErlAtom(Obj);
};

Erl.prototype.binary = function (Obj) {
    return new ErlBinary(Obj);
};

Erl.prototype.tuple = function () {
    var a = new Array(arguments.length);
    for (var i=0, n = arguments.length; i < n; ++i)
        a[i] = arguments[i];
    return new ErlTuple(a);
};

Erl.prototype.pid = function (Node, Id, Serial, Creation) {
    return new ErlPid(Node, Id, Serial, Creation);
};

Erl.prototype.ref = function (Node, Creation, IDs) {
    return new ErlRef(Node, Creation, IDs);
};

Erl.prototype.toArrayBuffer = function(A) {
    var b = new ArrayBuffer(A.length);
    var d = new DataView(b);
    for (var i=0, n=A.length; i < n; ++i)
        d.setUint8(i, A[i]);
    return b;
}

Erl.prototype.bufferToArray = function(B) {
    var d = new DataView(B);
    var r = new Array(d.byteLength);
    for (var i=0, n=d.byteLength; i < n; ++i)
        r[i] = d.getUint8(i);
    return r;
}

//-----------------------------------------------------------------------------
// - ENCODING -
//-----------------------------------------------------------------------------

Erl.prototype.Enum = {
    VERSION         : 131,
    SMALL_ATOM      : 115,
    ATOM            : 100,
    BINARY          : 109,
    SMALL_INTEGER   : 97 ,
    INTEGER         : 98 ,
    SMALL_BIG       : 110,
    LARGE_BIG       : 111,
    FLOAT           : 99 ,
    NEW_FLOAT       : 70 ,
    STRING          : 107,
    PORT            : 102,
    PID             : 103,
    SMALL_TUPLE     : 104,
    LARGE_TUPLE     : 105,
    LIST            : 108,
    REFERENCE       : 101,
    NEW_REFERENCE   : 114,
    NIL             : 106,
    //---- Custom --------
    TUPLE           : 104, // SMALL_TUPLE
    DOUBLE          : 70,  // NEW_FLOAT,
    BYTE            : 97,  // SMALL_INTEGER,
    BOOLEAN         : 254,
    VAR             : 255,
    ZERO            : 0
}

Erl.prototype.encode_size = function (Obj) {
    switch (typeof(Obj)) {
        case "number":      return this.encode_number_size(Obj);
        case "string":      return this.encode_string_size(Obj);
        case "boolean":     return Obj ? 7 : 8; // Atom "true" or "false"
        case "undefined":   return this.atom("undefined").encodeSize();
    }
    if (Obj === null)
        return this.atom("null").encodeSize();
    switch (Obj.type) {
        case "atom":        return Obj.encodeSize();
        case "tuple":       return Obj.encodeSize();
        case "binary":      return Obj.encodeSize();
        case "pid":         return Obj.encodeSize();
        case "ref":         return Obj.encodeSize();
    }
    var s = this.getClassName(Obj);
    return s.indexOf("Array") < 0
        ? this.encode_assoc_array_size(Obj) : this.encode_array_size(Obj);
};

Erl.prototype.encode_inner = function (Obj, dataView, Offset) {
    var func = 'encode_' + typeof(Obj);
    return this[func](Obj, dataView, Offset);
};

Erl.prototype.encode_object = function (Obj, DV, Offset) {
    if (Obj === null)
        return this.encode_inner(this.atom("null"), DV, Offset);

    switch (Obj.type) {
        case "atom":    return this.encode_atom(Obj, DV, Offset);
        case "binary":  return this.encode_binary(Obj, DV, Offset);
        case "tuple":   return this.encode_tuple(Obj, DV, Offset);
        case "ref":     return this.encode_ref(Obj, DV, Offset);
        case "pid":     return this.encode_pid(Obj, DV, Offset);
    }

    var s = this.getClassName(Obj);
    if (s.indexOf("Array") != -1)   return this.encode_array(Obj, DV, Offset);

    // Treat the object as an associative array
    return this.encode_assoc_array(Obj, DV, Offset);
};

Erl.prototype.encode_undefined = function(Obj, DV, Offset) {
    return this.encode_atom(this.atom("undefined"), DV, Offset);
}

Erl.prototype.encode_string_size = function (Obj) {
    return 1 + 2 + Obj.length; // FIXME: implement encoding for length > 0xFF
};

Erl.prototype.encode_string = function (Obj, DV, Offset) {
    DV.setUint8(Offset++, this.Enum.STRING);
    DV.setUint16(Offset, Obj.length); // FIXME: check length > 0xFF
    Offset += 2;
    for (var i = 0, n = Obj.length; i < n; ++i)
        DV.setUint8(Offset++, Obj.charCodeAt(i));
    return { data: DV, offset: Offset };
};

Erl.prototype.encode_boolean = function (Obj, DV, Offset) {
    return this.encode_atom(new ErlAtom(Obj ? "true" : "false"), DV, Offset);
};

Erl.prototype.encode_number_size = function (Obj) {
    var s, isInteger = this.isInt(Obj);

    // Handle floats
    if (!isInteger) return 1 + 8;

    // Small int
    if (Obj >= 0 && Obj < 256) return 1 + 1;

    // 4 byte int
    if (Obj >= -2147483648 && Obj <= 2147483647) return 1 + 4;

    // Bignum
    var n = 0;
    if (Obj < 0) Obj = -Obj;
    for (; Obj; ++n, Obj = Math.floor(Obj / 256));

    return 1 + 2 + n;
};

Erl.prototype.encode_number = function (Obj, DV, Offset) {
    /* assuming that Obj is numeric, otherwise need to check that: Obj === +Obj */

    // Handle floats
    if (!this.isInt(Obj)) return this.encode_float(Obj, DV, Offset);

    // Small int...
    if (Obj >= 0 && Obj < 256) {
        DV.setUint8(Offset++, this.Enum.SMALL_INTEGER);
        DV.setUint8(Offset++, Obj);
        return { data: DV, offset: Offset };
    }

    // 4 byte int
    if (Obj >= -2147483648 && Obj <= 2147483647) {
        DV.setUint8(Offset++, this.Enum.INTEGER);
        DV.setUint32(Offset, Obj);
        return { data: DV, offset: Offset+4 };
    }

    // Bignum
    var pos = Offset;
    Offset += 2; // code, arity
    DV.setUint8(Offset++, Obj < 0 ? 1 : 0); // Sign
    if (Obj < 0) Obj = -Obj;
    var n = 0;
    for (; Obj; ++n, Obj = Math.floor(Obj / 256)) {
        var i = Obj % 256;
        DV.setUint8(Offset++, i);
    }
    var code = n < 256 ? this.Enum.SMALL_BIG : this.Enum.LARGE_BIG;
    DV.setUint8(pos++, code);
    DV.setUint8(pos, n);
    return { data: DV, offset: Offset };
};

Erl.prototype.encode_float = function (Obj, DV, Offset) {
    DV.setUint8(Offset++, this.Enum.NEW_FLOAT);
    DV.setFloat64(Offset, Obj);
    return { data: DV, offset: Offset+8 };
};

Erl.prototype.encode_atom = function (Obj, DV, Offset) {
    DV.setUint8(Offset++, this.Enum.ATOM);
    DV.setUint16(Offset, Obj.value.length);
    Offset += 2;
    for (var i = 0, n = Obj.value.length; i < n; ++i)
        DV.setUint8(Offset++, Obj.value.charCodeAt(i));
    return { data: DV, offset: Offset };
};

Erl.prototype.encode_binary = function (Obj, DV, Offset) {
    DV.setUint8(Offset++, this.Enum.BINARY);
    DV.setUint32(Offset, Obj.value.length);
    Offset += 4;
    for (var i = 0, n = Obj.value.length; i < n; ++i)
        DV.setUint8(Offset++, Obj.value[i]);
    return { data: DV, offset: Offset };
};

Erl.prototype.encode_tuple = function (Obj, DV, Offset) {
    var n = Obj.length;
    if (n < 256) {
        DV.setUint8(Offset++, this.Enum.SMALL_TUPLE);
        DV.setUint8(Offset++, n);
    } else {
        DV.setUint8(Offset++, this.Enum.LARGE_TUPLE);
        DV.setUint32(Offset, n);
        Offset += 4;
    }
    return Obj.value.reduce(
        function(a, e) { return Erl.encode_inner(e, a.data, a.offset); },
        {data: DV, offset: Offset});
};

Erl.prototype.encode_pid = function (Obj, DV, Offset) {
    DV.setUint8(Offset++, this.Enum.PID);
    var r = this.encode_atom(Obj.node, DV, Offset);
    Offset = r.offset;
    DV.setUint32(Offset,  (Obj.num >> 15) & 0x7fff); Offset += 4;
    DV.setUint32(Offset,  (Obj.num >>  2) & 0x1fff); Offset += 4;
    DV.setUint8 (Offset++,(Obj.num & 0x3));
    return { data: DV, offset: Offset };
};

Erl.prototype.encode_ref = function (Obj, DV, Offset) {
    DV.setUint8(Offset++, this.Enum.NEW_REFERENCE);
    DV.setUint16(Offset, Obj.ids.length); Offset += 2;
    var r = this.encode_atom(Obj.node, DV, Offset);
    Offset = r.offset;
    DV.setUint8(Offset++, this.creation);
    Offset = Obj.ids.reduce(function(n,i) { DV.setUint32(n, i); return n+4; }, Offset);
    return { data: DV, offset: Offset };
};

Erl.prototype.encode_array_size = function (Obj) {
    return Obj.reduce(function(a,e) { return a + Erl.encode_size(e); }, Obj.length ? 6 : 1)
};

Erl.prototype.encode_array = function (Obj, DV, Offset) {
    if (Obj.length > 0) {
        DV.setUint8(Offset++, this.Enum.LIST);
        DV.setUint32(Offset, Obj.length); Offset += 4;
        Offset = Obj.reduce(
            function(n,e) { var r = Erl.encode_inner(e, DV, n); return r.offset; },
            Offset
        );
    }
    DV.setUint8(Offset++, this.Enum.NIL);
    return { data: DV, offset: Offset };
};

Erl.prototype.encode_assoc_array_size = function (Obj) {
    var n = 6 /* list begin/end */;
    for (var key in Obj)
        if (Obj.hasOwnProperty(key))
            n += 2 /* tuple */
              + this.atom(key).encodeSize()
              + this.encode_size(Obj[key]);
    return n;
}

Erl.prototype.encode_assoc_array = function (Obj, DV, Offset) {
    var Arr = [];
    for (var key in Obj)
        if (Obj.hasOwnProperty(key))
            Arr.push(this.tuple(this.atom(key), Obj[key]));
    return this.encode_array(Arr, DV, Offset);
};

//-----------------------------------------------------------------------------
// - DECODING -
//-----------------------------------------------------------------------------

Erl.prototype.decode_inner = function (Obj) {
    var DV = Obj.data;
    var Type = DV.getUint8(Obj.offset);
    switch (Type) {
        case this.Enum.SMALL_ATOM:      return this.decode_atom(Obj);
        case this.Enum.ATOM:            return this.decode_atom(Obj);
        case this.Enum.STRING:          return this.decode_string(Obj);
        case this.Enum.SMALL_INTEGER:   return this.decode_integer(Obj);
        case this.Enum.INTEGER:
        case this.Enum.SMALL_BIG:
        case this.Enum.LARGE_BIG:       return this.decode_integer(Obj);
        case this.Enum.FLOAT:
        case this.Enum.NEW_FLOAT:       return this.decode_float(Obj);
        case this.Enum.LIST:            return this.decode_list(Obj);
        case this.Enum.NIL:             return { value: [], offset: Obj.offset+1 };
        case this.Enum.SMALL_TUPLE:
        case this.Enum.LARGE_TUPLE:     return this.decode_tuple(Obj);
        case this.Enum.BINARY:          return this.decode_binary(Obj);
        case this.Enum.PID:             return this.decode_pid(Obj);
        case this.Enum.NEW_REFERENCE:   return this.decode_ref(Obj);
        default: throw new Error("Unexpected Erlang type: " +
                                Type + " at offset " + Obj.offset);
    }
};

Erl.prototype.decode_atom = function (Obj) {
    var DV = Obj.data;
    var Offset = Obj.offset;
    var n, Type = DV.getUint8(Offset++);
    switch (Type) {
        case this.Enum.ATOM:
            n = DV.getUint16(Offset); Offset += 2;
            break;
        case this.Enum.SMALL_ATOM:
            n = DV.getUint8(Offset++);
            break;
        default:
            throw new Error("Invalid Erlang atom: " +
                            Type + " at offset " + Offset);
    }
    var A = new Uint8Array(DV.buffer, Offset, n);
    Offset += n;
    var s = String.fromCharCode.apply(String, A);
    var v;
    switch (s) {
        case "true":        v = true; break;
        case "false":       v = false; break;
        case "undefined":   v = undefined; break;
        case "null":        v = null; break;
        default:            v = this.atom(s);
    }
    return { value: v, offset: Offset };
};

Erl.prototype.decode_binary = function (Obj) {
    var DV = Obj.data;
    var Offset = Obj.offset;
    var Type = DV.getUint8(Offset++);
    if (Type !== this.Enum.BINARY)
        throw new Error("Invalid Erlang binary: " + Type + " at offset " + Offset);
    var n = DV.getUint32(Offset); Offset += 4;
    var a = new Array(n);
    for (var i=Offset, j=0, m = Offset+n; i < m; ++i, ++j) a[j] = DV.getUint8(i);
    return { value: this.binary(a), offset: Offset+n };
};

Erl.prototype.decode_integer = function (Obj) {
    var DV = Obj.data;
    var Offset = Obj.offset;
    var Type = DV.getUint8(Offset++);
    var v, arity, sign;
    switch (Type) {
        case this.Enum.SMALL_INTEGER:
            v = DV.getUint8(Offset++);
            break;
        case this.Enum.INTEGER:
            v = DV.getInt32(Offset); Offset += 4;
            break;
        case this.Enum.SMALL_BIG:
            arity = DV.getUint8(Offset++);
            // Deliverately falling through
        case this.Enum.LARGE_BIG:
            if (Type != this.Enum.SMALL_BIG) {
                arity = DV.getUint32(Offset); Offset += 4;
            }
            if (arity > 8)
                throw new Error("Integer value too large for type: " +
                                Type + " arity " + arity);
            sign = DV.getUint8(Offset++);
            v = 0;
            for (var i = 0, n = 1; i < arity; ++i, n *= 256)
                v += DV.getUint8(Offset++) * n;

            if (sign) v = -v;
            break;
        default:
            throw new Error("Invalid Erlang integer type: " +
                            Type + " at offset " + Offset);
    }

    return { value: v, offset: Offset };
};

Erl.prototype.decode_float = function (Obj) {
    var DV = Obj.data;
    var Offset = Obj.offset;
    var Type = DV.getUint8(Offset++);
    var V, N;
    switch (Type) {
        case this.Enum.FLOAT:
            N = 31;
            var A = new Uint8Array(DV.buffer, Offset, N);
            Offset += N;
            var S = String.fromCharCode.apply(String, A);
            V = parseFloat(S);
            break;
        case this.Enum.NEW_FLOAT:
            V = DV.getFloat64(Offset); Offset += 8;
            break;
        default:
            throw new Error("Invalid Erlang float type: " +
                            Type + " at offset " + Offset);
    }
    return { value: V, offset: Offset };
};

Erl.prototype.decode_string = function (Obj) {
    var DV = Obj.data;
    var Offset = Obj.offset;
    var n, s, Type = DV.getUint8(Offset++);
    switch (Type) {
        case this.Enum.STRING:
            n = DV.getUint16(Offset); Offset += 2;
            var a = new Uint8Array(DV.buffer, Offset, n);
            Offset += n;
            s = String.fromCharCode.apply(String, a);
            break;
        case this.Enum.LIST:
            n = DV.getUint32(Offset); Offset += 4;
            var r = [];
            for (var i = 0; i < n; i++) {
                if (DV.getUint8(Offset++) !== this.SMALL_INTEGER)
                    throw new Error("Error decoding string.");
                var c = DV.getUint8(Offset++);
                r.push(c);
            }
            s = String.fromCharCode.apply(String, r);
            break;
        case this.Enum.NIL:
            s = "";
            break;
        default:
            throw new Error("Invalid Erlang string type: " +
                            Type + " at offset " + Offset);
    }
    return { value: s, offset: Offset };
};

Erl.prototype.decode_list = function (Obj) {
    var DV = Obj.data;
    var Offset = Obj.offset;
    var n, r, Type = DV.getUint8(Offset++);
    switch (Type) {
        case this.Enum.STRING:
            n = DV.getUint16(Offset); Offset += 2;
            var a = new Uint8Array(DV.buffer, Offset, n);
            Offset += n;
            r = String.fromCharCode.apply(String, a);
            break;
        case this.Enum.LIST:
            n = DV.getUint32(Offset);
            Obj.offset = Offset + 4;
            r = new Array(n);
            for (var i = 0; i < n; ++i) {
                var Res = Erl.decode_inner(Obj);
                r[i] = Res.value;
                Obj.offset = Res.offset;
            }
            Offset = Obj.offset;
            if (DV.byteLength > Offset && DV.getUint8(Offset) === this.Enum.NIL)
                Offset++;
            // Check if the list is an associative array
            if (r.every(function(e) {
                    return e instanceof ErlTuple && e.length === 2 && e.value[0] instanceof ErlAtom; })
            ) {
                // Try to convert the associative array to an object
                var b = true;
                var out = {};
                for (var i=0; i < n; ++i) {
                    var e = r[i];
                    var k = e.value[0];
                    if (k in out) {
                        // Key already exists
                        b = false;
                        break;
                    }
                    out[k] = e.value[1];
                }
                if (b)
                    r = out;
            }
            break;
        case this.Enum.NIL:
            r = [];
            break;
        default:
            throw new Error("Invalid Erlang list type: " +
                            Type + " at offset " + Offset);
    }
    return { value: r, offset: Offset };
};

Erl.prototype.decode_tuple = function (Obj) {
    var DV = Obj.data;
    var Offset = Obj.offset;
    var n, Type = DV.getUint8(Offset++);
    switch (Type) {
        case this.Enum.SMALL_TUPLE:
            n = DV.getUint8(Offset);
            Obj.offset = Offset + 1;
            break;
        case this.Enum.LARGE_TUPLE:
            n = DV.getUint32(Offset);
            Obj.offset = Offset + 4;
            break;
        default:
            throw new Error("Invalid Erlang tuple type: " +
                            Type + " at offset " + Offset);
    }
    var r = new Array(n);
    for (var i = 0; i < n; i++) {
        var res = Erl.decode_inner(Obj);
        r[i] = res.value;
        Obj.offset = res.offset;
    }
    return { value: this.tuple.apply(this, r), offset: Obj.offset };
};

Erl.prototype.decode_pid = function(Obj) {
    var DV = Obj.data;
    var Offset =  Obj.offset;
    var Type = DV.getUint8(Offset++);
    if (Type !== this.Enum.PID)
        throw new Error("Invalid pid type: " + Type);
    Obj.offset = Offset;
    var r = this.decode_atom(Obj);
    Offset = r.offset;
    var Id = DV.getUint32(Offset) & 0x7fff; Offset += 4;
    var Sn = DV.getUint32(Offset) & 0x1fff; Offset += 4;
    var Cr = DV.getUint8 (Offset++) & 0x3;
    return { value: this.pid(r.value, Id, Sn, Cr), offset: Offset };
}

Erl.prototype.decode_ref = function(Obj) {
    var DV = Obj.data;
    var Offset =  Obj.offset;
    var Type = DV.getUint8(Offset++);
    if (Type !== this.Enum.NEW_REFERENCE)
        throw new Error("Invalid ref type: " + Type);
    var n   = DV.getUint16(Offset); Offset += 2;
    var Ids = new Array(n);
    Obj.offset = Offset;
    var r  = this.decode_atom(Obj);
    Offset = r.offset;
    var Cr = DV.getUint8 (Offset++) & 0x3;
    for (var i = 0; i < n; ++i, Offset += 4)
        Ids[i] = DV.getUint32(Offset);

    return { value: this.ref(r.value, Cr, Ids), offset: Offset };
}

//-----------------------------------------------------------------------------
// - UTILITY FUNCTIONS -
//-----------------------------------------------------------------------------

Erl.prototype.getClassName = function(Obj) {
    var funcNameRegex = /function (.{1,})\(/;
    var results = (funcNameRegex).exec(Obj.constructor.toString());
    return (results && results.length > 1) ? results[1] : "";
};

Erl.prototype.isInt = function(x) { return parseFloat(x) == parseInt(x) && !isNaN(x); }

Erl.prototype.timestampToTuple = function(n) {
    var Ms = Math.floor(n / 1000000000); n -= Ms*1000000000;
    var s  = Math.floor(n / 1000); n -= s*1000;
    var ms = n;
    return new ErlTuple([Ms, s, ms]);
}

Erl.prototype.dateToTuple = function(d) {
    var n  = d.getTime();
    var Ms = Math.floor(n / 1000000000); n -= Ms*1000000000;
    var s  = Math.floor(n / 1000); n -= s*1000;
    var ms = n;
    return new ErlTuple([Ms, s, ms]);
}

var Erl = new Erl();

/*
// Override console log to display Erl objects friendly
(function() {
    var cl = console.log;
    console.log = function() {
        cl.apply(console, [].slice.call(arguments).map(function(el) {
            return typeof el === 'object' // {}.toString.call(el) === '[object Object]'
                && typeof el.toString === 'function'
                && el.toString !== Object.prototype.toString ? el.toString() : el;
        }));
    };
    console.oldlog = cl;
}());
*/

// attach the .equals method to Array's prototype to call it on any array
Array.prototype.equals = function (array) {
    // if the other array is undefined or null return a false value
    if (!array)
        return false;

    // compare lengths - can save a lot of time
    if (this.length != array.length)
        return false;

    for (var i = 0, l=this.length; i < l; i++) {
        // Check if we have nested arrays
        if (   (this[i] instanceof Array && array[i] instanceof Array)
            || (this[i] instanceof ErlTuple && array[i] instanceof ErlTuple)
            || (this[i].equals !== undefined))
        {
            // recurse into the nested arrays
            if (!this[i].equals(array[i]))
                return false;
        } else if (this[i] !== array[i])
            return false;
    }
    return true;
}

ArrayBuffer.prototype.equals = function (array) {
    if (!array)
        return false;
    var a = new Uint8Array(this);
    var b = array instanceof ArrayBuffer ? new Uint8Array(array): array;
    // compare lengths - can save a lot of time
    if (a.length != b.length)
        return false;

    for (var i = 0, l=a.length; i < l; ++i)
        if (a[i] !== b[i])
            return false;
    return true;
}
