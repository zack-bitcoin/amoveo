var bigInt = (function (undefined) {
    "use strict";

    var BASE = 1e7,
	LOG_BASE = 7,
	MAX_INT = 9007199254740992,
	MAX_INT_ARR = smallToArray(MAX_INT),
	LOG_MAX_INT = Math.log(MAX_INT);

    function Integer(v, radix) {
	if (typeof v === "undefined") return Integer[0];
	if (typeof radix !== "undefined") return +radix === 10 ? parseValue(v) : parseBase(v, radix);
	return parseValue(v);
    }

    function BigInteger(value, sign) {
	this.value = value;
	this.sign = sign;
	this.isSmall = false;
    }
    BigInteger.prototype = Object.create(Integer.prototype);

    function SmallInteger(value) {
	this.value = value;
	this.sign = value < 0;
	this.isSmall = true;
    }
    SmallInteger.prototype = Object.create(Integer.prototype);

    function isPrecise(n) {
	return -MAX_INT < n && n < MAX_INT;
    }

    function smallToArray(n) { // For performance reasons doesn't reference BASE, need to change this function if BASE changes
	if (n < 1e7)
	    return [n];
	if (n < 1e14)
	    return [n % 1e7, Math.floor(n / 1e7)];
	return [n % 1e7, Math.floor(n / 1e7) % 1e7, Math.floor(n / 1e14)];
    }

    function arrayToSmall(arr) { // If BASE changes this function may need to change
	trim(arr);
	var length = arr.length;
	if (length < 4 && compareAbs(arr, MAX_INT_ARR) < 0) {
	    switch (length) {
	    case 0: return 0;
	    case 1: return arr[0];
	    case 2: return arr[0] + arr[1] * BASE;
	    default: return arr[0] + (arr[1] + arr[2] * BASE) * BASE;
	    }
	}
	return arr;
    }

    function trim(v) {
	var i = v.length;
	while (v[--i] === 0);
	v.length = i + 1;
    }

    function createArray(length) { // function shamelessly stolen from Yaffle's library https://github.com/Yaffle/BigInteger
	var x = new Array(length);
	var i = -1;
	while (++i < length) {
	    x[i] = 0;
	}
	return x;
    }

    function truncate(n) {
	if (n > 0) return Math.floor(n);
	return Math.ceil(n);
    }

    function add(a, b) { // assumes a and b are arrays with a.length >= b.length
	var l_a = a.length,
	    l_b = b.length,
	    r = new Array(l_a),
	    carry = 0,
	    base = BASE,
	    sum, i;
	for (i = 0; i < l_b; i++) {
	    sum = a[i] + b[i] + carry;
	    carry = sum >= base ? 1 : 0;
	    r[i] = sum - carry * base;
	}
	while (i < l_a) {
	    sum = a[i] + carry;
	    carry = sum === base ? 1 : 0;
	    r[i++] = sum - carry * base;
	}
	if (carry > 0) r.push(carry);
	return r;
    }

    function addAny(a, b) {
	if (a.length >= b.length) return add(a, b);
	return add(b, a);
    }

    function addSmall(a, carry) { // assumes a is array, carry is number with 0 <= carry < MAX_INT
	var l = a.length,
	    r = new Array(l),
	    base = BASE,
	    sum, i;
	for (i = 0; i < l; i++) {
	    sum = a[i] - base + carry;
	    carry = Math.floor(sum / base);
	    r[i] = sum - carry * base;
	    carry += 1;
	}
	while (carry > 0) {
	    r[i++] = carry % base;
	    carry = Math.floor(carry / base);
	}
	return r;
    }

    BigInteger.prototype.add = function (v) {
	var n = parseValue(v);
	if (this.sign !== n.sign) {
	    return this.subtract(n.negate());
	}
	var a = this.value, b = n.value;
	if (n.isSmall) {
	    return new BigInteger(addSmall(a, Math.abs(b)), this.sign);
	}
	return new BigInteger(addAny(a, b), this.sign);
    };
    BigInteger.prototype.plus = BigInteger.prototype.add;

    SmallInteger.prototype.add = function (v) {
	var n = parseValue(v);
	var a = this.value;
	if (a < 0 !== n.sign) {
	    return this.subtract(n.negate());
	}
	var b = n.value;
	if (n.isSmall) {
	    if (isPrecise(a + b)) return new SmallInteger(a + b);
	    b = smallToArray(Math.abs(b));
	}
	return new BigInteger(addSmall(b, Math.abs(a)), a < 0);
    };
    SmallInteger.prototype.plus = SmallInteger.prototype.add;

    function subtract(a, b) { // assumes a and b are arrays with a >= b
	var a_l = a.length,
	    b_l = b.length,
	    r = new Array(a_l),
	    borrow = 0,
	    base = BASE,
	    i, difference;
	for (i = 0; i < b_l; i++) {
	    difference = a[i] - borrow - b[i];
	    if (difference < 0) {
		difference += base;
		borrow = 1;
	    } else borrow = 0;
	    r[i] = difference;
	}
	for (i = b_l; i < a_l; i++) {
	    difference = a[i] - borrow;
	    if (difference < 0) difference += base;
	    else {
		r[i++] = difference;
		break;
	    }
	    r[i] = difference;
	}
	for (; i < a_l; i++) {
	    r[i] = a[i];
	}
	trim(r);
	return r;
    }

    function subtractAny(a, b, sign) {
	var value;
	if (compareAbs(a, b) >= 0) {
	    value = subtract(a, b);
	} else {
	    value = subtract(b, a);
	    sign = !sign;
	}
	value = arrayToSmall(value);
	if (typeof value === "number") {
	    if (sign) value = -value;
	    return new SmallInteger(value);
	}
	return new BigInteger(value, sign);
    }

    function subtractSmall(a, b, sign) { // assumes a is array, b is number with 0 <= b < MAX_INT
	var l = a.length,
	    r = new Array(l),
	    carry = -b,
	    base = BASE,
	    i, difference;
	for (i = 0; i < l; i++) {
	    difference = a[i] + carry;
	    carry = Math.floor(difference / base);
	    difference %= base;
	    r[i] = difference < 0 ? difference + base : difference;
	}
	r = arrayToSmall(r);
	if (typeof r === "number") {
	    if (sign) r = -r;
	    return new SmallInteger(r);
	} return new BigInteger(r, sign);
    }

    BigInteger.prototype.subtract = function (v) {
	var n = parseValue(v);
	if (this.sign !== n.sign) {
	    return this.add(n.negate());
	}
	var a = this.value, b = n.value;
	if (n.isSmall)
	    return subtractSmall(a, Math.abs(b), this.sign);
	return subtractAny(a, b, this.sign);
    };
    BigInteger.prototype.minus = BigInteger.prototype.subtract;

    SmallInteger.prototype.subtract = function (v) {
	var n = parseValue(v);
	var a = this.value;
	if (a < 0 !== n.sign) {
	    return this.add(n.negate());
	}
	var b = n.value;
	if (n.isSmall) {
	    return new SmallInteger(a - b);
	}
	return subtractSmall(b, Math.abs(a), a >= 0);
    };
        SmallInteger.prototype.minus = SmallInteger.prototype.subtract;

BigInteger.prototype.negate = function () {
    return new BigInteger(this.value, !this.sign);
};
SmallInteger.prototype.negate = function () {
    var sign = this.sign;
    var small = new SmallInteger(-this.value);
    small.sign = !sign;
    return small;
};

BigInteger.prototype.abs = function () {
    return new BigInteger(this.value, false);
};
SmallInteger.prototype.abs = function () {
    return new SmallInteger(Math.abs(this.value));
};

function multiplyLong(a, b) {
    var a_l = a.length,
	b_l = b.length,
	l = a_l + b_l,
	r = createArray(l),
	base = BASE,
	product, carry, i, a_i, b_j;
    for (i = 0; i < a_l; ++i) {
	a_i = a[i];
	for (var j = 0; j < b_l; ++j) {
	    b_j = b[j];
	    product = a_i * b_j + r[i + j];
	    carry = Math.floor(product / base);
	    r[i + j] = product - carry * base;
	    r[i + j + 1] += carry;
	}
    }
    trim(r);
    return r;
}

function multiplySmall(a, b) { // assumes a is array, b is number with |b| < BASE
    var l = a.length,
	r = new Array(l),
	base = BASE,
	carry = 0,
	product, i;
    for (i = 0; i < l; i++) {
	product = a[i] * b + carry;
	carry = Math.floor(product / base);
	r[i] = product - carry * base;
    }
    while (carry > 0) {
	r[i++] = carry % base;
	carry = Math.floor(carry / base);
    }
    return r;
}

function shiftLeft(x, n) {
    var r = [];
    while (n-- > 0) r.push(0);
    return r.concat(x);
}

function multiplyKaratsuba(x, y) {
    var n = Math.max(x.length, y.length);

    if (n <= 30) return multiplyLong(x, y);
    n = Math.ceil(n / 2);

    var b = x.slice(n),
	a = x.slice(0, n),
	d = y.slice(n),
	c = y.slice(0, n);

    var ac = multiplyKaratsuba(a, c),
	bd = multiplyKaratsuba(b, d),
	abcd = multiplyKaratsuba(addAny(a, b), addAny(c, d));

    var product = addAny(addAny(ac, shiftLeft(subtract(subtract(abcd, ac), bd), n)), shiftLeft(bd, 2 * n));
    trim(product);
    return product;
}

// The following function is derived from a surface fit of a graph plotting the performance difference
// between long multiplication and karatsuba multiplication versus the lengths of the two arrays.
function useKaratsuba(l1, l2) {
    return -0.012 * l1 - 0.012 * l2 + 0.000015 * l1 * l2 > 0;
}

BigInteger.prototype.multiply = function (v) {
    var n = parseValue(v),
	a = this.value, b = n.value,
	sign = this.sign !== n.sign,
	abs;
    if (n.isSmall) {
	if (b === 0) return Integer[0];
	if (b === 1) return this;
	if (b === -1) return this.negate();
	abs = Math.abs(b);
	if (abs < BASE) {
	    return new BigInteger(multiplySmall(a, abs), sign);
	}
	b = smallToArray(abs);
    }
    if (useKaratsuba(a.length, b.length)) // Karatsuba is only faster for certain array sizes
	return new BigInteger(multiplyKaratsuba(a, b), sign);
    return new BigInteger(multiplyLong(a, b), sign);
};

BigInteger.prototype.times = BigInteger.prototype.multiply;

function multiplySmallAndArray(a, b, sign) { // a >= 0
    if (a < BASE) {
	return new BigInteger(multiplySmall(b, a), sign);
    }
    return new BigInteger(multiplyLong(b, smallToArray(a)), sign);
}
SmallInteger.prototype._multiplyBySmall = function (a) {
    if (isPrecise(a.value * this.value)) {
	return new SmallInteger(a.value * this.value);
    }
    return multiplySmallAndArray(Math.abs(a.value), smallToArray(Math.abs(this.value)), this.sign !== a.sign);
};
BigInteger.prototype._multiplyBySmall = function (a) {
    if (a.value === 0) return Integer[0];
    if (a.value === 1) return this;
    if (a.value === -1) return this.negate();
    return multiplySmallAndArray(Math.abs(a.value), this.value, this.sign !== a.sign);
};
SmallInteger.prototype.multiply = function (v) {
    return parseValue(v)._multiplyBySmall(this);
};
SmallInteger.prototype.times = SmallInteger.prototype.multiply;

function square(a) {
    //console.assert(2 * BASE * BASE < MAX_INT);
    var l = a.length,
	r = createArray(l + l),
	base = BASE,
	product, carry, i, a_i, a_j;
    for (i = 0; i < l; i++) {
	a_i = a[i];
	carry = 0 - a_i * a_i;
	for (var j = i; j < l; j++) {
	    a_j = a[j];
	    product = 2 * (a_i * a_j) + r[i + j] + carry;
	    carry = Math.floor(product / base);
	    r[i + j] = product - carry * base;
	}
	r[i + l] = carry;
    }
    trim(r);
    return r;
}

BigInteger.prototype.square = function () {
    return new BigInteger(square(this.value), false);
};

SmallInteger.prototype.square = function () {
    var value = this.value * this.value;
    if (isPrecise(value)) return new SmallInteger(value);
    return new BigInteger(square(smallToArray(Math.abs(this.value))), false);
};

function divMod1(a, b) { // Left over from previous version. Performs faster than divMod2 on smaller input sizes.
    var a_l = a.length,
	b_l = b.length,
	base = BASE,
	result = createArray(b.length),
	divisorMostSignificantDigit = b[b_l - 1],
	// normalization
	lambda = Math.ceil(base / (2 * divisorMostSignificantDigit)),
	remainder = multiplySmall(a, lambda),
	divisor = multiplySmall(b, lambda),
	quotientDigit, shift, carry, borrow, i, l, q;
    if (remainder.length <= a_l) remainder.push(0);
    divisor.push(0);
    divisorMostSignificantDigit = divisor[b_l - 1];
    for (shift = a_l - b_l; shift >= 0; shift--) {
	quotientDigit = base - 1;
	if (remainder[shift + b_l] !== divisorMostSignificantDigit) {
	    quotientDigit = Math.floor((remainder[shift + b_l] * base + remainder[shift + b_l - 1]) / divisorMostSignificantDigit);
	}
	// quotientDigit <= base - 1
	carry = 0;
	borrow = 0;
	l = divisor.length;
	for (i = 0; i < l; i++) {
	    carry += quotientDigit * divisor[i];
	    q = Math.floor(carry / base);
	    borrow += remainder[shift + i] - (carry - q * base);
	    carry = q;
	    if (borrow < 0) {
		remainder[shift + i] = borrow + base;
		borrow = -1;
	    } else {
		remainder[shift + i] = borrow;
		borrow = 0;
	    }
	}
	while (borrow !== 0) {
	    quotientDigit -= 1;
	    carry = 0;
	    for (i = 0; i < l; i++) {
		carry += remainder[shift + i] - base + divisor[i];
		if (carry < 0) {
		    remainder[shift + i] = carry + base;
		    carry = 0;
		} else {
		    remainder[shift + i] = carry;
		    carry = 1;
		}
	    }
	    borrow += carry;
	}
	result[shift] = quotientDigit;
    }
    // denormalization
    remainder = divModSmall(remainder, lambda)[0];
    return [arrayToSmall(result), arrayToSmall(remainder)];
}

function divMod2(a, b) { // Implementation idea shamelessly stolen from Silent Matt's library http://silentmatt.com/biginteger/
    // Performs faster than divMod1 on larger input sizes.
    var a_l = a.length,
	b_l = b.length,
	result = [],
	part = [],
	base = BASE,
	guess, xlen, highx, highy, check;
    while (a_l) {
	part.unshift(a[--a_l]);
	trim(part);
	if (compareAbs(part, b) < 0) {
	    result.push(0);
	    continue;
	}
	xlen = part.length;
	highx = part[xlen - 1] * base + part[xlen - 2];
	highy = b[b_l - 1] * base + b[b_l - 2];
	if (xlen > b_l) {
	    highx = (highx + 1) * base;
	}
	guess = Math.ceil(highx / highy);
	do {
	    check = multiplySmall(b, guess);
	    if (compareAbs(check, part) <= 0) break;
	    guess--;
	} while (guess);
	result.push(guess);
	part = subtract(part, check);
    }
    result.reverse();
    return [arrayToSmall(result), arrayToSmall(part)];
}

function divModSmall(value, lambda) {
    var length = value.length,
	quotient = createArray(length),
	base = BASE,
	i, q, remainder, divisor;
    remainder = 0;
    for (i = length - 1; i >= 0; --i) {
	divisor = remainder * base + value[i];
	q = truncate(divisor / lambda);
	remainder = divisor - q * lambda;
	quotient[i] = q | 0;
    }
    return [quotient, remainder | 0];
}

function divModAny(self, v) {
    var value, n = parseValue(v);
    var a = self.value, b = n.value;
    var quotient;
    if (b === 0) throw new Error("Cannot divide by zero");
    if (self.isSmall) {
	if (n.isSmall) {
	    return [new SmallInteger(truncate(a / b)), new SmallInteger(a % b)];
	}
	return [Integer[0], self];
    }
    if (n.isSmall) {
	if (b === 1) return [self, Integer[0]];
	if (b == -1) return [self.negate(), Integer[0]];
	var abs = Math.abs(b);
	if (abs < BASE) {
	    value = divModSmall(a, abs);
	    quotient = arrayToSmall(value[0]);
	    var remainder = value[1];
	    if (self.sign) remainder = -remainder;
	    if (typeof quotient === "number") {
		if (self.sign !== n.sign) quotient = -quotient;
		return [new SmallInteger(quotient), new SmallInteger(remainder)];
	    }
	    return [new BigInteger(quotient, self.sign !== n.sign), new SmallInteger(remainder)];
	}
	b = smallToArray(abs);
    }
    var comparison = compareAbs(a, b);
    if (comparison === -1) return [Integer[0], self];
    if (comparison === 0) return [Integer[self.sign === n.sign ? 1 : -1], Integer[0]];

    // divMod1 is faster on smaller input sizes
    if (a.length + b.length <= 200)
	value = divMod1(a, b);
    else value = divMod2(a, b);

    quotient = value[0];
    var qSign = self.sign !== n.sign,
	mod = value[1],
	mSign = self.sign;
    if (typeof quotient === "number") {
	if (qSign) quotient = -quotient;
	quotient = new SmallInteger(quotient);
    } else quotient = new BigInteger(quotient, qSign);
    if (typeof mod === "number") {
	if (mSign) mod = -mod;
	mod = new SmallInteger(mod);
    } else mod = new BigInteger(mod, mSign);
    return [quotient, mod];
}

BigInteger.prototype.divmod = function (v) {
    var result = divModAny(this, v);
    return {
	quotient: result[0],
	remainder: result[1]
    };
};
SmallInteger.prototype.divmod = BigInteger.prototype.divmod;

BigInteger.prototype.divide = function (v) {
    return divModAny(this, v)[0];
};
SmallInteger.prototype.over = SmallInteger.prototype.divide = BigInteger.prototype.over = BigInteger.prototype.divide;

BigInteger.prototype.mod = function (v) {
    return divModAny(this, v)[1];
};
SmallInteger.prototype.remainder = SmallInteger.prototype.mod = BigInteger.prototype.remainder = BigInteger.prototype.mod;

BigInteger.prototype.pow = function (v) {
    var n = parseValue(v),
	a = this.value,
	b = n.value,
	value, x, y;
    if (b === 0) return Integer[1];
    if (a === 0) return Integer[0];
    if (a === 1) return Integer[1];
    if (a === -1) return n.isEven() ? Integer[1] : Integer[-1];
    if (n.sign) {
	return Integer[0];
    }
    if (!n.isSmall) throw new Error("The exponent " + n.toString() + " is too large.");
    if (this.isSmall) {
	if (isPrecise(value = Math.pow(a, b)))
	    return new SmallInteger(truncate(value));
    }
    x = this;
    y = Integer[1];
    while (true) {
	if (b & 1 === 1) {
	    y = y.times(x);
	    --b;
	}
	if (b === 0) break;
	b /= 2;
	x = x.square();
    }
    return y;
};
SmallInteger.prototype.pow = BigInteger.prototype.pow;

BigInteger.prototype.modPow = function (exp, mod) {
    exp = parseValue(exp);
    mod = parseValue(mod);
    if (mod.isZero()) throw new Error("Cannot take modPow with modulus 0");
    var r = Integer[1],
	base = this.mod(mod);
    while (exp.isPositive()) {
	if (base.isZero()) return Integer[0];
	if (exp.isOdd()) r = r.multiply(base).mod(mod);
	exp = exp.divide(2);
	base = base.square().mod(mod);
    }
    return r;
};
SmallInteger.prototype.modPow = BigInteger.prototype.modPow;

function compareAbs(a, b) {
    if (a.length !== b.length) {
	return a.length > b.length ? 1 : -1;
    }
    for (var i = a.length - 1; i >= 0; i--) {
	if (a[i] !== b[i]) return a[i] > b[i] ? 1 : -1;
    }
    return 0;
}

BigInteger.prototype.compareAbs = function (v) {
    var n = parseValue(v),
	a = this.value,
	b = n.value;
    if (n.isSmall) return 1;
    return compareAbs(a, b);
};
SmallInteger.prototype.compareAbs = function (v) {
    var n = parseValue(v),
	a = Math.abs(this.value),
	b = n.value;
    if (n.isSmall) {
	b = Math.abs(b);
	return a === b ? 0 : a > b ? 1 : -1;
    }
    return -1;
};

BigInteger.prototype.compare = function (v) {
    // See discussion about comparison with Infinity:
    // https://github.com/peterolson/BigInteger.js/issues/61
    if (v === Infinity) {
	return -1;
    }
    if (v === -Infinity) {
	return 1;
    }

    var n = parseValue(v),
	a = this.value,
	b = n.value;
    if (this.sign !== n.sign) {
	return n.sign ? 1 : -1;
    }
    if (n.isSmall) {
	return this.sign ? -1 : 1;
    }
    return compareAbs(a, b) * (this.sign ? -1 : 1);
};
BigInteger.prototype.compareTo = BigInteger.prototype.compare;

SmallInteger.prototype.compare = function (v) {
    if (v === Infinity) {
	return -1;
    }
    if (v === -Infinity) {
	return 1;
    }

    var n = parseValue(v),
	a = this.value,
	b = n.value;
    if (n.isSmall) {
	return a == b ? 0 : a > b ? 1 : -1;
    }
    if (a < 0 !== n.sign) {
	return a < 0 ? -1 : 1;
    }
    return a < 0 ? 1 : -1;
};
SmallInteger.prototype.compareTo = SmallInteger.prototype.compare;

BigInteger.prototype.equals = function (v) {
    return this.compare(v) === 0;
};
SmallInteger.prototype.eq = SmallInteger.prototype.equals = BigInteger.prototype.eq = BigInteger.prototype.equals;

BigInteger.prototype.notEquals = function (v) {
    return this.compare(v) !== 0;
};
SmallInteger.prototype.neq = SmallInteger.prototype.notEquals = BigInteger.prototype.neq = BigInteger.prototype.notEquals;

BigInteger.prototype.greater = function (v) {
    return this.compare(v) > 0;
};
SmallInteger.prototype.gt = SmallInteger.prototype.greater = BigInteger.prototype.gt = BigInteger.prototype.greater;

BigInteger.prototype.lesser = function (v) {
    return this.compare(v) < 0;
};
SmallInteger.prototype.lt = SmallInteger.prototype.lesser = BigInteger.prototype.lt = BigInteger.prototype.lesser;

BigInteger.prototype.greaterOrEquals = function (v) {
    return this.compare(v) >= 0;
};
SmallInteger.prototype.geq = SmallInteger.prototype.greaterOrEquals = BigInteger.prototype.geq = BigInteger.prototype.greaterOrEquals;

BigInteger.prototype.lesserOrEquals = function (v) {
    return this.compare(v) <= 0;
};
SmallInteger.prototype.leq = SmallInteger.prototype.lesserOrEquals = BigInteger.prototype.leq = BigInteger.prototype.lesserOrEquals;

BigInteger.prototype.isEven = function () {
    return (this.value[0] & 1) === 0;
};
SmallInteger.prototype.isEven = function () {
    return (this.value & 1) === 0;
};

BigInteger.prototype.isOdd = function () {
    return (this.value[0] & 1) === 1;
};
SmallInteger.prototype.isOdd = function () {
    return (this.value & 1) === 1;
};

BigInteger.prototype.isPositive = function () {
    return !this.sign;
};
SmallInteger.prototype.isPositive = function () {
    return this.value > 0;
};

BigInteger.prototype.isNegative = function () {
    return this.sign;
};
SmallInteger.prototype.isNegative = function () {
    return this.value < 0;
};

BigInteger.prototype.isUnit = function () {
    return false;
};
SmallInteger.prototype.isUnit = function () {
    return Math.abs(this.value) === 1;
};

BigInteger.prototype.isZero = function () {
    return false;
};
SmallInteger.prototype.isZero = function () {
    return this.value === 0;
};
BigInteger.prototype.isDivisibleBy = function (v) {
    var n = parseValue(v);
    var value = n.value;
    if (value === 0) return false;
    if (value === 1) return true;
    if (value === 2) return this.isEven();
    return this.mod(n).equals(Integer[0]);
};
SmallInteger.prototype.isDivisibleBy = BigInteger.prototype.isDivisibleBy;

function isBasicPrime(v) {
    var n = v.abs();
    if (n.isUnit()) return false;
    if (n.equals(2) || n.equals(3) || n.equals(5)) return true;
    if (n.isEven() || n.isDivisibleBy(3) || n.isDivisibleBy(5)) return false;
    if (n.lesser(25)) return true;
    // we don't know if it's prime: let the other functions figure it out
}

BigInteger.prototype.isPrime = function () {
    var isPrime = isBasicPrime(this);
    if (isPrime !== undefined) return isPrime;
    var n = this.abs(),
	nPrev = n.prev();
    var a = [2, 3, 5, 7, 11, 13, 17, 19],
	b = nPrev,
	d, t, i, x;
    while (b.isEven()) b = b.divide(2);
    for (i = 0; i < a.length; i++) {
	x = bigInt(a[i]).modPow(b, n);
	if (x.equals(Integer[1]) || x.equals(nPrev)) continue;
	for (t = true, d = b; t && d.lesser(nPrev); d = d.multiply(2)) {
	    x = x.square().mod(n);
	    if (x.equals(nPrev)) t = false;
	}
	if (t) return false;
    }
    return true;
};
SmallInteger.prototype.isPrime = BigInteger.prototype.isPrime;

BigInteger.prototype.isProbablePrime = function (iterations) {
    var isPrime = isBasicPrime(this);
    if (isPrime !== undefined) return isPrime;
    var n = this.abs();
    var t = iterations === undefined ? 5 : iterations;
    // use the Fermat primality test
    for (var i = 0; i < t; i++) {
	var a = bigInt.randBetween(2, n.minus(2));
	if (!a.modPow(n.prev(), n).isUnit()) return false; // definitely composite
    }
    return true; // large chance of being prime
};
SmallInteger.prototype.isProbablePrime = BigInteger.prototype.isProbablePrime;

BigInteger.prototype.modInv = function (n) {
    var t = bigInt.zero, newT = bigInt.one, r = parseValue(n), newR = this.abs(), q, lastT, lastR;
    while (!newR.equals(bigInt.zero)) {
	q = r.divide(newR);
	lastT = t;
	lastR = r;
	t = newT;
	r = newR;
	newT = lastT.subtract(q.multiply(newT));
	newR = lastR.subtract(q.multiply(newR));
    }
    if (!r.equals(1)) throw new Error(this.toString() + " and " + n.toString() + " are not co-prime");
    if (t.compare(0) === -1) {
	t = t.add(n);
    }
    if (this.isNegative()) {
	return t.negate();
    }
    return t;
};

SmallInteger.prototype.modInv = BigInteger.prototype.modInv;

BigInteger.prototype.next = function () {
    var value = this.value;
    if (this.sign) {
	return subtractSmall(value, 1, this.sign);
    }
    return new BigInteger(addSmall(value, 1), this.sign);
};
SmallInteger.prototype.next = function () {
    var value = this.value;
    if (value + 1 < MAX_INT) return new SmallInteger(value + 1);
    return new BigInteger(MAX_INT_ARR, false);
};

BigInteger.prototype.prev = function () {
    var value = this.value;
    if (this.sign) {
	return new BigInteger(addSmall(value, 1), true);
    }
    return subtractSmall(value, 1, this.sign);
};
SmallInteger.prototype.prev = function () {
    var value = this.value;
    if (value - 1 > -MAX_INT) return new SmallInteger(value - 1);
    return new BigInteger(MAX_INT_ARR, true);
};

var powersOfTwo = [1];
while (2 * powersOfTwo[powersOfTwo.length - 1] <= BASE) powersOfTwo.push(2 * powersOfTwo[powersOfTwo.length - 1]);
var powers2Length = powersOfTwo.length, highestPower2 = powersOfTwo[powers2Length - 1];

function shift_isSmall(n) {
    return ((typeof n === "number" || typeof n === "string") && +Math.abs(n) <= BASE) ||
	(n instanceof BigInteger && n.value.length <= 1);
}

BigInteger.prototype.shiftLeft = function (n) {
    if (!shift_isSmall(n)) {
	throw new Error(String(n) + " is too large for shifting.");
    }
    n = +n;
    if (n < 0) return this.shiftRight(-n);
    var result = this;
    if (result.isZero()) return result;
    while (n >= powers2Length) {
	result = result.multiply(highestPower2);
	n -= powers2Length - 1;
    }
    return result.multiply(powersOfTwo[n]);
};
SmallInteger.prototype.shiftLeft = BigInteger.prototype.shiftLeft;

BigInteger.prototype.shiftRight = function (n) {
    var remQuo;
    if (!shift_isSmall(n)) {
	throw new Error(String(n) + " is too large for shifting.");
    }
    n = +n;
    if (n < 0) return this.shiftLeft(-n);
    var result = this;
    while (n >= powers2Length) {
	if (result.isZero() || (result.isNegative() && result.isUnit())) return result;
	remQuo = divModAny(result, highestPower2);
	result = remQuo[1].isNegative() ? remQuo[0].prev() : remQuo[0];
	n -= powers2Length - 1;
    }
    remQuo = divModAny(result, powersOfTwo[n]);
    return remQuo[1].isNegative() ? remQuo[0].prev() : remQuo[0];
};
SmallInteger.prototype.shiftRight = BigInteger.prototype.shiftRight;

function bitwise(x, y, fn) {
    y = parseValue(y);
    var xSign = x.isNegative(), ySign = y.isNegative();
    var xRem = xSign ? x.not() : x,
	yRem = ySign ? y.not() : y;
    var xDigit = 0, yDigit = 0;
    var xDivMod = null, yDivMod = null;
    var result = [];
    while (!xRem.isZero() || !yRem.isZero()) {
	xDivMod = divModAny(xRem, highestPower2);
	xDigit = xDivMod[1].toJSNumber();
	if (xSign) {
	    xDigit = highestPower2 - 1 - xDigit; // two's complement for negative numbers
	}

	yDivMod = divModAny(yRem, highestPower2);
	yDigit = yDivMod[1].toJSNumber();
	if (ySign) {
	    yDigit = highestPower2 - 1 - yDigit; // two's complement for negative numbers
	}

	xRem = xDivMod[0];
	yRem = yDivMod[0];
	result.push(fn(xDigit, yDigit));
    }
    var sum = fn(xSign ? 1 : 0, ySign ? 1 : 0) !== 0 ? bigInt(-1) : bigInt(0);
    for (var i = result.length - 1; i >= 0; i -= 1) {
	sum = sum.multiply(highestPower2).add(bigInt(result[i]));
    }
    return sum;
}

BigInteger.prototype.not = function () {
    return this.negate().prev();
};
SmallInteger.prototype.not = BigInteger.prototype.not;

BigInteger.prototype.and = function (n) {
    return bitwise(this, n, function (a, b) { return a & b; });
};
SmallInteger.prototype.and = BigInteger.prototype.and;

BigInteger.prototype.or = function (n) {
    return bitwise(this, n, function (a, b) { return a | b; });
};
SmallInteger.prototype.or = BigInteger.prototype.or;

BigInteger.prototype.xor = function (n) {
    return bitwise(this, n, function (a, b) { return a ^ b; });
};
SmallInteger.prototype.xor = BigInteger.prototype.xor;

var LOBMASK_I = 1 << 30, LOBMASK_BI = (BASE & -BASE) * (BASE & -BASE) | LOBMASK_I;
function roughLOB(n) { // get lowestOneBit (rough)
    // SmallInteger: return Min(lowestOneBit(n), 1 << 30)
    // BigInteger: return Min(lowestOneBit(n), 1 << 14) [BASE=1e7]
    var v = n.value, x = typeof v === "number" ? v | LOBMASK_I : v[0] + v[1] * BASE | LOBMASK_BI;
    return x & -x;
}

function integerLogarithm(value, base) {
    if (base.compareTo(value) <= 0) {
	var tmp = integerLogarithm(value, base.square(base));
	var p = tmp.p;
	var e = tmp.e;
	var t = p.multiply(base);
	return t.compareTo(value) <= 0 ? { p: t, e: e * 2 + 1 } : { p: p, e: e * 2 };
    }
    return { p: bigInt(1), e: 0 };
}

BigInteger.prototype.bitLength = function () {
    var n = this;
    if (n.compareTo(bigInt(0)) < 0) {
	n = n.negate().subtract(bigInt(1));
    }
    if (n.compareTo(bigInt(0)) === 0) {
	return bigInt(0);
    }
    return bigInt(integerLogarithm(n, bigInt(2)).e).add(bigInt(1));
}
SmallInteger.prototype.bitLength = BigInteger.prototype.bitLength;

function max(a, b) {
    a = parseValue(a);
    b = parseValue(b);
    return a.greater(b) ? a : b;
}
function min(a, b) {
    a = parseValue(a);
    b = parseValue(b);
    return a.lesser(b) ? a : b;
}
function gcd(a, b) {
    a = parseValue(a).abs();
    b = parseValue(b).abs();
    if (a.equals(b)) return a;
    if (a.isZero()) return b;
    if (b.isZero()) return a;
    var c = Integer[1], d, t;
    while (a.isEven() && b.isEven()) {
	d = Math.min(roughLOB(a), roughLOB(b));
	a = a.divide(d);
	b = b.divide(d);
	c = c.multiply(d);
    }
    while (a.isEven()) {
	a = a.divide(roughLOB(a));
    }
    do {
	while (b.isEven()) {
	    b = b.divide(roughLOB(b));
	}
	if (a.greater(b)) {
	    t = b; b = a; a = t;
	}
	b = b.subtract(a);
    } while (!b.isZero());
    return c.isUnit() ? a : a.multiply(c);
}
function lcm(a, b) {
    a = parseValue(a).abs();
    b = parseValue(b).abs();
    return a.divide(gcd(a, b)).multiply(b);
}
function randBetween(a, b) {
    a = parseValue(a);
    b = parseValue(b);
    var low = min(a, b), high = max(a, b);
    var range = high.subtract(low).add(1);
    if (range.isSmall) return low.add(Math.floor(Math.random() * range));
    var length = range.value.length - 1;
    var result = [], restricted = true;
    for (var i = length; i >= 0; i--) {
	var top = restricted ? range.value[i] : BASE;
	var digit = truncate(Math.random() * top);
	result.unshift(digit);
	if (digit < top) restricted = false;
    }
    result = arrayToSmall(result);
    return low.add(typeof result === "number" ? new SmallInteger(result) : new BigInteger(result, false));
}
var parseBase = function (text, base) {
    var length = text.length;
    var i;
    var absBase = Math.abs(base);
    for (var i = 0; i < length; i++) {
	var c = text[i].toLowerCase();
	if (c === "-") continue;
	if (/[a-z0-9]/.test(c)) {
	    if (/[0-9]/.test(c) && +c >= absBase) {
		if (c === "1" && absBase === 1) continue;
		throw new Error(c + " is not a valid digit in base " + base + ".");
	    } else if (c.charCodeAt(0) - 87 >= absBase) {
		throw new Error(c + " is not a valid digit in base " + base + ".");
	    }
	}
    }
    if (2 <= base && base <= 36) {
	if (length <= LOG_MAX_INT / Math.log(base)) {
	    var result = parseInt(text, base);
	    if (isNaN(result)) {
		throw new Error(c + " is not a valid digit in base " + base + ".");
	    }
	    return new SmallInteger(parseInt(text, base));
	}
    }
    base = parseValue(base);
    var digits = [];
    var isNegative = text[0] === "-";
    for (i = isNegative ? 1 : 0; i < text.length; i++) {
	var c = text[i].toLowerCase(),
	    charCode = c.charCodeAt(0);
	if (48 <= charCode && charCode <= 57) digits.push(parseValue(c));
	else if (97 <= charCode && charCode <= 122) digits.push(parseValue(c.charCodeAt(0) - 87));
	else if (c === "<") {
	    var start = i;
	    do { i++; } while (text[i] !== ">");
	    digits.push(parseValue(text.slice(start + 1, i)));
	}
	else throw new Error(c + " is not a valid character");
    }
    return parseBaseFromArray(digits, base, isNegative);
};

function parseBaseFromArray(digits, base, isNegative) {
    var val = Integer[0], pow = Integer[1], i;
    for (i = digits.length - 1; i >= 0; i--) {
	val = val.add(digits[i].times(pow));
	pow = pow.times(base);
    }
    return isNegative ? val.negate() : val;
}

function stringify(digit) {
    if (digit <= 35) {
	return "0123456789abcdefghijklmnopqrstuvwxyz".charAt(digit);
    }
    return "<" + digit + ">";
}

function toBase(n, base) {
    base = bigInt(base);
    if (base.isZero()) {
	if (n.isZero()) return { value: [0], isNegative: false };
	throw new Error("Cannot convert nonzero numbers to base 0.");
    }
    if (base.equals(-1)) {
	if (n.isZero()) return { value: [0], isNegative: false };
	if (n.isNegative())
	    return {
		value: [].concat.apply([], Array.apply(null, Array(-n))
				       .map(Array.prototype.valueOf, [1, 0])
				      ),
		isNegative: false
	    };

	var arr = Array.apply(null, Array(+n - 1))
	    .map(Array.prototype.valueOf, [0, 1]);
	arr.unshift([1]);
	return {
	    value: [].concat.apply([], arr),
	    isNegative: false
	};
    }

    var neg = false;
    if (n.isNegative() && base.isPositive()) {
	neg = true;
	n = n.abs();
    }
    if (base.equals(1)) {
	if (n.isZero()) return { value: [0], isNegative: false };

	return {
	    value: Array.apply(null, Array(+n))
	        .map(Number.prototype.valueOf, 1),
	    isNegative: neg
	};
    }
    var out = [];
    var left = n, divmod;
    while (left.isNegative() || left.compareAbs(base) >= 0) {
	divmod = left.divmod(base);
	left = divmod.quotient;
	var digit = divmod.remainder;
	if (digit.isNegative()) {
	    digit = base.minus(digit).abs();
	    left = left.next();
	}
	out.push(digit.toJSNumber());
    }
    out.push(left.toJSNumber());
    return { value: out.reverse(), isNegative: neg };
}

function toBaseString(n, base) {
    var arr = toBase(n, base);
    return (arr.isNegative ? "-" : "") + arr.value.map(stringify).join('');
}

BigInteger.prototype.toArray = function (radix) {
    return toBase(this, radix);
};

SmallInteger.prototype.toArray = function (radix) {
    return toBase(this, radix);
};

BigInteger.prototype.toString = function (radix) {
    if (radix === undefined) radix = 10;
    if (radix !== 10) return toBaseString(this, radix);
    var v = this.value, l = v.length, str = String(v[--l]), zeros = "0000000", digit;
    while (--l >= 0) {
	digit = String(v[l]);
	str += zeros.slice(digit.length) + digit;
    }
    var sign = this.sign ? "-" : "";
    return sign + str;
};

SmallInteger.prototype.toString = function (radix) {
    if (radix === undefined) radix = 10;
    if (radix != 10) return toBaseString(this, radix);
    return String(this.value);
};
BigInteger.prototype.toJSON = SmallInteger.prototype.toJSON = function () { return this.toString(); }

BigInteger.prototype.valueOf = function () {
    return parseInt(this.toString(), 10);
};
BigInteger.prototype.toJSNumber = BigInteger.prototype.valueOf;

SmallInteger.prototype.valueOf = function () {
    return this.value;
};
SmallInteger.prototype.toJSNumber = SmallInteger.prototype.valueOf;

function parseStringValue(v) {
    if (isPrecise(+v)) {
	var x = +v;
	if (x === truncate(x))
	    return new SmallInteger(x);
	throw new Error("Invalid integer: " + v);
    }
    var sign = v[0] === "-";
    if (sign) v = v.slice(1);
    var split = v.split(/e/i);
    if (split.length > 2) throw new Error("Invalid integer: " + split.join("e"));
    if (split.length === 2) {
	var exp = split[1];
	if (exp[0] === "+") exp = exp.slice(1);
	exp = +exp;
	if (exp !== truncate(exp) || !isPrecise(exp)) throw new Error("Invalid integer: " + exp + " is not a valid exponent.");
	var text = split[0];
	var decimalPlace = text.indexOf(".");
	if (decimalPlace >= 0) {
	    exp -= text.length - decimalPlace - 1;
	    text = text.slice(0, decimalPlace) + text.slice(decimalPlace + 1);
	}
	if (exp < 0) throw new Error("Cannot include negative exponent part for integers");
	text += (new Array(exp + 1)).join("0");
	v = text;
    }
    var isValid = /^([0-9][0-9]*)$/.test(v);
    if (!isValid) throw new Error("Invalid integer: " + v);
    var r = [], max = v.length, l = LOG_BASE, min = max - l;
    while (max > 0) {
	r.push(+v.slice(min, max));
	min -= l;
	if (min < 0) min = 0;
	max -= l;
    }
    trim(r);
    return new BigInteger(r, sign);
}

function parseNumberValue(v) {
    if (isPrecise(v)) {
	if (v !== truncate(v)) throw new Error(v + " is not an integer.");
	return new SmallInteger(v);
    }
    return parseStringValue(v.toString());
}

function parseValue(v) {
    if (typeof v === "number") {
	return parseNumberValue(v);
    }
    if (typeof v === "string") {
	return parseStringValue(v);
    }
    return v;
}
// Pre-define numbers in range [-999,999]
for (var i = 0; i < 1000; i++) {
    Integer[i] = new SmallInteger(i);
    if (i > 0) Integer[-i] = new SmallInteger(-i);
}
// Backwards compatibility
Integer.one = Integer[1];
Integer.zero = Integer[0];
Integer.minusOne = Integer[-1];
Integer.max = max;
Integer.min = min;
Integer.gcd = gcd;
Integer.lcm = lcm;
Integer.isInstance = function (x) { return x instanceof BigInteger || x instanceof SmallInteger; };
Integer.randBetween = randBetween;

Integer.fromArray = function (digits, base, isNegative) {
    return parseBaseFromArray(digits.map(parseValue), parseValue(base || 10), isNegative);
};

return Integer;
})();

// Node.js check
if (typeof module !== "undefined" && module.hasOwnProperty("exports")) {
    module.exports = bigInt;
}

//amd check
if (typeof define === "function" && define.amd) {
    define("big-integer", [], function () {
	return bigInt;
    });
}

"use strict";var sjcl={cipher:{},hash:{},keyexchange:{},mode:{},misc:{},codec:{},exception:{corrupt:function(a){this.toString=function(){return"CORRUPT: "+this.message};this.message=a},invalid:function(a){this.toString=function(){return"INVALID: "+this.message};this.message=a},bug:function(a){this.toString=function(){return"BUG: "+this.message};this.message=a},notReady:function(a){this.toString=function(){return"NOT READY: "+this.message};this.message=a}}};
sjcl.cipher.aes=function(a){this.s[0][0][0]||this.O();var b,c,d,e,f=this.s[0][4],g=this.s[1];b=a.length;var h=1;if(4!==b&&6!==b&&8!==b)throw new sjcl.exception.invalid("invalid aes key size");this.b=[d=a.slice(0),e=[]];for(a=b;a<4*b+28;a++){c=d[a-1];if(0===a%b||8===b&&4===a%b)c=f[c>>>24]<<24^f[c>>16&255]<<16^f[c>>8&255]<<8^f[c&255],0===a%b&&(c=c<<8^c>>>24^h<<24,h=h<<1^283*(h>>7));d[a]=d[a-b]^c}for(b=0;a;b++,a--)c=d[b&3?a:a-4],e[b]=4>=a||4>b?c:g[0][f[c>>>24]]^g[1][f[c>>16&255]]^g[2][f[c>>8&255]]^g[3][f[c&
255]]};
sjcl.cipher.aes.prototype={encrypt:function(a){return t(this,a,0)},decrypt:function(a){return t(this,a,1)},s:[[[],[],[],[],[]],[[],[],[],[],[]]],O:function(){var a=this.s[0],b=this.s[1],c=a[4],d=b[4],e,f,g,h=[],k=[],l,n,m,p;for(e=0;0x100>e;e++)k[(h[e]=e<<1^283*(e>>7))^e]=e;for(f=g=0;!c[f];f^=l||1,g=k[g]||1)for(m=g^g<<1^g<<2^g<<3^g<<4,m=m>>8^m&255^99,c[f]=m,d[m]=f,n=h[e=h[l=h[f]]],p=0x1010101*n^0x10001*e^0x101*l^0x1010100*f,n=0x101*h[m]^0x1010100*m,e=0;4>e;e++)a[e][f]=n=n<<24^n>>>8,b[e][m]=p=p<<24^p>>>8;for(e=
0;5>e;e++)a[e]=a[e].slice(0),b[e]=b[e].slice(0)}};
function t(a,b,c){if(4!==b.length)throw new sjcl.exception.invalid("invalid aes block size");var d=a.b[c],e=b[0]^d[0],f=b[c?3:1]^d[1],g=b[2]^d[2];b=b[c?1:3]^d[3];var h,k,l,n=d.length/4-2,m,p=4,r=[0,0,0,0];h=a.s[c];a=h[0];var q=h[1],v=h[2],w=h[3],x=h[4];for(m=0;m<n;m++)h=a[e>>>24]^q[f>>16&255]^v[g>>8&255]^w[b&255]^d[p],k=a[f>>>24]^q[g>>16&255]^v[b>>8&255]^w[e&255]^d[p+1],l=a[g>>>24]^q[b>>16&255]^v[e>>8&255]^w[f&255]^d[p+2],b=a[b>>>24]^q[e>>16&255]^v[f>>8&255]^w[g&255]^d[p+3],p+=4,e=h,f=k,g=l;for(m=
0;4>m;m++)r[c?3&-m:m]=x[e>>>24]<<24^x[f>>16&255]<<16^x[g>>8&255]<<8^x[b&255]^d[p++],h=e,e=f,f=g,g=b,b=h;return r}
sjcl.bitArray={bitSlice:function(a,b,c){a=sjcl.bitArray.$(a.slice(b/32),32-(b&31)).slice(1);return void 0===c?a:sjcl.bitArray.clamp(a,c-b)},extract:function(a,b,c){var d=Math.floor(-b-c&31);return((b+c-1^b)&-32?a[b/32|0]<<32-d^a[b/32+1|0]>>>d:a[b/32|0]>>>d)&(1<<c)-1},concat:function(a,b){if(0===a.length||0===b.length)return a.concat(b);var c=a[a.length-1],d=sjcl.bitArray.getPartial(c);return 32===d?a.concat(b):sjcl.bitArray.$(b,d,c|0,a.slice(0,a.length-1))},bitLength:function(a){var b=a.length;return 0===
b?0:32*(b-1)+sjcl.bitArray.getPartial(a[b-1])},clamp:function(a,b){if(32*a.length<b)return a;a=a.slice(0,Math.ceil(b/32));var c=a.length;b=b&31;0<c&&b&&(a[c-1]=sjcl.bitArray.partial(b,a[c-1]&2147483648>>b-1,1));return a},partial:function(a,b,c){return 32===a?b:(c?b|0:b<<32-a)+0x10000000000*a},getPartial:function(a){return Math.round(a/0x10000000000)||32},equal:function(a,b){if(sjcl.bitArray.bitLength(a)!==sjcl.bitArray.bitLength(b))return!1;var c=0,d;for(d=0;d<a.length;d++)c|=a[d]^b[d];return 0===
c},$:function(a,b,c,d){var e;e=0;for(void 0===d&&(d=[]);32<=b;b-=32)d.push(c),c=0;if(0===b)return d.concat(a);for(e=0;e<a.length;e++)d.push(c|a[e]>>>b),c=a[e]<<32-b;e=a.length?a[a.length-1]:0;a=sjcl.bitArray.getPartial(e);d.push(sjcl.bitArray.partial(b+a&31,32<b+a?c:d.pop(),1));return d},i:function(a,b){return[a[0]^b[0],a[1]^b[1],a[2]^b[2],a[3]^b[3]]},byteswapM:function(a){var b,c;for(b=0;b<a.length;++b)c=a[b],a[b]=c>>>24|c>>>8&0xff00|(c&0xff00)<<8|c<<24;return a}};
sjcl.codec.utf8String={fromBits:function(a){var b="",c=sjcl.bitArray.bitLength(a),d,e;for(d=0;d<c/8;d++)0===(d&3)&&(e=a[d/4]),b+=String.fromCharCode(e>>>8>>>8>>>8),e<<=8;return decodeURIComponent(escape(b))},toBits:function(a){a=unescape(encodeURIComponent(a));var b=[],c,d=0;for(c=0;c<a.length;c++)d=d<<8|a.charCodeAt(c),3===(c&3)&&(b.push(d),d=0);c&3&&b.push(sjcl.bitArray.partial(8*(c&3),d));return b}};
sjcl.codec.hex={fromBits:function(a){var b="",c;for(c=0;c<a.length;c++)b+=((a[c]|0)+0xf00000000000).toString(16).substr(4);return b.substr(0,sjcl.bitArray.bitLength(a)/4)},toBits:function(a){var b,c=[],d;a=a.replace(/\s|0x/g,"");d=a.length;a=a+"00000000";for(b=0;b<a.length;b+=8)c.push(parseInt(a.substr(b,8),16)^0);return sjcl.bitArray.clamp(c,4*d)}};
sjcl.codec.base32={B:"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567",X:"0123456789ABCDEFGHIJKLMNOPQRSTUV",BITS:32,BASE:5,REMAINING:27,fromBits:function(a,b,c){var d=sjcl.codec.base32.BASE,e=sjcl.codec.base32.REMAINING,f="",g=0,h=sjcl.codec.base32.B,k=0,l=sjcl.bitArray.bitLength(a);c&&(h=sjcl.codec.base32.X);for(c=0;f.length*d<l;)f+=h.charAt((k^a[c]>>>g)>>>e),g<d?(k=a[c]<<d-g,g+=e,c++):(k<<=d,g-=d);for(;f.length&7&&!b;)f+="=";return f},toBits:function(a,b){a=a.replace(/\s|=/g,"").toUpperCase();var c=sjcl.codec.base32.BITS,
d=sjcl.codec.base32.BASE,e=sjcl.codec.base32.REMAINING,f=[],g,h=0,k=sjcl.codec.base32.B,l=0,n,m="base32";b&&(k=sjcl.codec.base32.X,m="base32hex");for(g=0;g<a.length;g++){n=k.indexOf(a.charAt(g));if(0>n){if(!b)try{return sjcl.codec.base32hex.toBits(a)}catch(p){}throw new sjcl.exception.invalid("this isn't "+m+"!");}h>e?(h-=e,f.push(l^n>>>h),l=n<<c-h):(h+=d,l^=n<<c-h)}h&56&&f.push(sjcl.bitArray.partial(h&56,l,1));return f}};
sjcl.codec.base32hex={fromBits:function(a,b){return sjcl.codec.base32.fromBits(a,b,1)},toBits:function(a){return sjcl.codec.base32.toBits(a,1)}};
sjcl.codec.base64={B:"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",fromBits:function(a,b,c){var d="",e=0,f=sjcl.codec.base64.B,g=0,h=sjcl.bitArray.bitLength(a);c&&(f=f.substr(0,62)+"-_");for(c=0;6*d.length<h;)d+=f.charAt((g^a[c]>>>e)>>>26),6>e?(g=a[c]<<6-e,e+=26,c++):(g<<=6,e-=6);for(;d.length&3&&!b;)d+="=";return d},toBits:function(a,b){a=a.replace(/\s|=/g,"");var c=[],d,e=0,f=sjcl.codec.base64.B,g=0,h;b&&(f=f.substr(0,62)+"-_");for(d=0;d<a.length;d++){h=f.indexOf(a.charAt(d));
if(0>h)throw new sjcl.exception.invalid("this isn't base64!");26<e?(e-=26,c.push(g^h>>>e),g=h<<32-e):(e+=6,g^=h<<32-e)}e&56&&c.push(sjcl.bitArray.partial(e&56,g,1));return c}};sjcl.codec.base64url={fromBits:function(a){return sjcl.codec.base64.fromBits(a,1,1)},toBits:function(a){return sjcl.codec.base64.toBits(a,1)}};sjcl.hash.sha256=function(a){this.b[0]||this.O();a?(this.F=a.F.slice(0),this.A=a.A.slice(0),this.l=a.l):this.reset()};sjcl.hash.sha256.hash=function(a){return(new sjcl.hash.sha256).update(a).finalize()};
sjcl.hash.sha256.prototype={blockSize:512,reset:function(){this.F=this.Y.slice(0);this.A=[];this.l=0;return this},update:function(a){"string"===typeof a&&(a=sjcl.codec.utf8String.toBits(a));var b,c=this.A=sjcl.bitArray.concat(this.A,a);b=this.l;a=this.l=b+sjcl.bitArray.bitLength(a);if(0x1fffffffffffff<a)throw new sjcl.exception.invalid("Cannot hash more than 2^53 - 1 bits");if("undefined"!==typeof Uint32Array){var d=new Uint32Array(c),e=0;for(b=512+b-(512+b&0x1ff);b<=a;b+=512)u(this,d.subarray(16*e,
16*(e+1))),e+=1;c.splice(0,16*e)}else for(b=512+b-(512+b&0x1ff);b<=a;b+=512)u(this,c.splice(0,16));return this},finalize:function(){var a,b=this.A,c=this.F,b=sjcl.bitArray.concat(b,[sjcl.bitArray.partial(1,1)]);for(a=b.length+2;a&15;a++)b.push(0);b.push(Math.floor(this.l/0x100000000));for(b.push(this.l|0);b.length;)u(this,b.splice(0,16));this.reset();return c},Y:[],b:[],O:function(){function a(a){return 0x100000000*(a-Math.floor(a))|0}for(var b=0,c=2,d,e;64>b;c++){e=!0;for(d=2;d*d<=c;d++)if(0===c%d){e=
!1;break}e&&(8>b&&(this.Y[b]=a(Math.pow(c,.5))),this.b[b]=a(Math.pow(c,1/3)),b++)}}};
function u(a,b){var c,d,e,f=a.F,g=a.b,h=f[0],k=f[1],l=f[2],n=f[3],m=f[4],p=f[5],r=f[6],q=f[7];for(c=0;64>c;c++)16>c?d=b[c]:(d=b[c+1&15],e=b[c+14&15],d=b[c&15]=(d>>>7^d>>>18^d>>>3^d<<25^d<<14)+(e>>>17^e>>>19^e>>>10^e<<15^e<<13)+b[c&15]+b[c+9&15]|0),d=d+q+(m>>>6^m>>>11^m>>>25^m<<26^m<<21^m<<7)+(r^m&(p^r))+g[c],q=r,r=p,p=m,m=n+d|0,n=l,l=k,k=h,h=d+(k&l^n&(k^l))+(k>>>2^k>>>13^k>>>22^k<<30^k<<19^k<<10)|0;f[0]=f[0]+h|0;f[1]=f[1]+k|0;f[2]=f[2]+l|0;f[3]=f[3]+n|0;f[4]=f[4]+m|0;f[5]=f[5]+p|0;f[6]=f[6]+r|0;f[7]=
f[7]+q|0}
sjcl.mode.ccm={name:"ccm",G:[],listenProgress:function(a){sjcl.mode.ccm.G.push(a)},unListenProgress:function(a){a=sjcl.mode.ccm.G.indexOf(a);-1<a&&sjcl.mode.ccm.G.splice(a,1)},fa:function(a){var b=sjcl.mode.ccm.G.slice(),c;for(c=0;c<b.length;c+=1)b[c](a)},encrypt:function(a,b,c,d,e){var f,g=b.slice(0),h=sjcl.bitArray,k=h.bitLength(c)/8,l=h.bitLength(g)/8;e=e||64;d=d||[];if(7>k)throw new sjcl.exception.invalid("ccm: iv must be at least 7 bytes");for(f=2;4>f&&l>>>8*f;f++);f<15-k&&(f=15-k);c=h.clamp(c,
8*(15-f));b=sjcl.mode.ccm.V(a,b,c,d,e,f);g=sjcl.mode.ccm.C(a,g,c,b,e,f);return h.concat(g.data,g.tag)},decrypt:function(a,b,c,d,e){e=e||64;d=d||[];var f=sjcl.bitArray,g=f.bitLength(c)/8,h=f.bitLength(b),k=f.clamp(b,h-e),l=f.bitSlice(b,h-e),h=(h-e)/8;if(7>g)throw new sjcl.exception.invalid("ccm: iv must be at least 7 bytes");for(b=2;4>b&&h>>>8*b;b++);b<15-g&&(b=15-g);c=f.clamp(c,8*(15-b));k=sjcl.mode.ccm.C(a,k,c,l,e,b);a=sjcl.mode.ccm.V(a,k.data,c,d,e,b);if(!f.equal(k.tag,a))throw new sjcl.exception.corrupt("ccm: tag doesn't match");
return k.data},na:function(a,b,c,d,e,f){var g=[],h=sjcl.bitArray,k=h.i;d=[h.partial(8,(b.length?64:0)|d-2<<2|f-1)];d=h.concat(d,c);d[3]|=e;d=a.encrypt(d);if(b.length)for(c=h.bitLength(b)/8,65279>=c?g=[h.partial(16,c)]:0xffffffff>=c&&(g=h.concat([h.partial(16,65534)],[c])),g=h.concat(g,b),b=0;b<g.length;b+=4)d=a.encrypt(k(d,g.slice(b,b+4).concat([0,0,0])));return d},V:function(a,b,c,d,e,f){var g=sjcl.bitArray,h=g.i;e/=8;if(e%2||4>e||16<e)throw new sjcl.exception.invalid("ccm: invalid tag length");
if(0xffffffff<d.length||0xffffffff<b.length)throw new sjcl.exception.bug("ccm: can't deal with 4GiB or more data");c=sjcl.mode.ccm.na(a,d,c,e,g.bitLength(b)/8,f);for(d=0;d<b.length;d+=4)c=a.encrypt(h(c,b.slice(d,d+4).concat([0,0,0])));return g.clamp(c,8*e)},C:function(a,b,c,d,e,f){var g,h=sjcl.bitArray;g=h.i;var k=b.length,l=h.bitLength(b),n=k/50,m=n;c=h.concat([h.partial(8,f-1)],c).concat([0,0,0]).slice(0,4);d=h.bitSlice(g(d,a.encrypt(c)),0,e);if(!k)return{tag:d,data:[]};for(g=0;g<k;g+=4)g>n&&(sjcl.mode.ccm.fa(g/
k),n+=m),c[3]++,e=a.encrypt(c),b[g]^=e[0],b[g+1]^=e[1],b[g+2]^=e[2],b[g+3]^=e[3];return{tag:d,data:h.clamp(b,l)}}};
sjcl.mode.ocb2={name:"ocb2",encrypt:function(a,b,c,d,e,f){if(128!==sjcl.bitArray.bitLength(c))throw new sjcl.exception.invalid("ocb iv must be 128 bits");var g,h=sjcl.mode.ocb2.S,k=sjcl.bitArray,l=k.i,n=[0,0,0,0];c=h(a.encrypt(c));var m,p=[];d=d||[];e=e||64;for(g=0;g+4<b.length;g+=4)m=b.slice(g,g+4),n=l(n,m),p=p.concat(l(c,a.encrypt(l(c,m)))),c=h(c);m=b.slice(g);b=k.bitLength(m);g=a.encrypt(l(c,[0,0,0,b]));m=k.clamp(l(m.concat([0,0,0]),g),b);n=l(n,l(m.concat([0,0,0]),g));n=a.encrypt(l(n,l(c,h(c))));
d.length&&(n=l(n,f?d:sjcl.mode.ocb2.pmac(a,d)));return p.concat(k.concat(m,k.clamp(n,e)))},decrypt:function(a,b,c,d,e,f){if(128!==sjcl.bitArray.bitLength(c))throw new sjcl.exception.invalid("ocb iv must be 128 bits");e=e||64;var g=sjcl.mode.ocb2.S,h=sjcl.bitArray,k=h.i,l=[0,0,0,0],n=g(a.encrypt(c)),m,p,r=sjcl.bitArray.bitLength(b)-e,q=[];d=d||[];for(c=0;c+4<r/32;c+=4)m=k(n,a.decrypt(k(n,b.slice(c,c+4)))),l=k(l,m),q=q.concat(m),n=g(n);p=r-32*c;m=a.encrypt(k(n,[0,0,0,p]));m=k(m,h.clamp(b.slice(c),p).concat([0,
0,0]));l=k(l,m);l=a.encrypt(k(l,k(n,g(n))));d.length&&(l=k(l,f?d:sjcl.mode.ocb2.pmac(a,d)));if(!h.equal(h.clamp(l,e),h.bitSlice(b,r)))throw new sjcl.exception.corrupt("ocb: tag doesn't match");return q.concat(h.clamp(m,p))},pmac:function(a,b){var c,d=sjcl.mode.ocb2.S,e=sjcl.bitArray,f=e.i,g=[0,0,0,0],h=a.encrypt([0,0,0,0]),h=f(h,d(d(h)));for(c=0;c+4<b.length;c+=4)h=d(h),g=f(g,a.encrypt(f(h,b.slice(c,c+4))));c=b.slice(c);128>e.bitLength(c)&&(h=f(h,d(h)),c=e.concat(c,[-2147483648,0,0,0]));g=f(g,c);
return a.encrypt(f(d(f(h,d(h))),g))},S:function(a){return[a[0]<<1^a[1]>>>31,a[1]<<1^a[2]>>>31,a[2]<<1^a[3]>>>31,a[3]<<1^135*(a[0]>>>31)]}};
sjcl.mode.gcm={name:"gcm",encrypt:function(a,b,c,d,e){var f=b.slice(0);b=sjcl.bitArray;d=d||[];a=sjcl.mode.gcm.C(!0,a,f,d,c,e||128);return b.concat(a.data,a.tag)},decrypt:function(a,b,c,d,e){var f=b.slice(0),g=sjcl.bitArray,h=g.bitLength(f);e=e||128;d=d||[];e<=h?(b=g.bitSlice(f,h-e),f=g.bitSlice(f,0,h-e)):(b=f,f=[]);a=sjcl.mode.gcm.C(!1,a,f,d,c,e);if(!g.equal(a.tag,b))throw new sjcl.exception.corrupt("gcm: tag doesn't match");return a.data},ka:function(a,b){var c,d,e,f,g,h=sjcl.bitArray.i;e=[0,0,
0,0];f=b.slice(0);for(c=0;128>c;c++){(d=0!==(a[Math.floor(c/32)]&1<<31-c%32))&&(e=h(e,f));g=0!==(f[3]&1);for(d=3;0<d;d--)f[d]=f[d]>>>1|(f[d-1]&1)<<31;f[0]>>>=1;g&&(f[0]^=-0x1f000000)}return e},j:function(a,b,c){var d,e=c.length;b=b.slice(0);for(d=0;d<e;d+=4)b[0]^=0xffffffff&c[d],b[1]^=0xffffffff&c[d+1],b[2]^=0xffffffff&c[d+2],b[3]^=0xffffffff&c[d+3],b=sjcl.mode.gcm.ka(b,a);return b},C:function(a,b,c,d,e,f){var g,h,k,l,n,m,p,r,q=sjcl.bitArray;m=c.length;p=q.bitLength(c);r=q.bitLength(d);h=q.bitLength(e);
g=b.encrypt([0,0,0,0]);96===h?(e=e.slice(0),e=q.concat(e,[1])):(e=sjcl.mode.gcm.j(g,[0,0,0,0],e),e=sjcl.mode.gcm.j(g,e,[0,0,Math.floor(h/0x100000000),h&0xffffffff]));h=sjcl.mode.gcm.j(g,[0,0,0,0],d);n=e.slice(0);d=h.slice(0);a||(d=sjcl.mode.gcm.j(g,h,c));for(l=0;l<m;l+=4)n[3]++,k=b.encrypt(n),c[l]^=k[0],c[l+1]^=k[1],c[l+2]^=k[2],c[l+3]^=k[3];c=q.clamp(c,p);a&&(d=sjcl.mode.gcm.j(g,h,c));a=[Math.floor(r/0x100000000),r&0xffffffff,Math.floor(p/0x100000000),p&0xffffffff];d=sjcl.mode.gcm.j(g,d,a);k=b.encrypt(e);
d[0]^=k[0];d[1]^=k[1];d[2]^=k[2];d[3]^=k[3];return{tag:q.bitSlice(d,0,f),data:c}}};sjcl.misc.hmac=function(a,b){this.W=b=b||sjcl.hash.sha256;var c=[[],[]],d,e=b.prototype.blockSize/32;this.w=[new b,new b];a.length>e&&(a=b.hash(a));for(d=0;d<e;d++)c[0][d]=a[d]^909522486,c[1][d]=a[d]^1549556828;this.w[0].update(c[0]);this.w[1].update(c[1]);this.R=new b(this.w[0])};
sjcl.misc.hmac.prototype.encrypt=sjcl.misc.hmac.prototype.mac=function(a){if(this.aa)throw new sjcl.exception.invalid("encrypt on already updated hmac called!");this.update(a);return this.digest(a)};sjcl.misc.hmac.prototype.reset=function(){this.R=new this.W(this.w[0]);this.aa=!1};sjcl.misc.hmac.prototype.update=function(a){this.aa=!0;this.R.update(a)};sjcl.misc.hmac.prototype.digest=function(){var a=this.R.finalize(),a=(new this.W(this.w[1])).update(a).finalize();this.reset();return a};
sjcl.misc.pbkdf2=function(a,b,c,d,e){c=c||1E4;if(0>d||0>c)throw new sjcl.exception.invalid("invalid params to pbkdf2");"string"===typeof a&&(a=sjcl.codec.utf8String.toBits(a));"string"===typeof b&&(b=sjcl.codec.utf8String.toBits(b));e=e||sjcl.misc.hmac;a=new e(a);var f,g,h,k,l=[],n=sjcl.bitArray;for(k=1;32*l.length<(d||1);k++){e=f=a.encrypt(n.concat(b,[k]));for(g=1;g<c;g++)for(f=a.encrypt(f),h=0;h<f.length;h++)e[h]^=f[h];l=l.concat(e)}d&&(l=n.clamp(l,d));return l};
sjcl.prng=function(a){this.c=[new sjcl.hash.sha256];this.m=[0];this.P=0;this.H={};this.N=0;this.U={};this.Z=this.f=this.o=this.ha=0;this.b=[0,0,0,0,0,0,0,0];this.h=[0,0,0,0];this.L=void 0;this.M=a;this.D=!1;this.K={progress:{},seeded:{}};this.u=this.ga=0;this.I=1;this.J=2;this.ca=0x10000;this.T=[0,48,64,96,128,192,0x100,384,512,768,1024];this.da=3E4;this.ba=80};
sjcl.prng.prototype={randomWords:function(a,b){var c=[],d;d=this.isReady(b);var e;if(d===this.u)throw new sjcl.exception.notReady("generator isn't seeded");if(d&this.J){d=!(d&this.I);e=[];var f=0,g;this.Z=e[0]=(new Date).valueOf()+this.da;for(g=0;16>g;g++)e.push(0x100000000*Math.random()|0);for(g=0;g<this.c.length&&(e=e.concat(this.c[g].finalize()),f+=this.m[g],this.m[g]=0,d||!(this.P&1<<g));g++);this.P>=1<<this.c.length&&(this.c.push(new sjcl.hash.sha256),this.m.push(0));this.f-=f;f>this.o&&(this.o=
f);this.P++;this.b=sjcl.hash.sha256.hash(this.b.concat(e));this.L=new sjcl.cipher.aes(this.b);for(d=0;4>d&&(this.h[d]=this.h[d]+1|0,!this.h[d]);d++);}for(d=0;d<a;d+=4)0===(d+1)%this.ca&&y(this),e=z(this),c.push(e[0],e[1],e[2],e[3]);y(this);return c.slice(0,a)},setDefaultParanoia:function(a,b){if(0===a&&"Setting paranoia=0 will ruin your security; use it only for testing"!==b)throw new sjcl.exception.invalid("Setting paranoia=0 will ruin your security; use it only for testing");this.M=a},addEntropy:function(a,
b,c){c=c||"user";var d,e,f=(new Date).valueOf(),g=this.H[c],h=this.isReady(),k=0;d=this.U[c];void 0===d&&(d=this.U[c]=this.ha++);void 0===g&&(g=this.H[c]=0);this.H[c]=(this.H[c]+1)%this.c.length;switch(typeof a){case "number":void 0===b&&(b=1);this.c[g].update([d,this.N++,1,b,f,1,a|0]);break;case "object":c=Object.prototype.toString.call(a);if("[object Uint32Array]"===c){e=[];for(c=0;c<a.length;c++)e.push(a[c]);a=e}else for("[object Array]"!==c&&(k=1),c=0;c<a.length&&!k;c++)"number"!==typeof a[c]&&
(k=1);if(!k){if(void 0===b)for(c=b=0;c<a.length;c++)for(e=a[c];0<e;)b++,e=e>>>1;this.c[g].update([d,this.N++,2,b,f,a.length].concat(a))}break;case "string":void 0===b&&(b=a.length);this.c[g].update([d,this.N++,3,b,f,a.length]);this.c[g].update(a);break;default:k=1}if(k)throw new sjcl.exception.bug("random: addEntropy only supports number, array of numbers or string");this.m[g]+=b;this.f+=b;h===this.u&&(this.isReady()!==this.u&&A("seeded",Math.max(this.o,this.f)),A("progress",this.getProgress()))},
isReady:function(a){a=this.T[void 0!==a?a:this.M];return this.o&&this.o>=a?this.m[0]>this.ba&&(new Date).valueOf()>this.Z?this.J|this.I:this.I:this.f>=a?this.J|this.u:this.u},getProgress:function(a){a=this.T[a?a:this.M];return this.o>=a?1:this.f>a?1:this.f/a},startCollectors:function(){if(!this.D){this.a={loadTimeCollector:B(this,this.ma),mouseCollector:B(this,this.oa),keyboardCollector:B(this,this.la),accelerometerCollector:B(this,this.ea),touchCollector:B(this,this.qa)};if(window.addEventListener)window.addEventListener("load",
this.a.loadTimeCollector,!1),window.addEventListener("mousemove",this.a.mouseCollector,!1),window.addEventListener("keypress",this.a.keyboardCollector,!1),window.addEventListener("devicemotion",this.a.accelerometerCollector,!1),window.addEventListener("touchmove",this.a.touchCollector,!1);else if(document.attachEvent)document.attachEvent("onload",this.a.loadTimeCollector),document.attachEvent("onmousemove",this.a.mouseCollector),document.attachEvent("keypress",this.a.keyboardCollector);else throw new sjcl.exception.bug("can't attach event");
this.D=!0}},stopCollectors:function(){this.D&&(window.removeEventListener?(window.removeEventListener("load",this.a.loadTimeCollector,!1),window.removeEventListener("mousemove",this.a.mouseCollector,!1),window.removeEventListener("keypress",this.a.keyboardCollector,!1),window.removeEventListener("devicemotion",this.a.accelerometerCollector,!1),window.removeEventListener("touchmove",this.a.touchCollector,!1)):document.detachEvent&&(document.detachEvent("onload",this.a.loadTimeCollector),document.detachEvent("onmousemove",
this.a.mouseCollector),document.detachEvent("keypress",this.a.keyboardCollector)),this.D=!1)},addEventListener:function(a,b){this.K[a][this.ga++]=b},removeEventListener:function(a,b){var c,d,e=this.K[a],f=[];for(d in e)e.hasOwnProperty(d)&&e[d]===b&&f.push(d);for(c=0;c<f.length;c++)d=f[c],delete e[d]},la:function(){C(this,1)},oa:function(a){var b,c;try{b=a.x||a.clientX||a.offsetX||0,c=a.y||a.clientY||a.offsetY||0}catch(d){c=b=0}0!=b&&0!=c&&this.addEntropy([b,c],2,"mouse");C(this,0)},qa:function(a){a=
a.touches[0]||a.changedTouches[0];this.addEntropy([a.pageX||a.clientX,a.pageY||a.clientY],1,"touch");C(this,0)},ma:function(){C(this,2)},ea:function(a){a=a.accelerationIncludingGravity.x||a.accelerationIncludingGravity.y||a.accelerationIncludingGravity.z;if(window.orientation){var b=window.orientation;"number"===typeof b&&this.addEntropy(b,1,"accelerometer")}a&&this.addEntropy(a,2,"accelerometer");C(this,0)}};
function A(a,b){var c,d=sjcl.random.K[a],e=[];for(c in d)d.hasOwnProperty(c)&&e.push(d[c]);for(c=0;c<e.length;c++)e[c](b)}function C(a,b){"undefined"!==typeof window&&window.performance&&"function"===typeof window.performance.now?a.addEntropy(window.performance.now(),b,"loadtime"):a.addEntropy((new Date).valueOf(),b,"loadtime")}function y(a){a.b=z(a).concat(z(a));a.L=new sjcl.cipher.aes(a.b)}function z(a){for(var b=0;4>b&&(a.h[b]=a.h[b]+1|0,!a.h[b]);b++);return a.L.encrypt(a.h)}
function B(a,b){return function(){b.apply(a,arguments)}}sjcl.random=new sjcl.prng(6);
a:try{var D,E,F,G;if(G="undefined"!==typeof module&&module.exports){var H;try{H=require("crypto")}catch(a){H=null}G=E=H}if(G&&E.randomBytes)D=E.randomBytes(128),D=new Uint32Array((new Uint8Array(D)).buffer),sjcl.random.addEntropy(D,1024,"crypto['randomBytes']");else if("undefined"!==typeof window&&"undefined"!==typeof Uint32Array){F=new Uint32Array(32);if(window.crypto&&window.crypto.getRandomValues)window.crypto.getRandomValues(F);else if(window.msCrypto&&window.msCrypto.getRandomValues)window.msCrypto.getRandomValues(F);
else break a;sjcl.random.addEntropy(F,1024,"crypto['getRandomValues']")}}catch(a){"undefined"!==typeof window&&window.console&&(console.log("There was an error collecting entropy from the browser:"),console.log(a))}
sjcl.json={defaults:{v:1,iter:1E4,ks:128,ts:64,mode:"ccm",adata:"",cipher:"aes"},ja:function(a,b,c,d){c=c||{};d=d||{};var e=sjcl.json,f=e.g({iv:sjcl.random.randomWords(4,0)},e.defaults),g;e.g(f,c);c=f.adata;"string"===typeof f.salt&&(f.salt=sjcl.codec.base64.toBits(f.salt));"string"===typeof f.iv&&(f.iv=sjcl.codec.base64.toBits(f.iv));if(!sjcl.mode[f.mode]||!sjcl.cipher[f.cipher]||"string"===typeof a&&100>=f.iter||64!==f.ts&&96!==f.ts&&128!==f.ts||128!==f.ks&&192!==f.ks&&0x100!==f.ks||2>f.iv.length||
4<f.iv.length)throw new sjcl.exception.invalid("json encrypt: invalid parameters");"string"===typeof a?(g=sjcl.misc.cachedPbkdf2(a,f),a=g.key.slice(0,f.ks/32),f.salt=g.salt):sjcl.ecc&&a instanceof sjcl.ecc.elGamal.publicKey&&(g=a.kem(),f.kemtag=g.tag,a=g.key.slice(0,f.ks/32));"string"===typeof b&&(b=sjcl.codec.utf8String.toBits(b));"string"===typeof c&&(f.adata=c=sjcl.codec.utf8String.toBits(c));g=new sjcl.cipher[f.cipher](a);e.g(d,f);d.key=a;f.ct="ccm"===f.mode&&sjcl.arrayBuffer&&sjcl.arrayBuffer.ccm&&
b instanceof ArrayBuffer?sjcl.arrayBuffer.ccm.encrypt(g,b,f.iv,c,f.ts):sjcl.mode[f.mode].encrypt(g,b,f.iv,c,f.ts);return f},encrypt:function(a,b,c,d){var e=sjcl.json,f=e.ja.apply(e,arguments);return e.encode(f)},ia:function(a,b,c,d){c=c||{};d=d||{};var e=sjcl.json;b=e.g(e.g(e.g({},e.defaults),b),c,!0);var f,g;f=b.adata;"string"===typeof b.salt&&(b.salt=sjcl.codec.base64.toBits(b.salt));"string"===typeof b.iv&&(b.iv=sjcl.codec.base64.toBits(b.iv));if(!sjcl.mode[b.mode]||!sjcl.cipher[b.cipher]||"string"===
typeof a&&100>=b.iter||64!==b.ts&&96!==b.ts&&128!==b.ts||128!==b.ks&&192!==b.ks&&0x100!==b.ks||!b.iv||2>b.iv.length||4<b.iv.length)throw new sjcl.exception.invalid("json decrypt: invalid parameters");"string"===typeof a?(g=sjcl.misc.cachedPbkdf2(a,b),a=g.key.slice(0,b.ks/32),b.salt=g.salt):sjcl.ecc&&a instanceof sjcl.ecc.elGamal.secretKey&&(a=a.unkem(sjcl.codec.base64.toBits(b.kemtag)).slice(0,b.ks/32));"string"===typeof f&&(f=sjcl.codec.utf8String.toBits(f));g=new sjcl.cipher[b.cipher](a);f="ccm"===
b.mode&&sjcl.arrayBuffer&&sjcl.arrayBuffer.ccm&&b.ct instanceof ArrayBuffer?sjcl.arrayBuffer.ccm.decrypt(g,b.ct,b.iv,b.tag,f,b.ts):sjcl.mode[b.mode].decrypt(g,b.ct,b.iv,f,b.ts);e.g(d,b);d.key=a;return 1===c.raw?f:sjcl.codec.utf8String.fromBits(f)},decrypt:function(a,b,c,d){var e=sjcl.json;return e.ia(a,e.decode(b),c,d)},encode:function(a){var b,c="{",d="";for(b in a)if(a.hasOwnProperty(b)){if(!b.match(/^[a-z0-9]+$/i))throw new sjcl.exception.invalid("json encode: invalid property name");c+=d+'"'+
b+'":';d=",";switch(typeof a[b]){case "number":case "boolean":c+=a[b];break;case "string":c+='"'+escape(a[b])+'"';break;case "object":c+='"'+sjcl.codec.base64.fromBits(a[b],0)+'"';break;default:throw new sjcl.exception.bug("json encode: unsupported type");}}return c+"}"},decode:function(a){a=a.replace(/\s/g,"");if(!a.match(/^\{.*\}$/))throw new sjcl.exception.invalid("json decode: this isn't json!");a=a.replace(/^\{|\}$/g,"").split(/,/);var b={},c,d;for(c=0;c<a.length;c++){if(!(d=a[c].match(/^\s*(?:(["']?)([a-z][a-z0-9]*)\1)\s*:\s*(?:(-?\d+)|"([a-z0-9+\/%*_.@=\-]*)"|(true|false))$/i)))throw new sjcl.exception.invalid("json decode: this isn't json!");
null!=d[3]?b[d[2]]=parseInt(d[3],10):null!=d[4]?b[d[2]]=d[2].match(/^(ct|adata|salt|iv)$/)?sjcl.codec.base64.toBits(d[4]):unescape(d[4]):null!=d[5]&&(b[d[2]]="true"===d[5])}return b},g:function(a,b,c){void 0===a&&(a={});if(void 0===b)return a;for(var d in b)if(b.hasOwnProperty(d)){if(c&&void 0!==a[d]&&a[d]!==b[d])throw new sjcl.exception.invalid("required parameter overridden");a[d]=b[d]}return a},sa:function(a,b){var c={},d;for(d in a)a.hasOwnProperty(d)&&a[d]!==b[d]&&(c[d]=a[d]);return c},ra:function(a,
b){var c={},d;for(d=0;d<b.length;d++)void 0!==a[b[d]]&&(c[b[d]]=a[b[d]]);return c}};sjcl.encrypt=sjcl.json.encrypt;sjcl.decrypt=sjcl.json.decrypt;sjcl.misc.pa={};sjcl.misc.cachedPbkdf2=function(a,b){var c=sjcl.misc.pa,d;b=b||{};d=b.iter||1E3;c=c[a]=c[a]||{};d=c[d]=c[d]||{firstSalt:b.salt&&b.salt.length?b.salt.slice(0):sjcl.random.randomWords(2,0)};c=void 0===b.salt?d.firstSalt:b.salt;d[c]=d[c]||sjcl.misc.pbkdf2(a,c,b.iter);return{key:d[c].slice(0),salt:c.slice(0)}};
"undefined"!==typeof module&&module.exports&&(module.exports=sjcl);"function"===typeof define&&define([],function(){return sjcl});

/** @fileOverview Javascript SHA-256 implementation.
    *
    * An older version of this implementation is available in the public
    * domain, but this one is (c) Emily Stark, Mike Hamburg, Dan Boneh,
    * Stanford University 2008-2010 and BSD-licensed for liability
    * reasons.
    *
    * Special thanks to Aldo Cortesi for pointing out several bugs in
    * this code.
    *
    * @author Emily Stark
    * @author Mike Hamburg
    * @author Dan Boneh
*/
/**
 * Context for a SHA-256 operation in progress.
 * @constructor
 */
sjcl.hash.sha256 = function (hash) {
    if (!this._key[0]) { this._precompute(); }
    if (hash) {
        this._h = hash._h.slice(0);
        this._buffer = hash._buffer.slice(0);
        this._length = hash._length;
    } else {
        this.reset();
    }
};
/**
 * Hash a string or an array of words.
 * @static
 * @param {bitArray|String} data the data to hash.
 * @return {bitArray} The hash value, an array of 16 big-endian words.
 */
sjcl.hash.sha256.hash = function (data) {
    return (new sjcl.hash.sha256()).update(data).finalize();
};
sjcl.hash.sha256.prototype = {
      /**
   * The hash's block size, in bits.
   * @constant
   */
    blockSize: 512,
      /**
   * Reset the hash state.
   * @return this
   */
    reset:function () {
        this._h = this._init.slice(0);
        this._buffer = [];
        this._length = 0;
        return this;
    },
      /**
   * Input several words to the hash.
   * @param {bitArray|String} data the data to hash.
   * @return this
   */
    update: function (data) {
        if (typeof data === "string") {
            data = sjcl.codec.utf8String.toBits(data);
        }
        var i, b = this._buffer = sjcl.bitArray.concat(this._buffer, data),
            ol = this._length,
            nl = this._length = ol + sjcl.bitArray.bitLength(data);
        if (nl > 9007199254740991){
            throw new sjcl.exception.invalid("Cannot hash more than 2^53 - 1 bits");
        }
        if (typeof Uint32Array !== 'undefined') {
            var c = new Uint32Array(b);
            var j = 0;
            for (i = 512+ol - ((512+ol) & 511); i <= nl; i+= 512) {
                this._block(c.subarray(16 * j, 16 * (j+1)));
                j += 1;
            }
            b.splice(0, 16 * j);
        } else {
            for (i = 512+ol - ((512+ol) & 511); i <= nl; i+= 512) {
                this._block(b.splice(0,16));
            }
        }
        return this;
    },
      /**
   * Complete hashing and output the hash value.
   * @return {bitArray} The hash value, an array of 8 big-endian words.
   */
    finalize:function () {
        var i, b = this._buffer, h = this._h;
        // Round out and push the buffer
        b = sjcl.bitArray.concat(b, [sjcl.bitArray.partial(1,1)]);
        // Round out the buffer to a multiple of 16 words, less the 2 length words.
        for (i = b.length + 2; i & 15; i++) {
            b.push(0);
        }
        // append the length
        b.push(Math.floor(this._length / 0x100000000));
        b.push(this._length | 0);
        while (b.length) {
            this._block(b.splice(0,16));
        }
        this.reset();
        return h;
    },
      /**
   * The SHA-256 initialization vector, to be precomputed.
   * @private
   */
    _init:[],
      /*
  _init:[0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19],
      */
      /**
   * The SHA-256 hash key, to be precomputed.
   * @private
   */
    _key:[],
      /*
  _key:
    [0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
     0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
     0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
     0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
     0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
     0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
     0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
     0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2],
      */
      /**
   * Function to precompute _init and _key.
   * @private
   */
    _precompute: function () {
        var i = 0, prime = 2, factor, isPrime;
        function frac(x) { return (x-Math.floor(x)) * 0x100000000 | 0; }
        for (; i<64; prime++) {
            isPrime = true;
            for (factor=2; factor*factor <= prime; factor++) {
                if (prime % factor === 0) {
                    isPrime = false;
                    break;
                }
            }
            if (isPrime) {
                if (i<8) {
                    this._init[i] = frac(Math.pow(prime, 1/2));
                }
                this._key[i] = frac(Math.pow(prime, 1/3));
                i++;
            }
        }
    },
      /**
   * Perform one cycle of SHA-256.
   * @param {Uint32Array|bitArray} w one block of words.
   * @private
   */
    _block:function (w) {
        var i, tmp, a, b,
            h = this._h,
            k = this._key,
            h0 = h[0], h1 = h[1], h2 = h[2], h3 = h[3],
            h4 = h[4], h5 = h[5], h6 = h[6], h7 = h[7];
            /* Rationale for placement of |0 :
     * If a value can overflow is original 32 bits by a factor of more than a few
     * million (2^23 ish), there is a possibility that it might overflow the
     * 53-bit mantissa and lose precision.
     *
     * To avoid this, we clamp back to 32 bits by |'ing with 0 on any value that
     * propagates around the loop, and on the hash state h[].  I don't believe
     * that the clamps on h4 and on h0 are strictly necessary, but it's close
     * (for h4 anyway), and better safe than sorry.
     *
     * The clamps on h[] are necessary for the output to be correct even in the
     * common case and for short inputs.
     */
        for (i=0; i<64; i++) {
            // load up the input word for this round
            if (i<16) {
                tmp = w[i];
            } else {
                a   = w[(i+1 ) & 15];
                b   = w[(i+14) & 15];
                tmp = w[i&15] = ((a>>>7  ^ a>>>18 ^ a>>>3  ^ a<<25 ^ a<<14) +
                                 (b>>>17 ^ b>>>19 ^ b>>>10 ^ b<<15 ^ b<<13) +
                                 w[i&15] + w[(i+9) & 15]) | 0;
            }
            tmp = (tmp + h7 + (h4>>>6 ^ h4>>>11 ^ h4>>>25 ^ h4<<26 ^ h4<<21 ^ h4<<7) +  (h6 ^ h4&(h5^h6)) + k[i]); // | 0;
            // shift register
            h7 = h6; h6 = h5; h5 = h4;
            h4 = h3 + tmp | 0;
            h3 = h2; h2 = h1; h1 = h0;
            h0 = (tmp +  ((h1&h2) ^ (h3&(h1^h2))) + (h1>>>2 ^ h1>>>13 ^ h1>>>22 ^ h1<<30 ^ h1<<19 ^ h1<<10)) | 0;
        }
        h[0] = h[0]+h0 | 0;
        h[1] = h[1]+h1 | 0;
        h[2] = h[2]+h2 | 0;
        h[3] = h[3]+h3 | 0;
        h[4] = h[4]+h4 | 0;
        h[5] = h[5]+h5 | 0;
        h[6] = h[6]+h6 | 0;
        h[7] = h[7]+h7 | 0;
    }
};

!function(a){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=a();else if("function"==typeof define&&define.amd)define([],a);else{var b;b="undefined"!=typeof window?window:"undefined"!=typeof global?global:"undefined"!=typeof self?self:this,b.elliptic=a()}}(function(){return function a(b,c,d){function e(g,h){if(!c[g]){if(!b[g]){var i="function"==typeof require&&require;if(!h&&i)return i(g,!0);if(f)return f(g,!0);var j=new Error("Cannot find module '"+g+"'");throw j.code="MODULE_NOT_FOUND",j}var k=c[g]={exports:{}};b[g][0].call(k.exports,function(a){var c=b[g][1][a];return e(c?c:a)},k,k.exports,a,b,c,d)}return c[g].exports}for(var f="function"==typeof require&&require,g=0;g<d.length;g++)e(d[g]);return e}({1:[function(a,b,c){"use strict";var d=c;d.version=a("../package.json").version,d.utils=a("./elliptic/utils"),d.rand=a("brorand"),d.curve=a("./elliptic/curve"),d.curves=a("./elliptic/curves"),d.ec=a("./elliptic/ec"),d.eddsa=a("./elliptic/eddsa")},{"../package.json":30,"./elliptic/curve":4,"./elliptic/curves":7,"./elliptic/ec":8,"./elliptic/eddsa":11,"./elliptic/utils":15,brorand:17}],2:[function(a,b,c){"use strict";function d(a,b){this.type=a,this.p=new f(b.p,16),this.red=b.prime?f.red(b.prime):f.mont(this.p),this.zero=new f(0).toRed(this.red),this.one=new f(1).toRed(this.red),this.two=new f(2).toRed(this.red),this.n=b.n&&new f(b.n,16),this.g=b.g&&this.pointFromJSON(b.g,b.gRed),this._wnafT1=new Array(4),this._wnafT2=new Array(4),this._wnafT3=new Array(4),this._wnafT4=new Array(4);var c=this.n&&this.p.div(this.n);!c||c.cmpn(100)>0?this.redN=null:(this._maxwellTrick=!0,this.redN=this.n.toRed(this.red))}function e(a,b){this.curve=a,this.type=b,this.precomputed=null}var f=a("bn.js"),g=a("../../elliptic"),h=g.utils,i=h.getNAF,j=h.getJSF,k=h.assert;b.exports=d,d.prototype.point=function(){throw new Error("Not implemented")},d.prototype.validate=function(){throw new Error("Not implemented")},d.prototype._fixedNafMul=function(a,b){k(a.precomputed);var c=a._getDoubles(),d=i(b,1),e=(1<<c.step+1)-(c.step%2===0?2:1);e/=3;for(var f=[],g=0;g<d.length;g+=c.step){for(var h=0,b=g+c.step-1;b>=g;b--)h=(h<<1)+d[b];f.push(h)}for(var j=this.jpoint(null,null,null),l=this.jpoint(null,null,null),m=e;m>0;m--){for(var g=0;g<f.length;g++){var h=f[g];h===m?l=l.mixedAdd(c.points[g]):h===-m&&(l=l.mixedAdd(c.points[g].neg()))}j=j.add(l)}return j.toP()},d.prototype._wnafMul=function(a,b){var c=4,d=a._getNAFPoints(c);c=d.wnd;for(var e=d.points,f=i(b,c),g=this.jpoint(null,null,null),h=f.length-1;h>=0;h--){for(var b=0;h>=0&&0===f[h];h--)b++;if(h>=0&&b++,g=g.dblp(b),h<0)break;var j=f[h];k(0!==j),g="affine"===a.type?j>0?g.mixedAdd(e[j-1>>1]):g.mixedAdd(e[-j-1>>1].neg()):j>0?g.add(e[j-1>>1]):g.add(e[-j-1>>1].neg())}return"affine"===a.type?g.toP():g},d.prototype._wnafMulAdd=function(a,b,c,d,e){for(var f=this._wnafT1,g=this._wnafT2,h=this._wnafT3,k=0,l=0;l<d;l++){var m=b[l],n=m._getNAFPoints(a);f[l]=n.wnd,g[l]=n.points}for(var l=d-1;l>=1;l-=2){var o=l-1,p=l;if(1===f[o]&&1===f[p]){var q=[b[o],null,null,b[p]];0===b[o].y.cmp(b[p].y)?(q[1]=b[o].add(b[p]),q[2]=b[o].toJ().mixedAdd(b[p].neg())):0===b[o].y.cmp(b[p].y.redNeg())?(q[1]=b[o].toJ().mixedAdd(b[p]),q[2]=b[o].add(b[p].neg())):(q[1]=b[o].toJ().mixedAdd(b[p]),q[2]=b[o].toJ().mixedAdd(b[p].neg()));var r=[-3,-1,-5,-7,0,7,5,1,3],s=j(c[o],c[p]);k=Math.max(s[0].length,k),h[o]=new Array(k),h[p]=new Array(k);for(var t=0;t<k;t++){var u=0|s[0][t],v=0|s[1][t];h[o][t]=r[3*(u+1)+(v+1)],h[p][t]=0,g[o]=q}}else h[o]=i(c[o],f[o]),h[p]=i(c[p],f[p]),k=Math.max(h[o].length,k),k=Math.max(h[p].length,k)}for(var w=this.jpoint(null,null,null),x=this._wnafT4,l=k;l>=0;l--){for(var y=0;l>=0;){for(var z=!0,t=0;t<d;t++)x[t]=0|h[t][l],0!==x[t]&&(z=!1);if(!z)break;y++,l--}if(l>=0&&y++,w=w.dblp(y),l<0)break;for(var t=0;t<d;t++){var m,A=x[t];0!==A&&(A>0?m=g[t][A-1>>1]:A<0&&(m=g[t][-A-1>>1].neg()),w="affine"===m.type?w.mixedAdd(m):w.add(m))}}for(var l=0;l<d;l++)g[l]=null;return e?w:w.toP()},d.BasePoint=e,e.prototype.eq=function(){throw new Error("Not implemented")},e.prototype.validate=function(){return this.curve.validate(this)},d.prototype.decodePoint=function(a,b){a=h.toArray(a,b);var c=this.p.byteLength();if((4===a[0]||6===a[0]||7===a[0])&&a.length-1===2*c){6===a[0]?k(a[a.length-1]%2===0):7===a[0]&&k(a[a.length-1]%2===1);var d=this.point(a.slice(1,1+c),a.slice(1+c,1+2*c));return d}if((2===a[0]||3===a[0])&&a.length-1===c)return this.pointFromX(a.slice(1,1+c),3===a[0]);throw new Error("Unknown point format")},e.prototype.encodeCompressed=function(a){return this.encode(a,!0)},e.prototype._encode=function(a){var b=this.curve.p.byteLength(),c=this.getX().toArray("be",b);return a?[this.getY().isEven()?2:3].concat(c):[4].concat(c,this.getY().toArray("be",b))},e.prototype.encode=function(a,b){return h.encode(this._encode(b),a)},e.prototype.precompute=function(a){if(this.precomputed)return this;var b={doubles:null,naf:null,beta:null};return b.naf=this._getNAFPoints(8),b.doubles=this._getDoubles(4,a),b.beta=this._getBeta(),this.precomputed=b,this},e.prototype._hasDoubles=function(a){if(!this.precomputed)return!1;var b=this.precomputed.doubles;return!!b&&b.points.length>=Math.ceil((a.bitLength()+1)/b.step)},e.prototype._getDoubles=function(a,b){if(this.precomputed&&this.precomputed.doubles)return this.precomputed.doubles;for(var c=[this],d=this,e=0;e<b;e+=a){for(var f=0;f<a;f++)d=d.dbl();c.push(d)}return{step:a,points:c}},e.prototype._getNAFPoints=function(a){if(this.precomputed&&this.precomputed.naf)return this.precomputed.naf;for(var b=[this],c=(1<<a)-1,d=1===c?null:this.dbl(),e=1;e<c;e++)b[e]=b[e-1].add(d);return{wnd:a,points:b}},e.prototype._getBeta=function(){return null},e.prototype.dblp=function(a){for(var b=this,c=0;c<a;c++)b=b.dbl();return b}},{"../../elliptic":1,"bn.js":16}],3:[function(a,b,c){"use strict";function d(a){this.twisted=1!==(0|a.a),this.mOneA=this.twisted&&(0|a.a)===-1,this.extended=this.mOneA,j.call(this,"edwards",a),this.a=new h(a.a,16).umod(this.red.m),this.a=this.a.toRed(this.red),this.c=new h(a.c,16).toRed(this.red),this.c2=this.c.redSqr(),this.d=new h(a.d,16).toRed(this.red),this.dd=this.d.redAdd(this.d),k(!this.twisted||0===this.c.fromRed().cmpn(1)),this.oneC=1===(0|a.c)}function e(a,b,c,d,e){j.BasePoint.call(this,a,"projective"),null===b&&null===c&&null===d?(this.x=this.curve.zero,this.y=this.curve.one,this.z=this.curve.one,this.t=this.curve.zero,this.zOne=!0):(this.x=new h(b,16),this.y=new h(c,16),this.z=d?new h(d,16):this.curve.one,this.t=e&&new h(e,16),this.x.red||(this.x=this.x.toRed(this.curve.red)),this.y.red||(this.y=this.y.toRed(this.curve.red)),this.z.red||(this.z=this.z.toRed(this.curve.red)),this.t&&!this.t.red&&(this.t=this.t.toRed(this.curve.red)),this.zOne=this.z===this.curve.one,this.curve.extended&&!this.t&&(this.t=this.x.redMul(this.y),this.zOne||(this.t=this.t.redMul(this.z.redInvm()))))}var f=a("../curve"),g=a("../../elliptic"),h=a("bn.js"),i=a("inherits"),j=f.base,k=g.utils.assert;i(d,j),b.exports=d,d.prototype._mulA=function(a){return this.mOneA?a.redNeg():this.a.redMul(a)},d.prototype._mulC=function(a){return this.oneC?a:this.c.redMul(a)},d.prototype.jpoint=function(a,b,c,d){return this.point(a,b,c,d)},d.prototype.pointFromX=function(a,b){a=new h(a,16),a.red||(a=a.toRed(this.red));var c=a.redSqr(),d=this.c2.redSub(this.a.redMul(c)),e=this.one.redSub(this.c2.redMul(this.d).redMul(c)),f=d.redMul(e.redInvm()),g=f.redSqrt();if(0!==g.redSqr().redSub(f).cmp(this.zero))throw new Error("invalid point");var i=g.fromRed().isOdd();return(b&&!i||!b&&i)&&(g=g.redNeg()),this.point(a,g)},d.prototype.pointFromY=function(a,b){a=new h(a,16),a.red||(a=a.toRed(this.red));var c=a.redSqr(),d=c.redSub(this.one),e=c.redMul(this.d).redAdd(this.one),f=d.redMul(e.redInvm());if(0===f.cmp(this.zero)){if(b)throw new Error("invalid point");return this.point(this.zero,a)}var g=f.redSqrt();if(0!==g.redSqr().redSub(f).cmp(this.zero))throw new Error("invalid point");return g.isOdd()!==b&&(g=g.redNeg()),this.point(g,a)},d.prototype.validate=function(a){if(a.isInfinity())return!0;a.normalize();var b=a.x.redSqr(),c=a.y.redSqr(),d=b.redMul(this.a).redAdd(c),e=this.c2.redMul(this.one.redAdd(this.d.redMul(b).redMul(c)));return 0===d.cmp(e)},i(e,j.BasePoint),d.prototype.pointFromJSON=function(a){return e.fromJSON(this,a)},d.prototype.point=function(a,b,c,d){return new e(this,a,b,c,d)},e.fromJSON=function(a,b){return new e(a,b[0],b[1],b[2])},e.prototype.inspect=function(){return this.isInfinity()?"<EC Point Infinity>":"<EC Point x: "+this.x.fromRed().toString(16,2)+" y: "+this.y.fromRed().toString(16,2)+" z: "+this.z.fromRed().toString(16,2)+">"},e.prototype.isInfinity=function(){return 0===this.x.cmpn(0)&&0===this.y.cmp(this.z)},e.prototype._extDbl=function(){var a=this.x.redSqr(),b=this.y.redSqr(),c=this.z.redSqr();c=c.redIAdd(c);var d=this.curve._mulA(a),e=this.x.redAdd(this.y).redSqr().redISub(a).redISub(b),f=d.redAdd(b),g=f.redSub(c),h=d.redSub(b),i=e.redMul(g),j=f.redMul(h),k=e.redMul(h),l=g.redMul(f);return this.curve.point(i,j,l,k)},e.prototype._projDbl=function(){var a,b,c,d=this.x.redAdd(this.y).redSqr(),e=this.x.redSqr(),f=this.y.redSqr();if(this.curve.twisted){var g=this.curve._mulA(e),h=g.redAdd(f);if(this.zOne)a=d.redSub(e).redSub(f).redMul(h.redSub(this.curve.two)),b=h.redMul(g.redSub(f)),c=h.redSqr().redSub(h).redSub(h);else{var i=this.z.redSqr(),j=h.redSub(i).redISub(i);a=d.redSub(e).redISub(f).redMul(j),b=h.redMul(g.redSub(f)),c=h.redMul(j)}}else{var g=e.redAdd(f),i=this.curve._mulC(this.c.redMul(this.z)).redSqr(),j=g.redSub(i).redSub(i);a=this.curve._mulC(d.redISub(g)).redMul(j),b=this.curve._mulC(g).redMul(e.redISub(f)),c=g.redMul(j)}return this.curve.point(a,b,c)},e.prototype.dbl=function(){return this.isInfinity()?this:this.curve.extended?this._extDbl():this._projDbl()},e.prototype._extAdd=function(a){var b=this.y.redSub(this.x).redMul(a.y.redSub(a.x)),c=this.y.redAdd(this.x).redMul(a.y.redAdd(a.x)),d=this.t.redMul(this.curve.dd).redMul(a.t),e=this.z.redMul(a.z.redAdd(a.z)),f=c.redSub(b),g=e.redSub(d),h=e.redAdd(d),i=c.redAdd(b),j=f.redMul(g),k=h.redMul(i),l=f.redMul(i),m=g.redMul(h);return this.curve.point(j,k,m,l)},e.prototype._projAdd=function(a){var b,c,d=this.z.redMul(a.z),e=d.redSqr(),f=this.x.redMul(a.x),g=this.y.redMul(a.y),h=this.curve.d.redMul(f).redMul(g),i=e.redSub(h),j=e.redAdd(h),k=this.x.redAdd(this.y).redMul(a.x.redAdd(a.y)).redISub(f).redISub(g),l=d.redMul(i).redMul(k);return this.curve.twisted?(b=d.redMul(j).redMul(g.redSub(this.curve._mulA(f))),c=i.redMul(j)):(b=d.redMul(j).redMul(g.redSub(f)),c=this.curve._mulC(i).redMul(j)),this.curve.point(l,b,c)},e.prototype.add=function(a){return this.isInfinity()?a:a.isInfinity()?this:this.curve.extended?this._extAdd(a):this._projAdd(a)},e.prototype.mul=function(a){return this._hasDoubles(a)?this.curve._fixedNafMul(this,a):this.curve._wnafMul(this,a)},e.prototype.mulAdd=function(a,b,c){return this.curve._wnafMulAdd(1,[this,b],[a,c],2,!1)},e.prototype.jmulAdd=function(a,b,c){return this.curve._wnafMulAdd(1,[this,b],[a,c],2,!0)},e.prototype.normalize=function(){if(this.zOne)return this;var a=this.z.redInvm();return this.x=this.x.redMul(a),this.y=this.y.redMul(a),this.t&&(this.t=this.t.redMul(a)),this.z=this.curve.one,this.zOne=!0,this},e.prototype.neg=function(){return this.curve.point(this.x.redNeg(),this.y,this.z,this.t&&this.t.redNeg())},e.prototype.getX=function(){return this.normalize(),this.x.fromRed()},e.prototype.getY=function(){return this.normalize(),this.y.fromRed()},e.prototype.eq=function(a){return this===a||0===this.getX().cmp(a.getX())&&0===this.getY().cmp(a.getY())},e.prototype.eqXToP=function(a){var b=a.toRed(this.curve.red).redMul(this.z);if(0===this.x.cmp(b))return!0;for(var c=a.clone(),d=this.curve.redN.redMul(this.z);;){if(c.iadd(this.curve.n),c.cmp(this.curve.p)>=0)return!1;if(b.redIAdd(d),0===this.x.cmp(b))return!0}return!1},e.prototype.toP=e.prototype.normalize,e.prototype.mixedAdd=e.prototype.add},{"../../elliptic":1,"../curve":4,"bn.js":16,inherits:27}],4:[function(a,b,c){"use strict";var d=c;d.base=a("./base"),d["short"]=a("./short"),d.mont=a("./mont"),d.edwards=a("./edwards")},{"./base":2,"./edwards":3,"./mont":5,"./short":6}],5:[function(a,b,c){"use strict";function d(a){i.call(this,"mont",a),this.a=new g(a.a,16).toRed(this.red),this.b=new g(a.b,16).toRed(this.red),this.i4=new g(4).toRed(this.red).redInvm(),this.two=new g(2).toRed(this.red),this.a24=this.i4.redMul(this.a.redAdd(this.two))}function e(a,b,c){i.BasePoint.call(this,a,"projective"),null===b&&null===c?(this.x=this.curve.one,this.z=this.curve.zero):(this.x=new g(b,16),this.z=new g(c,16),this.x.red||(this.x=this.x.toRed(this.curve.red)),this.z.red||(this.z=this.z.toRed(this.curve.red)))}var f=a("../curve"),g=a("bn.js"),h=a("inherits"),i=f.base,j=a("../../elliptic"),k=j.utils;h(d,i),b.exports=d,d.prototype.validate=function(a){var b=a.normalize().x,c=b.redSqr(),d=c.redMul(b).redAdd(c.redMul(this.a)).redAdd(b),e=d.redSqrt();return 0===e.redSqr().cmp(d)},h(e,i.BasePoint),d.prototype.decodePoint=function(a,b){return this.point(k.toArray(a,b),1)},d.prototype.point=function(a,b){return new e(this,a,b)},d.prototype.pointFromJSON=function(a){return e.fromJSON(this,a)},e.prototype.precompute=function(){},e.prototype._encode=function(){return this.getX().toArray("be",this.curve.p.byteLength())},e.fromJSON=function(a,b){return new e(a,b[0],b[1]||a.one)},e.prototype.inspect=function(){return this.isInfinity()?"<EC Point Infinity>":"<EC Point x: "+this.x.fromRed().toString(16,2)+" z: "+this.z.fromRed().toString(16,2)+">"},e.prototype.isInfinity=function(){return 0===this.z.cmpn(0)},e.prototype.dbl=function(){var a=this.x.redAdd(this.z),b=a.redSqr(),c=this.x.redSub(this.z),d=c.redSqr(),e=b.redSub(d),f=b.redMul(d),g=e.redMul(d.redAdd(this.curve.a24.redMul(e)));return this.curve.point(f,g)},e.prototype.add=function(){throw new Error("Not supported on Montgomery curve")},e.prototype.diffAdd=function(a,b){var c=this.x.redAdd(this.z),d=this.x.redSub(this.z),e=a.x.redAdd(a.z),f=a.x.redSub(a.z),g=f.redMul(c),h=e.redMul(d),i=b.z.redMul(g.redAdd(h).redSqr()),j=b.x.redMul(g.redISub(h).redSqr());return this.curve.point(i,j)},e.prototype.mul=function(a){for(var b=a.clone(),c=this,d=this.curve.point(null,null),e=this,f=[];0!==b.cmpn(0);b.iushrn(1))f.push(b.andln(1));for(var g=f.length-1;g>=0;g--)0===f[g]?(c=c.diffAdd(d,e),d=d.dbl()):(d=c.diffAdd(d,e),c=c.dbl());return d},e.prototype.mulAdd=function(){throw new Error("Not supported on Montgomery curve")},e.prototype.jumlAdd=function(){throw new Error("Not supported on Montgomery curve")},e.prototype.eq=function(a){return 0===this.getX().cmp(a.getX())},e.prototype.normalize=function(){return this.x=this.x.redMul(this.z.redInvm()),this.z=this.curve.one,this},e.prototype.getX=function(){return this.normalize(),this.x.fromRed()}},{"../../elliptic":1,"../curve":4,"bn.js":16,inherits:27}],6:[function(a,b,c){"use strict";function d(a){k.call(this,"short",a),this.a=new i(a.a,16).toRed(this.red),this.b=new i(a.b,16).toRed(this.red),this.tinv=this.two.redInvm(),this.zeroA=0===this.a.fromRed().cmpn(0),this.threeA=0===this.a.fromRed().sub(this.p).cmpn(-3),this.endo=this._getEndomorphism(a),this._endoWnafT1=new Array(4),this._endoWnafT2=new Array(4)}function e(a,b,c,d){k.BasePoint.call(this,a,"affine"),null===b&&null===c?(this.x=null,this.y=null,this.inf=!0):(this.x=new i(b,16),this.y=new i(c,16),d&&(this.x.forceRed(this.curve.red),this.y.forceRed(this.curve.red)),this.x.red||(this.x=this.x.toRed(this.curve.red)),this.y.red||(this.y=this.y.toRed(this.curve.red)),this.inf=!1)}function f(a,b,c,d){k.BasePoint.call(this,a,"jacobian"),null===b&&null===c&&null===d?(this.x=this.curve.one,this.y=this.curve.one,this.z=new i(0)):(this.x=new i(b,16),this.y=new i(c,16),this.z=new i(d,16)),this.x.red||(this.x=this.x.toRed(this.curve.red)),this.y.red||(this.y=this.y.toRed(this.curve.red)),this.z.red||(this.z=this.z.toRed(this.curve.red)),this.zOne=this.z===this.curve.one}var g=a("../curve"),h=a("../../elliptic"),i=a("bn.js"),j=a("inherits"),k=g.base,l=h.utils.assert;j(d,k),b.exports=d,d.prototype._getEndomorphism=function(a){if(this.zeroA&&this.g&&this.n&&1===this.p.modn(3)){var b,c;if(a.beta)b=new i(a.beta,16).toRed(this.red);else{var d=this._getEndoRoots(this.p);b=d[0].cmp(d[1])<0?d[0]:d[1],b=b.toRed(this.red)}if(a.lambda)c=new i(a.lambda,16);else{var e=this._getEndoRoots(this.n);0===this.g.mul(e[0]).x.cmp(this.g.x.redMul(b))?c=e[0]:(c=e[1],l(0===this.g.mul(c).x.cmp(this.g.x.redMul(b))))}var f;return f=a.basis?a.basis.map(function(a){return{a:new i(a.a,16),b:new i(a.b,16)}}):this._getEndoBasis(c),{beta:b,lambda:c,basis:f}}},d.prototype._getEndoRoots=function(a){var b=a===this.p?this.red:i.mont(a),c=new i(2).toRed(b).redInvm(),d=c.redNeg(),e=new i(3).toRed(b).redNeg().redSqrt().redMul(c),f=d.redAdd(e).fromRed(),g=d.redSub(e).fromRed();return[f,g]},d.prototype._getEndoBasis=function(a){for(var b,c,d,e,f,g,h,j,k,l=this.n.ushrn(Math.floor(this.n.bitLength()/2)),m=a,n=this.n.clone(),o=new i(1),p=new i(0),q=new i(0),r=new i(1),s=0;0!==m.cmpn(0);){var t=n.div(m);j=n.sub(t.mul(m)),k=q.sub(t.mul(o));var u=r.sub(t.mul(p));if(!d&&j.cmp(l)<0)b=h.neg(),c=o,d=j.neg(),e=k;else if(d&&2===++s)break;h=j,n=m,m=j,q=o,o=k,r=p,p=u}f=j.neg(),g=k;var v=d.sqr().add(e.sqr()),w=f.sqr().add(g.sqr());return w.cmp(v)>=0&&(f=b,g=c),d.negative&&(d=d.neg(),e=e.neg()),f.negative&&(f=f.neg(),g=g.neg()),[{a:d,b:e},{a:f,b:g}]},d.prototype._endoSplit=function(a){var b=this.endo.basis,c=b[0],d=b[1],e=d.b.mul(a).divRound(this.n),f=c.b.neg().mul(a).divRound(this.n),g=e.mul(c.a),h=f.mul(d.a),i=e.mul(c.b),j=f.mul(d.b),k=a.sub(g).sub(h),l=i.add(j).neg();return{k1:k,k2:l}},d.prototype.pointFromX=function(a,b){a=new i(a,16),a.red||(a=a.toRed(this.red));var c=a.redSqr().redMul(a).redIAdd(a.redMul(this.a)).redIAdd(this.b),d=c.redSqrt();if(0!==d.redSqr().redSub(c).cmp(this.zero))throw new Error("invalid point");var e=d.fromRed().isOdd();return(b&&!e||!b&&e)&&(d=d.redNeg()),this.point(a,d)},d.prototype.validate=function(a){if(a.inf)return!0;var b=a.x,c=a.y,d=this.a.redMul(b),e=b.redSqr().redMul(b).redIAdd(d).redIAdd(this.b);return 0===c.redSqr().redISub(e).cmpn(0)},d.prototype._endoWnafMulAdd=function(a,b,c){for(var d=this._endoWnafT1,e=this._endoWnafT2,f=0;f<a.length;f++){var g=this._endoSplit(b[f]),h=a[f],i=h._getBeta();g.k1.negative&&(g.k1.ineg(),h=h.neg(!0)),g.k2.negative&&(g.k2.ineg(),i=i.neg(!0)),d[2*f]=h,d[2*f+1]=i,e[2*f]=g.k1,e[2*f+1]=g.k2}for(var j=this._wnafMulAdd(1,d,e,2*f,c),k=0;k<2*f;k++)d[k]=null,e[k]=null;return j},j(e,k.BasePoint),d.prototype.point=function(a,b,c){return new e(this,a,b,c)},d.prototype.pointFromJSON=function(a,b){return e.fromJSON(this,a,b)},e.prototype._getBeta=function(){if(this.curve.endo){var a=this.precomputed;if(a&&a.beta)return a.beta;var b=this.curve.point(this.x.redMul(this.curve.endo.beta),this.y);if(a){var c=this.curve,d=function(a){return c.point(a.x.redMul(c.endo.beta),a.y)};a.beta=b,b.precomputed={beta:null,naf:a.naf&&{wnd:a.naf.wnd,points:a.naf.points.map(d)},doubles:a.doubles&&{step:a.doubles.step,points:a.doubles.points.map(d)}}}return b}},e.prototype.toJSON=function(){return this.precomputed?[this.x,this.y,this.precomputed&&{doubles:this.precomputed.doubles&&{step:this.precomputed.doubles.step,points:this.precomputed.doubles.points.slice(1)},naf:this.precomputed.naf&&{wnd:this.precomputed.naf.wnd,points:this.precomputed.naf.points.slice(1)}}]:[this.x,this.y]},e.fromJSON=function(a,b,c){function d(b){return a.point(b[0],b[1],c)}"string"==typeof b&&(b=JSON.parse(b));var e=a.point(b[0],b[1],c);if(!b[2])return e;var f=b[2];return e.precomputed={beta:null,doubles:f.doubles&&{step:f.doubles.step,points:[e].concat(f.doubles.points.map(d))},naf:f.naf&&{wnd:f.naf.wnd,points:[e].concat(f.naf.points.map(d))}},e},e.prototype.inspect=function(){return this.isInfinity()?"<EC Point Infinity>":"<EC Point x: "+this.x.fromRed().toString(16,2)+" y: "+this.y.fromRed().toString(16,2)+">"},e.prototype.isInfinity=function(){return this.inf},e.prototype.add=function(a){if(this.inf)return a;if(a.inf)return this;if(this.eq(a))return this.dbl();if(this.neg().eq(a))return this.curve.point(null,null);if(0===this.x.cmp(a.x))return this.curve.point(null,null);var b=this.y.redSub(a.y);0!==b.cmpn(0)&&(b=b.redMul(this.x.redSub(a.x).redInvm()));var c=b.redSqr().redISub(this.x).redISub(a.x),d=b.redMul(this.x.redSub(c)).redISub(this.y);return this.curve.point(c,d)},e.prototype.dbl=function(){if(this.inf)return this;var a=this.y.redAdd(this.y);if(0===a.cmpn(0))return this.curve.point(null,null);var b=this.curve.a,c=this.x.redSqr(),d=a.redInvm(),e=c.redAdd(c).redIAdd(c).redIAdd(b).redMul(d),f=e.redSqr().redISub(this.x.redAdd(this.x)),g=e.redMul(this.x.redSub(f)).redISub(this.y);return this.curve.point(f,g)},e.prototype.getX=function(){return this.x.fromRed()},e.prototype.getY=function(){return this.y.fromRed()},e.prototype.mul=function(a){return a=new i(a,16),this._hasDoubles(a)?this.curve._fixedNafMul(this,a):this.curve.endo?this.curve._endoWnafMulAdd([this],[a]):this.curve._wnafMul(this,a)},e.prototype.mulAdd=function(a,b,c){var d=[this,b],e=[a,c];return this.curve.endo?this.curve._endoWnafMulAdd(d,e):this.curve._wnafMulAdd(1,d,e,2)},e.prototype.jmulAdd=function(a,b,c){var d=[this,b],e=[a,c];return this.curve.endo?this.curve._endoWnafMulAdd(d,e,!0):this.curve._wnafMulAdd(1,d,e,2,!0)},e.prototype.eq=function(a){return this===a||this.inf===a.inf&&(this.inf||0===this.x.cmp(a.x)&&0===this.y.cmp(a.y))},e.prototype.neg=function(a){if(this.inf)return this;var b=this.curve.point(this.x,this.y.redNeg());if(a&&this.precomputed){var c=this.precomputed,d=function(a){return a.neg()};b.precomputed={naf:c.naf&&{wnd:c.naf.wnd,points:c.naf.points.map(d)},doubles:c.doubles&&{step:c.doubles.step,points:c.doubles.points.map(d)}}}return b},e.prototype.toJ=function(){if(this.inf)return this.curve.jpoint(null,null,null);var a=this.curve.jpoint(this.x,this.y,this.curve.one);return a},j(f,k.BasePoint),d.prototype.jpoint=function(a,b,c){return new f(this,a,b,c)},f.prototype.toP=function(){if(this.isInfinity())return this.curve.point(null,null);var a=this.z.redInvm(),b=a.redSqr(),c=this.x.redMul(b),d=this.y.redMul(b).redMul(a);return this.curve.point(c,d)},f.prototype.neg=function(){return this.curve.jpoint(this.x,this.y.redNeg(),this.z)},f.prototype.add=function(a){if(this.isInfinity())return a;if(a.isInfinity())return this;var b=a.z.redSqr(),c=this.z.redSqr(),d=this.x.redMul(b),e=a.x.redMul(c),f=this.y.redMul(b.redMul(a.z)),g=a.y.redMul(c.redMul(this.z)),h=d.redSub(e),i=f.redSub(g);if(0===h.cmpn(0))return 0!==i.cmpn(0)?this.curve.jpoint(null,null,null):this.dbl();var j=h.redSqr(),k=j.redMul(h),l=d.redMul(j),m=i.redSqr().redIAdd(k).redISub(l).redISub(l),n=i.redMul(l.redISub(m)).redISub(f.redMul(k)),o=this.z.redMul(a.z).redMul(h);return this.curve.jpoint(m,n,o)},f.prototype.mixedAdd=function(a){if(this.isInfinity())return a.toJ();if(a.isInfinity())return this;var b=this.z.redSqr(),c=this.x,d=a.x.redMul(b),e=this.y,f=a.y.redMul(b).redMul(this.z),g=c.redSub(d),h=e.redSub(f);if(0===g.cmpn(0))return 0!==h.cmpn(0)?this.curve.jpoint(null,null,null):this.dbl();var i=g.redSqr(),j=i.redMul(g),k=c.redMul(i),l=h.redSqr().redIAdd(j).redISub(k).redISub(k),m=h.redMul(k.redISub(l)).redISub(e.redMul(j)),n=this.z.redMul(g);return this.curve.jpoint(l,m,n)},f.prototype.dblp=function(a){if(0===a)return this;if(this.isInfinity())return this;if(!a)return this.dbl();if(this.curve.zeroA||this.curve.threeA){for(var b=this,c=0;c<a;c++)b=b.dbl();return b}for(var d=this.curve.a,e=this.curve.tinv,f=this.x,g=this.y,h=this.z,i=h.redSqr().redSqr(),j=g.redAdd(g),c=0;c<a;c++){var k=f.redSqr(),l=j.redSqr(),m=l.redSqr(),n=k.redAdd(k).redIAdd(k).redIAdd(d.redMul(i)),o=f.redMul(l),p=n.redSqr().redISub(o.redAdd(o)),q=o.redISub(p),r=n.redMul(q);r=r.redIAdd(r).redISub(m);var s=j.redMul(h);c+1<a&&(i=i.redMul(m)),f=p,h=s,j=r}return this.curve.jpoint(f,j.redMul(e),h)},f.prototype.dbl=function(){return this.isInfinity()?this:this.curve.zeroA?this._zeroDbl():this.curve.threeA?this._threeDbl():this._dbl()},f.prototype._zeroDbl=function(){var a,b,c;if(this.zOne){var d=this.x.redSqr(),e=this.y.redSqr(),f=e.redSqr(),g=this.x.redAdd(e).redSqr().redISub(d).redISub(f);g=g.redIAdd(g);var h=d.redAdd(d).redIAdd(d),i=h.redSqr().redISub(g).redISub(g),j=f.redIAdd(f);j=j.redIAdd(j),j=j.redIAdd(j),a=i,b=h.redMul(g.redISub(i)).redISub(j),c=this.y.redAdd(this.y)}else{var k=this.x.redSqr(),l=this.y.redSqr(),m=l.redSqr(),n=this.x.redAdd(l).redSqr().redISub(k).redISub(m);n=n.redIAdd(n);var o=k.redAdd(k).redIAdd(k),p=o.redSqr(),q=m.redIAdd(m);q=q.redIAdd(q),q=q.redIAdd(q),a=p.redISub(n).redISub(n),b=o.redMul(n.redISub(a)).redISub(q),c=this.y.redMul(this.z),c=c.redIAdd(c)}return this.curve.jpoint(a,b,c)},f.prototype._threeDbl=function(){var a,b,c;if(this.zOne){var d=this.x.redSqr(),e=this.y.redSqr(),f=e.redSqr(),g=this.x.redAdd(e).redSqr().redISub(d).redISub(f);g=g.redIAdd(g);var h=d.redAdd(d).redIAdd(d).redIAdd(this.curve.a),i=h.redSqr().redISub(g).redISub(g);a=i;var j=f.redIAdd(f);j=j.redIAdd(j),j=j.redIAdd(j),b=h.redMul(g.redISub(i)).redISub(j),c=this.y.redAdd(this.y)}else{var k=this.z.redSqr(),l=this.y.redSqr(),m=this.x.redMul(l),n=this.x.redSub(k).redMul(this.x.redAdd(k));n=n.redAdd(n).redIAdd(n);var o=m.redIAdd(m);o=o.redIAdd(o);var p=o.redAdd(o);a=n.redSqr().redISub(p),c=this.y.redAdd(this.z).redSqr().redISub(l).redISub(k);var q=l.redSqr();q=q.redIAdd(q),q=q.redIAdd(q),q=q.redIAdd(q),b=n.redMul(o.redISub(a)).redISub(q)}return this.curve.jpoint(a,b,c)},f.prototype._dbl=function(){var a=this.curve.a,b=this.x,c=this.y,d=this.z,e=d.redSqr().redSqr(),f=b.redSqr(),g=c.redSqr(),h=f.redAdd(f).redIAdd(f).redIAdd(a.redMul(e)),i=b.redAdd(b);i=i.redIAdd(i);var j=i.redMul(g),k=h.redSqr().redISub(j.redAdd(j)),l=j.redISub(k),m=g.redSqr();m=m.redIAdd(m),m=m.redIAdd(m),m=m.redIAdd(m);var n=h.redMul(l).redISub(m),o=c.redAdd(c).redMul(d);return this.curve.jpoint(k,n,o)},f.prototype.trpl=function(){if(!this.curve.zeroA)return this.dbl().add(this);var a=this.x.redSqr(),b=this.y.redSqr(),c=this.z.redSqr(),d=b.redSqr(),e=a.redAdd(a).redIAdd(a),f=e.redSqr(),g=this.x.redAdd(b).redSqr().redISub(a).redISub(d);g=g.redIAdd(g),g=g.redAdd(g).redIAdd(g),g=g.redISub(f);var h=g.redSqr(),i=d.redIAdd(d);i=i.redIAdd(i),i=i.redIAdd(i),i=i.redIAdd(i);var j=e.redIAdd(g).redSqr().redISub(f).redISub(h).redISub(i),k=b.redMul(j);k=k.redIAdd(k),k=k.redIAdd(k);var l=this.x.redMul(h).redISub(k);l=l.redIAdd(l),l=l.redIAdd(l);var m=this.y.redMul(j.redMul(i.redISub(j)).redISub(g.redMul(h)));m=m.redIAdd(m),m=m.redIAdd(m),m=m.redIAdd(m);var n=this.z.redAdd(g).redSqr().redISub(c).redISub(h);return this.curve.jpoint(l,m,n)},f.prototype.mul=function(a,b){return a=new i(a,b),this.curve._wnafMul(this,a)},f.prototype.eq=function(a){if("affine"===a.type)return this.eq(a.toJ());if(this===a)return!0;var b=this.z.redSqr(),c=a.z.redSqr();if(0!==this.x.redMul(c).redISub(a.x.redMul(b)).cmpn(0))return!1;var d=b.redMul(this.z),e=c.redMul(a.z);return 0===this.y.redMul(e).redISub(a.y.redMul(d)).cmpn(0)},f.prototype.eqXToP=function(a){var b=this.z.redSqr(),c=a.toRed(this.curve.red).redMul(b);if(0===this.x.cmp(c))return!0;for(var d=a.clone(),e=this.curve.redN.redMul(b);;){if(d.iadd(this.curve.n),d.cmp(this.curve.p)>=0)return!1;if(c.redIAdd(e),0===this.x.cmp(c))return!0}return!1},f.prototype.inspect=function(){return this.isInfinity()?"<EC JPoint Infinity>":"<EC JPoint x: "+this.x.toString(16,2)+" y: "+this.y.toString(16,2)+" z: "+this.z.toString(16,2)+">"},f.prototype.isInfinity=function(){return 0===this.z.cmpn(0)}},{"../../elliptic":1,"../curve":4,"bn.js":16,inherits:27}],7:[function(a,b,c){"use strict";function d(a){"short"===a.type?this.curve=new h.curve["short"](a):"edwards"===a.type?this.curve=new h.curve.edwards(a):this.curve=new h.curve.mont(a),this.g=this.curve.g,this.n=this.curve.n,this.hash=a.hash,i(this.g.validate(),"Invalid curve"),i(this.g.mul(this.n).isInfinity(),"Invalid curve, G*N != O")}function e(a,b){Object.defineProperty(f,a,{configurable:!0,enumerable:!0,get:function(){var c=new d(b);return Object.defineProperty(f,a,{configurable:!0,enumerable:!0,value:c}),c}})}var f=c,g=a("hash.js"),h=a("../elliptic"),i=h.utils.assert;f.PresetCurve=d,e("p192",{type:"short",prime:"p192",p:"ffffffff ffffffff ffffffff fffffffe ffffffff ffffffff",a:"ffffffff ffffffff ffffffff fffffffe ffffffff fffffffc",b:"64210519 e59c80e7 0fa7e9ab 72243049 feb8deec c146b9b1",n:"ffffffff ffffffff ffffffff 99def836 146bc9b1 b4d22831",hash:g.sha256,gRed:!1,g:["188da80e b03090f6 7cbf20eb 43a18800 f4ff0afd 82ff1012","07192b95 ffc8da78 631011ed 6b24cdd5 73f977a1 1e794811"]}),e("p224",{type:"short",prime:"p224",p:"ffffffff ffffffff ffffffff ffffffff 00000000 00000000 00000001",a:"ffffffff ffffffff ffffffff fffffffe ffffffff ffffffff fffffffe",b:"b4050a85 0c04b3ab f5413256 5044b0b7 d7bfd8ba 270b3943 2355ffb4",n:"ffffffff ffffffff ffffffff ffff16a2 e0b8f03e 13dd2945 5c5c2a3d",hash:g.sha256,gRed:!1,g:["b70e0cbd 6bb4bf7f 321390b9 4a03c1d3 56c21122 343280d6 115c1d21","bd376388 b5f723fb 4c22dfe6 cd4375a0 5a074764 44d58199 85007e34"]}),e("p256",{type:"short",prime:null,p:"ffffffff 00000001 00000000 00000000 00000000 ffffffff ffffffff ffffffff",a:"ffffffff 00000001 00000000 00000000 00000000 ffffffff ffffffff fffffffc",b:"5ac635d8 aa3a93e7 b3ebbd55 769886bc 651d06b0 cc53b0f6 3bce3c3e 27d2604b",n:"ffffffff 00000000 ffffffff ffffffff bce6faad a7179e84 f3b9cac2 fc632551",hash:g.sha256,gRed:!1,g:["6b17d1f2 e12c4247 f8bce6e5 63a440f2 77037d81 2deb33a0 f4a13945 d898c296","4fe342e2 fe1a7f9b 8ee7eb4a 7c0f9e16 2bce3357 6b315ece cbb64068 37bf51f5"]}),e("p384",{type:"short",prime:null,p:"ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffffffe ffffffff 00000000 00000000 ffffffff",a:"ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffffffe ffffffff 00000000 00000000 fffffffc",b:"b3312fa7 e23ee7e4 988e056b e3f82d19 181d9c6e fe814112 0314088f 5013875a c656398d 8a2ed19d 2a85c8ed d3ec2aef",n:"ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff c7634d81 f4372ddf 581a0db2 48b0a77a ecec196a ccc52973",hash:g.sha384,gRed:!1,g:["aa87ca22 be8b0537 8eb1c71e f320ad74 6e1d3b62 8ba79b98 59f741e0 82542a38 5502f25d bf55296c 3a545e38 72760ab7","3617de4a 96262c6f 5d9e98bf 9292dc29 f8f41dbd 289a147c e9da3113 b5f0b8c0 0a60b1ce 1d7e819d 7a431d7c 90ea0e5f"]}),e("p521",{type:"short",prime:null,p:"000001ff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff",a:"000001ff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffffffc",b:"00000051 953eb961 8e1c9a1f 929a21a0 b68540ee a2da725b 99b315f3 b8b48991 8ef109e1 56193951 ec7e937b 1652c0bd 3bb1bf07 3573df88 3d2c34f1 ef451fd4 6b503f00",n:"000001ff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffffffa 51868783 bf2f966b 7fcc0148 f709a5d0 3bb5c9b8 899c47ae bb6fb71e 91386409",hash:g.sha512,gRed:!1,g:["000000c6 858e06b7 0404e9cd 9e3ecb66 2395b442 9c648139 053fb521 f828af60 6b4d3dba a14b5e77 efe75928 fe1dc127 a2ffa8de 3348b3c1 856a429b f97e7e31 c2e5bd66","00000118 39296a78 9a3bc004 5c8a5fb4 2c7d1bd9 98f54449 579b4468 17afbd17 273e662c 97ee7299 5ef42640 c550b901 3fad0761 353c7086 a272c240 88be9476 9fd16650"]}),e("curve25519",{type:"mont",prime:"p25519",p:"7fffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffed",a:"76d06",b:"1",n:"1000000000000000 0000000000000000 14def9dea2f79cd6 5812631a5cf5d3ed",hash:g.sha256,gRed:!1,g:["9"]}),e("ed25519",{type:"edwards",prime:"p25519",p:"7fffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffed",a:"-1",c:"1",d:"52036cee2b6ffe73 8cc740797779e898 00700a4d4141d8ab 75eb4dca135978a3",n:"1000000000000000 0000000000000000 14def9dea2f79cd6 5812631a5cf5d3ed",hash:g.sha256,gRed:!1,g:["216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a","6666666666666666666666666666666666666666666666666666666666666658"]});var j;try{j=a("./precomputed/secp256k1")}catch(k){
j=void 0}e("secp256k1",{type:"short",prime:"k256",p:"ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffffffe fffffc2f",a:"0",b:"7",n:"ffffffff ffffffff ffffffff fffffffe baaedce6 af48a03b bfd25e8c d0364141",h:"1",hash:g.sha256,beta:"7ae96a2b657c07106e64479eac3434e99cf0497512f58995c1396c28719501ee",lambda:"5363ad4cc05c30e0a5261c028812645a122e22ea20816678df02967c1b23bd72",basis:[{a:"3086d221a7d46bcde86c90e49284eb15",b:"-e4437ed6010e88286f547fa90abfe4c3"},{a:"114ca50f7a8e2f3f657c1108d9d44cfd8",b:"3086d221a7d46bcde86c90e49284eb15"}],gRed:!1,g:["79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798","483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8",j]})},{"../elliptic":1,"./precomputed/secp256k1":14,"hash.js":19}],8:[function(a,b,c){"use strict";function d(a){return this instanceof d?("string"==typeof a&&(i(g.curves.hasOwnProperty(a),"Unknown curve "+a),a=g.curves[a]),a instanceof g.curves.PresetCurve&&(a={curve:a}),this.curve=a.curve.curve,this.n=this.curve.n,this.nh=this.n.ushrn(1),this.g=this.curve.g,this.g=a.curve.g,this.g.precompute(a.curve.n.bitLength()+1),void(this.hash=a.hash||a.curve.hash)):new d(a)}var e=a("bn.js"),f=a("hmac-drbg"),g=a("../../elliptic"),h=g.utils,i=h.assert,j=a("./key"),k=a("./signature");b.exports=d,d.prototype.keyPair=function(a){return new j(this,a)},d.prototype.keyFromPrivate=function(a,b){return j.fromPrivate(this,a,b)},d.prototype.keyFromPublic=function(a,b){return j.fromPublic(this,a,b)},d.prototype.genKeyPair=function(a){a||(a={});for(var b=new f({hash:this.hash,pers:a.pers,persEnc:a.persEnc||"utf8",entropy:a.entropy||g.rand(this.hash.hmacStrength),entropyEnc:a.entropy&&a.entropyEnc||"utf8",nonce:this.n.toArray()}),c=this.n.byteLength(),d=this.n.sub(new e(2));;){var h=new e(b.generate(c));if(!(h.cmp(d)>0))return h.iaddn(1),this.keyFromPrivate(h)}},d.prototype._truncateToN=function(a,b){var c=8*a.byteLength()-this.n.bitLength();return c>0&&(a=a.ushrn(c)),!b&&a.cmp(this.n)>=0?a.sub(this.n):a},d.prototype.sign=function(a,b,c,d){"object"==typeof c&&(d=c,c=null),d||(d={}),b=this.keyFromPrivate(b,c),a=this._truncateToN(new e(a,16));for(var g=this.n.byteLength(),h=b.getPrivate().toArray("be",g),i=a.toArray("be",g),j=new f({hash:this.hash,entropy:h,nonce:i,pers:d.pers,persEnc:d.persEnc||"utf8"}),l=this.n.sub(new e(1)),m=0;!0;m++){var n=d.k?d.k(m):new e(j.generate(this.n.byteLength()));if(n=this._truncateToN(n,!0),!(n.cmpn(1)<=0||n.cmp(l)>=0)){var o=this.g.mul(n);if(!o.isInfinity()){var p=o.getX(),q=p.umod(this.n);if(0!==q.cmpn(0)){var r=n.invm(this.n).mul(q.mul(b.getPrivate()).iadd(a));if(r=r.umod(this.n),0!==r.cmpn(0)){var s=(o.getY().isOdd()?1:0)|(0!==p.cmp(q)?2:0);return d.canonical&&r.cmp(this.nh)>0&&(r=this.n.sub(r),s^=1),new k({r:q,s:r,recoveryParam:s})}}}}}},d.prototype.verify=function(a,b,c,d){a=this._truncateToN(new e(a,16)),c=this.keyFromPublic(c,d),b=new k(b,"hex");var f=b.r,g=b.s;if(f.cmpn(1)<0||f.cmp(this.n)>=0)return!1;if(g.cmpn(1)<0||g.cmp(this.n)>=0)return!1;var h=g.invm(this.n),i=h.mul(a).umod(this.n),j=h.mul(f).umod(this.n);if(!this.curve._maxwellTrick){var l=this.g.mulAdd(i,c.getPublic(),j);return!l.isInfinity()&&0===l.getX().umod(this.n).cmp(f)}var l=this.g.jmulAdd(i,c.getPublic(),j);return!l.isInfinity()&&l.eqXToP(f)},d.prototype.recoverPubKey=function(a,b,c,d){i((3&c)===c,"The recovery param is more than two bits"),b=new k(b,d);var f=this.n,g=new e(a),h=b.r,j=b.s,l=1&c,m=c>>1;if(h.cmp(this.curve.p.umod(this.curve.n))>=0&&m)throw new Error("Unable to find sencond key candinate");h=m?this.curve.pointFromX(h.add(this.curve.n),l):this.curve.pointFromX(h,l);var n=b.r.invm(f),o=f.sub(g).mul(n).umod(f),p=j.mul(n).umod(f);return this.g.mulAdd(o,h,p)},d.prototype.getKeyRecoveryParam=function(a,b,c,d){if(b=new k(b,d),null!==b.recoveryParam)return b.recoveryParam;for(var e=0;e<4;e++){var f;try{f=this.recoverPubKey(a,b,e)}catch(a){continue}if(f.eq(c))return e}throw new Error("Unable to find valid recovery factor")}},{"../../elliptic":1,"./key":9,"./signature":10,"bn.js":16,"hmac-drbg":25}],9:[function(a,b,c){"use strict";function d(a,b){this.ec=a,this.priv=null,this.pub=null,b.priv&&this._importPrivate(b.priv,b.privEnc),b.pub&&this._importPublic(b.pub,b.pubEnc)}var e=a("bn.js"),f=a("../../elliptic"),g=f.utils,h=g.assert;b.exports=d,d.fromPublic=function(a,b,c){return b instanceof d?b:new d(a,{pub:b,pubEnc:c})},d.fromPrivate=function(a,b,c){return b instanceof d?b:new d(a,{priv:b,privEnc:c})},d.prototype.validate=function(){var a=this.getPublic();return a.isInfinity()?{result:!1,reason:"Invalid public key"}:a.validate()?a.mul(this.ec.curve.n).isInfinity()?{result:!0,reason:null}:{result:!1,reason:"Public key * N != O"}:{result:!1,reason:"Public key is not a point"}},d.prototype.getPublic=function(a,b){return"string"==typeof a&&(b=a,a=null),this.pub||(this.pub=this.ec.g.mul(this.priv)),b?this.pub.encode(b,a):this.pub},d.prototype.getPrivate=function(a){return"hex"===a?this.priv.toString(16,2):this.priv},d.prototype._importPrivate=function(a,b){this.priv=new e(a,b||16),this.priv=this.priv.umod(this.ec.curve.n)},d.prototype._importPublic=function(a,b){return a.x||a.y?("mont"===this.ec.curve.type?h(a.x,"Need x coordinate"):"short"!==this.ec.curve.type&&"edwards"!==this.ec.curve.type||h(a.x&&a.y,"Need both x and y coordinate"),void(this.pub=this.ec.curve.point(a.x,a.y))):void(this.pub=this.ec.curve.decodePoint(a,b))},d.prototype.derive=function(a){return a.mul(this.priv).getX()},d.prototype.sign=function(a,b,c){return this.ec.sign(a,this,b,c)},d.prototype.verify=function(a,b){return this.ec.verify(a,b,this)},d.prototype.inspect=function(){return"<Key priv: "+(this.priv&&this.priv.toString(16,2))+" pub: "+(this.pub&&this.pub.inspect())+" >"}},{"../../elliptic":1,"bn.js":16}],10:[function(a,b,c){"use strict";function d(a,b){return a instanceof d?a:void(this._importDER(a,b)||(l(a.r&&a.s,"Signature without r or s"),this.r=new i(a.r,16),this.s=new i(a.s,16),void 0===a.recoveryParam?this.recoveryParam=null:this.recoveryParam=a.recoveryParam))}function e(){this.place=0}function f(a,b){var c=a[b.place++];if(!(128&c))return c;for(var d=15&c,e=0,f=0,g=b.place;f<d;f++,g++)e<<=8,e|=a[g];return b.place=g,e}function g(a){for(var b=0,c=a.length-1;!a[b]&&!(128&a[b+1])&&b<c;)b++;return 0===b?a:a.slice(b)}function h(a,b){if(b<128)return void a.push(b);var c=1+(Math.log(b)/Math.LN2>>>3);for(a.push(128|c);--c;)a.push(b>>>(c<<3)&255);a.push(b)}var i=a("bn.js"),j=a("../../elliptic"),k=j.utils,l=k.assert;b.exports=d,d.prototype._importDER=function(a,b){a=k.toArray(a,b);var c=new e;if(48!==a[c.place++])return!1;var d=f(a,c);if(d+c.place!==a.length)return!1;if(2!==a[c.place++])return!1;var g=f(a,c),h=a.slice(c.place,g+c.place);if(c.place+=g,2!==a[c.place++])return!1;var j=f(a,c);if(a.length!==j+c.place)return!1;var l=a.slice(c.place,j+c.place);return 0===h[0]&&128&h[1]&&(h=h.slice(1)),0===l[0]&&128&l[1]&&(l=l.slice(1)),this.r=new i(h),this.s=new i(l),this.recoveryParam=null,!0},d.prototype.toDER=function(a){var b=this.r.toArray(),c=this.s.toArray();for(128&b[0]&&(b=[0].concat(b)),128&c[0]&&(c=[0].concat(c)),b=g(b),c=g(c);!(c[0]||128&c[1]);)c=c.slice(1);var d=[2];h(d,b.length),d=d.concat(b),d.push(2),h(d,c.length);var e=d.concat(c),f=[48];return h(f,e.length),f=f.concat(e),k.encode(f,a)}},{"../../elliptic":1,"bn.js":16}],11:[function(a,b,c){"use strict";function d(a){if(h("ed25519"===a,"only tested with ed25519 so far"),!(this instanceof d))return new d(a);var a=f.curves[a].curve;this.curve=a,this.g=a.g,this.g.precompute(a.n.bitLength()+1),this.pointClass=a.point().constructor,this.encodingLength=Math.ceil(a.n.bitLength()/8),this.hash=e.sha512}var e=a("hash.js"),f=a("../../elliptic"),g=f.utils,h=g.assert,i=g.parseBytes,j=a("./key"),k=a("./signature");b.exports=d,d.prototype.sign=function(a,b){a=i(a);var c=this.keyFromSecret(b),d=this.hashInt(c.messagePrefix(),a),e=this.g.mul(d),f=this.encodePoint(e),g=this.hashInt(f,c.pubBytes(),a).mul(c.priv()),h=d.add(g).umod(this.curve.n);return this.makeSignature({R:e,S:h,Rencoded:f})},d.prototype.verify=function(a,b,c){a=i(a),b=this.makeSignature(b);var d=this.keyFromPublic(c),e=this.hashInt(b.Rencoded(),d.pubBytes(),a),f=this.g.mul(b.S()),g=b.R().add(d.pub().mul(e));return g.eq(f)},d.prototype.hashInt=function(){for(var a=this.hash(),b=0;b<arguments.length;b++)a.update(arguments[b]);return g.intFromLE(a.digest()).umod(this.curve.n)},d.prototype.keyFromPublic=function(a){return j.fromPublic(this,a)},d.prototype.keyFromSecret=function(a){return j.fromSecret(this,a)},d.prototype.makeSignature=function(a){return a instanceof k?a:new k(this,a)},d.prototype.encodePoint=function(a){var b=a.getY().toArray("le",this.encodingLength);return b[this.encodingLength-1]|=a.getX().isOdd()?128:0,b},d.prototype.decodePoint=function(a){a=g.parseBytes(a);var b=a.length-1,c=a.slice(0,b).concat(a[b]&-129),d=0!==(128&a[b]),e=g.intFromLE(c);return this.curve.pointFromY(e,d)},d.prototype.encodeInt=function(a){return a.toArray("le",this.encodingLength)},d.prototype.decodeInt=function(a){return g.intFromLE(a)},d.prototype.isPoint=function(a){return a instanceof this.pointClass}},{"../../elliptic":1,"./key":12,"./signature":13,"hash.js":19}],12:[function(a,b,c){"use strict";function d(a,b){this.eddsa=a,this._secret=h(b.secret),a.isPoint(b.pub)?this._pub=b.pub:this._pubBytes=h(b.pub)}var e=a("../../elliptic"),f=e.utils,g=f.assert,h=f.parseBytes,i=f.cachedProperty;d.fromPublic=function(a,b){return b instanceof d?b:new d(a,{pub:b})},d.fromSecret=function(a,b){return b instanceof d?b:new d(a,{secret:b})},d.prototype.secret=function(){return this._secret},i(d,"pubBytes",function(){return this.eddsa.encodePoint(this.pub())}),i(d,"pub",function(){return this._pubBytes?this.eddsa.decodePoint(this._pubBytes):this.eddsa.g.mul(this.priv())}),i(d,"privBytes",function(){var a=this.eddsa,b=this.hash(),c=a.encodingLength-1,d=b.slice(0,a.encodingLength);return d[0]&=248,d[c]&=127,d[c]|=64,d}),i(d,"priv",function(){return this.eddsa.decodeInt(this.privBytes())}),i(d,"hash",function(){return this.eddsa.hash().update(this.secret()).digest()}),i(d,"messagePrefix",function(){return this.hash().slice(this.eddsa.encodingLength)}),d.prototype.sign=function(a){return g(this._secret,"KeyPair can only verify"),this.eddsa.sign(a,this)},d.prototype.verify=function(a,b){return this.eddsa.verify(a,b,this)},d.prototype.getSecret=function(a){return g(this._secret,"KeyPair is public only"),f.encode(this.secret(),a)},d.prototype.getPublic=function(a){return f.encode(this.pubBytes(),a)},b.exports=d},{"../../elliptic":1}],13:[function(a,b,c){"use strict";function d(a,b){this.eddsa=a,"object"!=typeof b&&(b=j(b)),Array.isArray(b)&&(b={R:b.slice(0,a.encodingLength),S:b.slice(a.encodingLength)}),h(b.R&&b.S,"Signature without R or S"),a.isPoint(b.R)&&(this._R=b.R),b.S instanceof e&&(this._S=b.S),this._Rencoded=Array.isArray(b.R)?b.R:b.Rencoded,this._Sencoded=Array.isArray(b.S)?b.S:b.Sencoded}var e=a("bn.js"),f=a("../../elliptic"),g=f.utils,h=g.assert,i=g.cachedProperty,j=g.parseBytes;i(d,"S",function(){return this.eddsa.decodeInt(this.Sencoded())}),i(d,"R",function(){return this.eddsa.decodePoint(this.Rencoded())}),i(d,"Rencoded",function(){return this.eddsa.encodePoint(this.R())}),i(d,"Sencoded",function(){return this.eddsa.encodeInt(this.S())}),d.prototype.toBytes=function(){return this.Rencoded().concat(this.Sencoded())},d.prototype.toHex=function(){return g.encode(this.toBytes(),"hex").toUpperCase()},b.exports=d},{"../../elliptic":1,"bn.js":16}],14:[function(a,b,c){b.exports={doubles:{step:4,points:[["e60fce93b59e9ec53011aabc21c23e97b2a31369b87a5ae9c44ee89e2a6dec0a","f7e3507399e595929db99f34f57937101296891e44d23f0be1f32cce69616821"],["8282263212c609d9ea2a6e3e172de238d8c39cabd5ac1ca10646e23fd5f51508","11f8a8098557dfe45e8256e830b60ace62d613ac2f7b17bed31b6eaff6e26caf"],["175e159f728b865a72f99cc6c6fc846de0b93833fd2222ed73fce5b551e5b739","d3506e0d9e3c79eba4ef97a51ff71f5eacb5955add24345c6efa6ffee9fed695"],["363d90d447b00c9c99ceac05b6262ee053441c7e55552ffe526bad8f83ff4640","4e273adfc732221953b445397f3363145b9a89008199ecb62003c7f3bee9de9"],["8b4b5f165df3c2be8c6244b5b745638843e4a781a15bcd1b69f79a55dffdf80c","4aad0a6f68d308b4b3fbd7813ab0da04f9e336546162ee56b3eff0c65fd4fd36"],["723cbaa6e5db996d6bf771c00bd548c7b700dbffa6c0e77bcb6115925232fcda","96e867b5595cc498a921137488824d6e2660a0653779494801dc069d9eb39f5f"],["eebfa4d493bebf98ba5feec812c2d3b50947961237a919839a533eca0e7dd7fa","5d9a8ca3970ef0f269ee7edaf178089d9ae4cdc3a711f712ddfd4fdae1de8999"],["100f44da696e71672791d0a09b7bde459f1215a29b3c03bfefd7835b39a48db0","cdd9e13192a00b772ec8f3300c090666b7ff4a18ff5195ac0fbd5cd62bc65a09"],["e1031be262c7ed1b1dc9227a4a04c017a77f8d4464f3b3852c8acde6e534fd2d","9d7061928940405e6bb6a4176597535af292dd419e1ced79a44f18f29456a00d"],["feea6cae46d55b530ac2839f143bd7ec5cf8b266a41d6af52d5e688d9094696d","e57c6b6c97dce1bab06e4e12bf3ecd5c981c8957cc41442d3155debf18090088"],["da67a91d91049cdcb367be4be6ffca3cfeed657d808583de33fa978bc1ec6cb1","9bacaa35481642bc41f463f7ec9780e5dec7adc508f740a17e9ea8e27a68be1d"],["53904faa0b334cdda6e000935ef22151ec08d0f7bb11069f57545ccc1a37b7c0","5bc087d0bc80106d88c9eccac20d3c1c13999981e14434699dcb096b022771c8"],["8e7bcd0bd35983a7719cca7764ca906779b53a043a9b8bcaeff959f43ad86047","10b7770b2a3da4b3940310420ca9514579e88e2e47fd68b3ea10047e8460372a"],["385eed34c1cdff21e6d0818689b81bde71a7f4f18397e6690a841e1599c43862","283bebc3e8ea23f56701de19e9ebf4576b304eec2086dc8cc0458fe5542e5453"],["6f9d9b803ecf191637c73a4413dfa180fddf84a5947fbc9c606ed86c3fac3a7","7c80c68e603059ba69b8e2a30e45c4d47ea4dd2f5c281002d86890603a842160"],["3322d401243c4e2582a2147c104d6ecbf774d163db0f5e5313b7e0e742d0e6bd","56e70797e9664ef5bfb019bc4ddaf9b72805f63ea2873af624f3a2e96c28b2a0"],["85672c7d2de0b7da2bd1770d89665868741b3f9af7643397721d74d28134ab83","7c481b9b5b43b2eb6374049bfa62c2e5e77f17fcc5298f44c8e3094f790313a6"],["948bf809b1988a46b06c9f1919413b10f9226c60f668832ffd959af60c82a0a","53a562856dcb6646dc6b74c5d1c3418c6d4dff08c97cd2bed4cb7f88d8c8e589"],["6260ce7f461801c34f067ce0f02873a8f1b0e44dfc69752accecd819f38fd8e8","bc2da82b6fa5b571a7f09049776a1ef7ecd292238051c198c1a84e95b2b4ae17"],["e5037de0afc1d8d43d8348414bbf4103043ec8f575bfdc432953cc8d2037fa2d","4571534baa94d3b5f9f98d09fb990bddbd5f5b03ec481f10e0e5dc841d755bda"],["e06372b0f4a207adf5ea905e8f1771b4e7e8dbd1c6a6c5b725866a0ae4fce725","7a908974bce18cfe12a27bb2ad5a488cd7484a7787104870b27034f94eee31dd"],["213c7a715cd5d45358d0bbf9dc0ce02204b10bdde2a3f58540ad6908d0559754","4b6dad0b5ae462507013ad06245ba190bb4850f5f36a7eeddff2c27534b458f2"],["4e7c272a7af4b34e8dbb9352a5419a87e2838c70adc62cddf0cc3a3b08fbd53c","17749c766c9d0b18e16fd09f6def681b530b9614bff7dd33e0b3941817dcaae6"],["fea74e3dbe778b1b10f238ad61686aa5c76e3db2be43057632427e2840fb27b6","6e0568db9b0b13297cf674deccb6af93126b596b973f7b77701d3db7f23cb96f"],["76e64113f677cf0e10a2570d599968d31544e179b760432952c02a4417bdde39","c90ddf8dee4e95cf577066d70681f0d35e2a33d2b56d2032b4b1752d1901ac01"],["c738c56b03b2abe1e8281baa743f8f9a8f7cc643df26cbee3ab150242bcbb891","893fb578951ad2537f718f2eacbfbbbb82314eef7880cfe917e735d9699a84c3"],["d895626548b65b81e264c7637c972877d1d72e5f3a925014372e9f6588f6c14b","febfaa38f2bc7eae728ec60818c340eb03428d632bb067e179363ed75d7d991f"],["b8da94032a957518eb0f6433571e8761ceffc73693e84edd49150a564f676e03","2804dfa44805a1e4d7c99cc9762808b092cc584d95ff3b511488e4e74efdf6e7"],["e80fea14441fb33a7d8adab9475d7fab2019effb5156a792f1a11778e3c0df5d","eed1de7f638e00771e89768ca3ca94472d155e80af322ea9fcb4291b6ac9ec78"],["a301697bdfcd704313ba48e51d567543f2a182031efd6915ddc07bbcc4e16070","7370f91cfb67e4f5081809fa25d40f9b1735dbf7c0a11a130c0d1a041e177ea1"],["90ad85b389d6b936463f9d0512678de208cc330b11307fffab7ac63e3fb04ed4","e507a3620a38261affdcbd9427222b839aefabe1582894d991d4d48cb6ef150"],["8f68b9d2f63b5f339239c1ad981f162ee88c5678723ea3351b7b444c9ec4c0da","662a9f2dba063986de1d90c2b6be215dbbea2cfe95510bfdf23cbf79501fff82"],["e4f3fb0176af85d65ff99ff9198c36091f48e86503681e3e6686fd5053231e11","1e63633ad0ef4f1c1661a6d0ea02b7286cc7e74ec951d1c9822c38576feb73bc"],["8c00fa9b18ebf331eb961537a45a4266c7034f2f0d4e1d0716fb6eae20eae29e","efa47267fea521a1a9dc343a3736c974c2fadafa81e36c54e7d2a4c66702414b"],["e7a26ce69dd4829f3e10cec0a9e98ed3143d084f308b92c0997fddfc60cb3e41","2a758e300fa7984b471b006a1aafbb18d0a6b2c0420e83e20e8a9421cf2cfd51"],["b6459e0ee3662ec8d23540c223bcbdc571cbcb967d79424f3cf29eb3de6b80ef","67c876d06f3e06de1dadf16e5661db3c4b3ae6d48e35b2ff30bf0b61a71ba45"],["d68a80c8280bb840793234aa118f06231d6f1fc67e73c5a5deda0f5b496943e8","db8ba9fff4b586d00c4b1f9177b0e28b5b0e7b8f7845295a294c84266b133120"],["324aed7df65c804252dc0270907a30b09612aeb973449cea4095980fc28d3d5d","648a365774b61f2ff130c0c35aec1f4f19213b0c7e332843967224af96ab7c84"],["4df9c14919cde61f6d51dfdbe5fee5dceec4143ba8d1ca888e8bd373fd054c96","35ec51092d8728050974c23a1d85d4b5d506cdc288490192ebac06cad10d5d"],["9c3919a84a474870faed8a9c1cc66021523489054d7f0308cbfc99c8ac1f98cd","ddb84f0f4a4ddd57584f044bf260e641905326f76c64c8e6be7e5e03d4fc599d"],["6057170b1dd12fdf8de05f281d8e06bb91e1493a8b91d4cc5a21382120a959e5","9a1af0b26a6a4807add9a2daf71df262465152bc3ee24c65e899be932385a2a8"],["a576df8e23a08411421439a4518da31880cef0fba7d4df12b1a6973eecb94266","40a6bf20e76640b2c92b97afe58cd82c432e10a7f514d9f3ee8be11ae1b28ec8"],["7778a78c28dec3e30a05fe9629de8c38bb30d1f5cf9a3a208f763889be58ad71","34626d9ab5a5b22ff7098e12f2ff580087b38411ff24ac563b513fc1fd9f43ac"],["928955ee637a84463729fd30e7afd2ed5f96274e5ad7e5cb09eda9c06d903ac","c25621003d3f42a827b78a13093a95eeac3d26efa8a8d83fc5180e935bcd091f"],["85d0fef3ec6db109399064f3a0e3b2855645b4a907ad354527aae75163d82751","1f03648413a38c0be29d496e582cf5663e8751e96877331582c237a24eb1f962"],["ff2b0dce97eece97c1c9b6041798b85dfdfb6d8882da20308f5404824526087e","493d13fef524ba188af4c4dc54d07936c7b7ed6fb90e2ceb2c951e01f0c29907"],["827fbbe4b1e880ea9ed2b2e6301b212b57f1ee148cd6dd28780e5e2cf856e241","c60f9c923c727b0b71bef2c67d1d12687ff7a63186903166d605b68baec293ec"],["eaa649f21f51bdbae7be4ae34ce6e5217a58fdce7f47f9aa7f3b58fa2120e2b3","be3279ed5bbbb03ac69a80f89879aa5a01a6b965f13f7e59d47a5305ba5ad93d"],["e4a42d43c5cf169d9391df6decf42ee541b6d8f0c9a137401e23632dda34d24f","4d9f92e716d1c73526fc99ccfb8ad34ce886eedfa8d8e4f13a7f7131deba9414"],["1ec80fef360cbdd954160fadab352b6b92b53576a88fea4947173b9d4300bf19","aeefe93756b5340d2f3a4958a7abbf5e0146e77f6295a07b671cdc1cc107cefd"],["146a778c04670c2f91b00af4680dfa8bce3490717d58ba889ddb5928366642be","b318e0ec3354028add669827f9d4b2870aaa971d2f7e5ed1d0b297483d83efd0"],["fa50c0f61d22e5f07e3acebb1aa07b128d0012209a28b9776d76a8793180eef9","6b84c6922397eba9b72cd2872281a68a5e683293a57a213b38cd8d7d3f4f2811"],["da1d61d0ca721a11b1a5bf6b7d88e8421a288ab5d5bba5220e53d32b5f067ec2","8157f55a7c99306c79c0766161c91e2966a73899d279b48a655fba0f1ad836f1"],["a8e282ff0c9706907215ff98e8fd416615311de0446f1e062a73b0610d064e13","7f97355b8db81c09abfb7f3c5b2515888b679a3e50dd6bd6cef7c73111f4cc0c"],["174a53b9c9a285872d39e56e6913cab15d59b1fa512508c022f382de8319497c","ccc9dc37abfc9c1657b4155f2c47f9e6646b3a1d8cb9854383da13ac079afa73"],["959396981943785c3d3e57edf5018cdbe039e730e4918b3d884fdff09475b7ba","2e7e552888c331dd8ba0386a4b9cd6849c653f64c8709385e9b8abf87524f2fd"],["d2a63a50ae401e56d645a1153b109a8fcca0a43d561fba2dbb51340c9d82b151","e82d86fb6443fcb7565aee58b2948220a70f750af484ca52d4142174dcf89405"],["64587e2335471eb890ee7896d7cfdc866bacbdbd3839317b3436f9b45617e073","d99fcdd5bf6902e2ae96dd6447c299a185b90a39133aeab358299e5e9faf6589"],["8481bde0e4e4d885b3a546d3e549de042f0aa6cea250e7fd358d6c86dd45e458","38ee7b8cba5404dd84a25bf39cecb2ca900a79c42b262e556d64b1b59779057e"],["13464a57a78102aa62b6979ae817f4637ffcfed3c4b1ce30bcd6303f6caf666b","69be159004614580ef7e433453ccb0ca48f300a81d0942e13f495a907f6ecc27"],["bc4a9df5b713fe2e9aef430bcc1dc97a0cd9ccede2f28588cada3a0d2d83f366","d3a81ca6e785c06383937adf4b798caa6e8a9fbfa547b16d758d666581f33c1"],["8c28a97bf8298bc0d23d8c749452a32e694b65e30a9472a3954ab30fe5324caa","40a30463a3305193378fedf31f7cc0eb7ae784f0451cb9459e71dc73cbef9482"],["8ea9666139527a8c1dd94ce4f071fd23c8b350c5a4bb33748c4ba111faccae0","620efabbc8ee2782e24e7c0cfb95c5d735b783be9cf0f8e955af34a30e62b945"],["dd3625faef5ba06074669716bbd3788d89bdde815959968092f76cc4eb9a9787","7a188fa3520e30d461da2501045731ca941461982883395937f68d00c644a573"],["f710d79d9eb962297e4f6232b40e8f7feb2bc63814614d692c12de752408221e","ea98e67232d3b3295d3b535532115ccac8612c721851617526ae47a9c77bfc82"]]},naf:{wnd:7,points:[["f9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9","388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672"],["2f8bde4d1a07209355b4a7250a5c5128e88b84bddc619ab7cba8d569b240efe4","d8ac222636e5e3d6d4dba9dda6c9c426f788271bab0d6840dca87d3aa6ac62d6"],["5cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc","6aebca40ba255960a3178d6d861a54dba813d0b813fde7b5a5082628087264da"],["acd484e2f0c7f65309ad178a9f559abde09796974c57e714c35f110dfc27ccbe","cc338921b0a7d9fd64380971763b61e9add888a4375f8e0f05cc262ac64f9c37"],["774ae7f858a9411e5ef4246b70c65aac5649980be5c17891bbec17895da008cb","d984a032eb6b5e190243dd56d7b7b365372db1e2dff9d6a8301d74c9c953c61b"],["f28773c2d975288bc7d1d205c3748651b075fbc6610e58cddeeddf8f19405aa8","ab0902e8d880a89758212eb65cdaf473a1a06da521fa91f29b5cb52db03ed81"],["d7924d4f7d43ea965a465ae3095ff41131e5946f3c85f79e44adbcf8e27e080e","581e2872a86c72a683842ec228cc6defea40af2bd896d3a5c504dc9ff6a26b58"],["defdea4cdb677750a420fee807eacf21eb9898ae79b9768766e4faa04a2d4a34","4211ab0694635168e997b0ead2a93daeced1f4a04a95c0f6cfb199f69e56eb77"],["2b4ea0a797a443d293ef5cff444f4979f06acfebd7e86d277475656138385b6c","85e89bc037945d93b343083b5a1c86131a01f60c50269763b570c854e5c09b7a"],["352bbf4a4cdd12564f93fa332ce333301d9ad40271f8107181340aef25be59d5","321eb4075348f534d59c18259dda3e1f4a1b3b2e71b1039c67bd3d8bcf81998c"],["2fa2104d6b38d11b0230010559879124e42ab8dfeff5ff29dc9cdadd4ecacc3f","2de1068295dd865b64569335bd5dd80181d70ecfc882648423ba76b532b7d67"],["9248279b09b4d68dab21a9b066edda83263c3d84e09572e269ca0cd7f5453714","73016f7bf234aade5d1aa71bdea2b1ff3fc0de2a887912ffe54a32ce97cb3402"],["daed4f2be3a8bf278e70132fb0beb7522f570e144bf615c07e996d443dee8729","a69dce4a7d6c98e8d4a1aca87ef8d7003f83c230f3afa726ab40e52290be1c55"],["c44d12c7065d812e8acf28d7cbb19f9011ecd9e9fdf281b0e6a3b5e87d22e7db","2119a460ce326cdc76c45926c982fdac0e106e861edf61c5a039063f0e0e6482"],["6a245bf6dc698504c89a20cfded60853152b695336c28063b61c65cbd269e6b4","e022cf42c2bd4a708b3f5126f16a24ad8b33ba48d0423b6efd5e6348100d8a82"],["1697ffa6fd9de627c077e3d2fe541084ce13300b0bec1146f95ae57f0d0bd6a5","b9c398f186806f5d27561506e4557433a2cf15009e498ae7adee9d63d01b2396"],["605bdb019981718b986d0f07e834cb0d9deb8360ffb7f61df982345ef27a7479","2972d2de4f8d20681a78d93ec96fe23c26bfae84fb14db43b01e1e9056b8c49"],["62d14dab4150bf497402fdc45a215e10dcb01c354959b10cfe31c7e9d87ff33d","80fc06bd8cc5b01098088a1950eed0db01aa132967ab472235f5642483b25eaf"],["80c60ad0040f27dade5b4b06c408e56b2c50e9f56b9b8b425e555c2f86308b6f","1c38303f1cc5c30f26e66bad7fe72f70a65eed4cbe7024eb1aa01f56430bd57a"],["7a9375ad6167ad54aa74c6348cc54d344cc5dc9487d847049d5eabb0fa03c8fb","d0e3fa9eca8726909559e0d79269046bdc59ea10c70ce2b02d499ec224dc7f7"],["d528ecd9b696b54c907a9ed045447a79bb408ec39b68df504bb51f459bc3ffc9","eecf41253136e5f99966f21881fd656ebc4345405c520dbc063465b521409933"],["49370a4b5f43412ea25f514e8ecdad05266115e4a7ecb1387231808f8b45963","758f3f41afd6ed428b3081b0512fd62a54c3f3afbb5b6764b653052a12949c9a"],["77f230936ee88cbbd73df930d64702ef881d811e0e1498e2f1c13eb1fc345d74","958ef42a7886b6400a08266e9ba1b37896c95330d97077cbbe8eb3c7671c60d6"],["f2dac991cc4ce4b9ea44887e5c7c0bce58c80074ab9d4dbaeb28531b7739f530","e0dedc9b3b2f8dad4da1f32dec2531df9eb5fbeb0598e4fd1a117dba703a3c37"],["463b3d9f662621fb1b4be8fbbe2520125a216cdfc9dae3debcba4850c690d45b","5ed430d78c296c3543114306dd8622d7c622e27c970a1de31cb377b01af7307e"],["f16f804244e46e2a09232d4aff3b59976b98fac14328a2d1a32496b49998f247","cedabd9b82203f7e13d206fcdf4e33d92a6c53c26e5cce26d6579962c4e31df6"],["caf754272dc84563b0352b7a14311af55d245315ace27c65369e15f7151d41d1","cb474660ef35f5f2a41b643fa5e460575f4fa9b7962232a5c32f908318a04476"],["2600ca4b282cb986f85d0f1709979d8b44a09c07cb86d7c124497bc86f082120","4119b88753c15bd6a693b03fcddbb45d5ac6be74ab5f0ef44b0be9475a7e4b40"],["7635ca72d7e8432c338ec53cd12220bc01c48685e24f7dc8c602a7746998e435","91b649609489d613d1d5e590f78e6d74ecfc061d57048bad9e76f302c5b9c61"],["754e3239f325570cdbbf4a87deee8a66b7f2b33479d468fbc1a50743bf56cc18","673fb86e5bda30fb3cd0ed304ea49a023ee33d0197a695d0c5d98093c536683"],["e3e6bd1071a1e96aff57859c82d570f0330800661d1c952f9fe2694691d9b9e8","59c9e0bba394e76f40c0aa58379a3cb6a5a2283993e90c4167002af4920e37f5"],["186b483d056a033826ae73d88f732985c4ccb1f32ba35f4b4cc47fdcf04aa6eb","3b952d32c67cf77e2e17446e204180ab21fb8090895138b4a4a797f86e80888b"],["df9d70a6b9876ce544c98561f4be4f725442e6d2b737d9c91a8321724ce0963f","55eb2dafd84d6ccd5f862b785dc39d4ab157222720ef9da217b8c45cf2ba2417"],["5edd5cc23c51e87a497ca815d5dce0f8ab52554f849ed8995de64c5f34ce7143","efae9c8dbc14130661e8cec030c89ad0c13c66c0d17a2905cdc706ab7399a868"],["290798c2b6476830da12fe02287e9e777aa3fba1c355b17a722d362f84614fba","e38da76dcd440621988d00bcf79af25d5b29c094db2a23146d003afd41943e7a"],["af3c423a95d9f5b3054754efa150ac39cd29552fe360257362dfdecef4053b45","f98a3fd831eb2b749a93b0e6f35cfb40c8cd5aa667a15581bc2feded498fd9c6"],["766dbb24d134e745cccaa28c99bf274906bb66b26dcf98df8d2fed50d884249a","744b1152eacbe5e38dcc887980da38b897584a65fa06cedd2c924f97cbac5996"],["59dbf46f8c94759ba21277c33784f41645f7b44f6c596a58ce92e666191abe3e","c534ad44175fbc300f4ea6ce648309a042ce739a7919798cd85e216c4a307f6e"],["f13ada95103c4537305e691e74e9a4a8dd647e711a95e73cb62dc6018cfd87b8","e13817b44ee14de663bf4bc808341f326949e21a6a75c2570778419bdaf5733d"],["7754b4fa0e8aced06d4167a2c59cca4cda1869c06ebadfb6488550015a88522c","30e93e864e669d82224b967c3020b8fa8d1e4e350b6cbcc537a48b57841163a2"],["948dcadf5990e048aa3874d46abef9d701858f95de8041d2a6828c99e2262519","e491a42537f6e597d5d28a3224b1bc25df9154efbd2ef1d2cbba2cae5347d57e"],["7962414450c76c1689c7b48f8202ec37fb224cf5ac0bfa1570328a8a3d7c77ab","100b610ec4ffb4760d5c1fc133ef6f6b12507a051f04ac5760afa5b29db83437"],["3514087834964b54b15b160644d915485a16977225b8847bb0dd085137ec47ca","ef0afbb2056205448e1652c48e8127fc6039e77c15c2378b7e7d15a0de293311"],["d3cc30ad6b483e4bc79ce2c9dd8bc54993e947eb8df787b442943d3f7b527eaf","8b378a22d827278d89c5e9be8f9508ae3c2ad46290358630afb34db04eede0a4"],["1624d84780732860ce1c78fcbfefe08b2b29823db913f6493975ba0ff4847610","68651cf9b6da903e0914448c6cd9d4ca896878f5282be4c8cc06e2a404078575"],["733ce80da955a8a26902c95633e62a985192474b5af207da6df7b4fd5fc61cd4","f5435a2bd2badf7d485a4d8b8db9fcce3e1ef8e0201e4578c54673bc1dc5ea1d"],["15d9441254945064cf1a1c33bbd3b49f8966c5092171e699ef258dfab81c045c","d56eb30b69463e7234f5137b73b84177434800bacebfc685fc37bbe9efe4070d"],["a1d0fcf2ec9de675b612136e5ce70d271c21417c9d2b8aaaac138599d0717940","edd77f50bcb5a3cab2e90737309667f2641462a54070f3d519212d39c197a629"],["e22fbe15c0af8ccc5780c0735f84dbe9a790badee8245c06c7ca37331cb36980","a855babad5cd60c88b430a69f53a1a7a38289154964799be43d06d77d31da06"],["311091dd9860e8e20ee13473c1155f5f69635e394704eaa74009452246cfa9b3","66db656f87d1f04fffd1f04788c06830871ec5a64feee685bd80f0b1286d8374"],["34c1fd04d301be89b31c0442d3e6ac24883928b45a9340781867d4232ec2dbdf","9414685e97b1b5954bd46f730174136d57f1ceeb487443dc5321857ba73abee"],["f219ea5d6b54701c1c14de5b557eb42a8d13f3abbcd08affcc2a5e6b049b8d63","4cb95957e83d40b0f73af4544cccf6b1f4b08d3c07b27fb8d8c2962a400766d1"],["d7b8740f74a8fbaab1f683db8f45de26543a5490bca627087236912469a0b448","fa77968128d9c92ee1010f337ad4717eff15db5ed3c049b3411e0315eaa4593b"],["32d31c222f8f6f0ef86f7c98d3a3335ead5bcd32abdd94289fe4d3091aa824bf","5f3032f5892156e39ccd3d7915b9e1da2e6dac9e6f26e961118d14b8462e1661"],["7461f371914ab32671045a155d9831ea8793d77cd59592c4340f86cbc18347b5","8ec0ba238b96bec0cbdddcae0aa442542eee1ff50c986ea6b39847b3cc092ff6"],["ee079adb1df1860074356a25aa38206a6d716b2c3e67453d287698bad7b2b2d6","8dc2412aafe3be5c4c5f37e0ecc5f9f6a446989af04c4e25ebaac479ec1c8c1e"],["16ec93e447ec83f0467b18302ee620f7e65de331874c9dc72bfd8616ba9da6b5","5e4631150e62fb40d0e8c2a7ca5804a39d58186a50e497139626778e25b0674d"],["eaa5f980c245f6f038978290afa70b6bd8855897f98b6aa485b96065d537bd99","f65f5d3e292c2e0819a528391c994624d784869d7e6ea67fb18041024edc07dc"],["78c9407544ac132692ee1910a02439958ae04877151342ea96c4b6b35a49f51","f3e0319169eb9b85d5404795539a5e68fa1fbd583c064d2462b675f194a3ddb4"],["494f4be219a1a77016dcd838431aea0001cdc8ae7a6fc688726578d9702857a5","42242a969283a5f339ba7f075e36ba2af925ce30d767ed6e55f4b031880d562c"],["a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5","204b5d6f84822c307e4b4a7140737aec23fc63b65b35f86a10026dbd2d864e6b"],["c41916365abb2b5d09192f5f2dbeafec208f020f12570a184dbadc3e58595997","4f14351d0087efa49d245b328984989d5caf9450f34bfc0ed16e96b58fa9913"],["841d6063a586fa475a724604da03bc5b92a2e0d2e0a36acfe4c73a5514742881","73867f59c0659e81904f9a1c7543698e62562d6744c169ce7a36de01a8d6154"],["5e95bb399a6971d376026947f89bde2f282b33810928be4ded112ac4d70e20d5","39f23f366809085beebfc71181313775a99c9aed7d8ba38b161384c746012865"],["36e4641a53948fd476c39f8a99fd974e5ec07564b5315d8bf99471bca0ef2f66","d2424b1b1abe4eb8164227b085c9aa9456ea13493fd563e06fd51cf5694c78fc"],["336581ea7bfbbb290c191a2f507a41cf5643842170e914faeab27c2c579f726","ead12168595fe1be99252129b6e56b3391f7ab1410cd1e0ef3dcdcabd2fda224"],["8ab89816dadfd6b6a1f2634fcf00ec8403781025ed6890c4849742706bd43ede","6fdcef09f2f6d0a044e654aef624136f503d459c3e89845858a47a9129cdd24e"],["1e33f1a746c9c5778133344d9299fcaa20b0938e8acff2544bb40284b8c5fb94","60660257dd11b3aa9c8ed618d24edff2306d320f1d03010e33a7d2057f3b3b6"],["85b7c1dcb3cec1b7ee7f30ded79dd20a0ed1f4cc18cbcfcfa410361fd8f08f31","3d98a9cdd026dd43f39048f25a8847f4fcafad1895d7a633c6fed3c35e999511"],["29df9fbd8d9e46509275f4b125d6d45d7fbe9a3b878a7af872a2800661ac5f51","b4c4fe99c775a606e2d8862179139ffda61dc861c019e55cd2876eb2a27d84b"],["a0b1cae06b0a847a3fea6e671aaf8adfdfe58ca2f768105c8082b2e449fce252","ae434102edde0958ec4b19d917a6a28e6b72da1834aff0e650f049503a296cf2"],["4e8ceafb9b3e9a136dc7ff67e840295b499dfb3b2133e4ba113f2e4c0e121e5","cf2174118c8b6d7a4b48f6d534ce5c79422c086a63460502b827ce62a326683c"],["d24a44e047e19b6f5afb81c7ca2f69080a5076689a010919f42725c2b789a33b","6fb8d5591b466f8fc63db50f1c0f1c69013f996887b8244d2cdec417afea8fa3"],["ea01606a7a6c9cdd249fdfcfacb99584001edd28abbab77b5104e98e8e3b35d4","322af4908c7312b0cfbfe369f7a7b3cdb7d4494bc2823700cfd652188a3ea98d"],["af8addbf2b661c8a6c6328655eb96651252007d8c5ea31be4ad196de8ce2131f","6749e67c029b85f52a034eafd096836b2520818680e26ac8f3dfbcdb71749700"],["e3ae1974566ca06cc516d47e0fb165a674a3dabcfca15e722f0e3450f45889","2aeabe7e4531510116217f07bf4d07300de97e4874f81f533420a72eeb0bd6a4"],["591ee355313d99721cf6993ffed1e3e301993ff3ed258802075ea8ced397e246","b0ea558a113c30bea60fc4775460c7901ff0b053d25ca2bdeee98f1a4be5d196"],["11396d55fda54c49f19aa97318d8da61fa8584e47b084945077cf03255b52984","998c74a8cd45ac01289d5833a7beb4744ff536b01b257be4c5767bea93ea57a4"],["3c5d2a1ba39c5a1790000738c9e0c40b8dcdfd5468754b6405540157e017aa7a","b2284279995a34e2f9d4de7396fc18b80f9b8b9fdd270f6661f79ca4c81bd257"],["cc8704b8a60a0defa3a99a7299f2e9c3fbc395afb04ac078425ef8a1793cc030","bdd46039feed17881d1e0862db347f8cf395b74fc4bcdc4e940b74e3ac1f1b13"],["c533e4f7ea8555aacd9777ac5cad29b97dd4defccc53ee7ea204119b2889b197","6f0a256bc5efdf429a2fb6242f1a43a2d9b925bb4a4b3a26bb8e0f45eb596096"],["c14f8f2ccb27d6f109f6d08d03cc96a69ba8c34eec07bbcf566d48e33da6593","c359d6923bb398f7fd4473e16fe1c28475b740dd098075e6c0e8649113dc3a38"],["a6cbc3046bc6a450bac24789fa17115a4c9739ed75f8f21ce441f72e0b90e6ef","21ae7f4680e889bb130619e2c0f95a360ceb573c70603139862afd617fa9b9f"],["347d6d9a02c48927ebfb86c1359b1caf130a3c0267d11ce6344b39f99d43cc38","60ea7f61a353524d1c987f6ecec92f086d565ab687870cb12689ff1e31c74448"],["da6545d2181db8d983f7dcb375ef5866d47c67b1bf31c8cf855ef7437b72656a","49b96715ab6878a79e78f07ce5680c5d6673051b4935bd897fea824b77dc208a"],["c40747cc9d012cb1a13b8148309c6de7ec25d6945d657146b9d5994b8feb1111","5ca560753be2a12fc6de6caf2cb489565db936156b9514e1bb5e83037e0fa2d4"],["4e42c8ec82c99798ccf3a610be870e78338c7f713348bd34c8203ef4037f3502","7571d74ee5e0fb92a7a8b33a07783341a5492144cc54bcc40a94473693606437"],["3775ab7089bc6af823aba2e1af70b236d251cadb0c86743287522a1b3b0dedea","be52d107bcfa09d8bcb9736a828cfa7fac8db17bf7a76a2c42ad961409018cf7"],["cee31cbf7e34ec379d94fb814d3d775ad954595d1314ba8846959e3e82f74e26","8fd64a14c06b589c26b947ae2bcf6bfa0149ef0be14ed4d80f448a01c43b1c6d"],["b4f9eaea09b6917619f6ea6a4eb5464efddb58fd45b1ebefcdc1a01d08b47986","39e5c9925b5a54b07433a4f18c61726f8bb131c012ca542eb24a8ac07200682a"],["d4263dfc3d2df923a0179a48966d30ce84e2515afc3dccc1b77907792ebcc60e","62dfaf07a0f78feb30e30d6295853ce189e127760ad6cf7fae164e122a208d54"],["48457524820fa65a4f8d35eb6930857c0032acc0a4a2de422233eeda897612c4","25a748ab367979d98733c38a1fa1c2e7dc6cc07db2d60a9ae7a76aaa49bd0f77"],["dfeeef1881101f2cb11644f3a2afdfc2045e19919152923f367a1767c11cceda","ecfb7056cf1de042f9420bab396793c0c390bde74b4bbdff16a83ae09a9a7517"],["6d7ef6b17543f8373c573f44e1f389835d89bcbc6062ced36c82df83b8fae859","cd450ec335438986dfefa10c57fea9bcc521a0959b2d80bbf74b190dca712d10"],["e75605d59102a5a2684500d3b991f2e3f3c88b93225547035af25af66e04541f","f5c54754a8f71ee540b9b48728473e314f729ac5308b06938360990e2bfad125"],["eb98660f4c4dfaa06a2be453d5020bc99a0c2e60abe388457dd43fefb1ed620c","6cb9a8876d9cb8520609af3add26cd20a0a7cd8a9411131ce85f44100099223e"],["13e87b027d8514d35939f2e6892b19922154596941888336dc3563e3b8dba942","fef5a3c68059a6dec5d624114bf1e91aac2b9da568d6abeb2570d55646b8adf1"],["ee163026e9fd6fe017c38f06a5be6fc125424b371ce2708e7bf4491691e5764a","1acb250f255dd61c43d94ccc670d0f58f49ae3fa15b96623e5430da0ad6c62b2"],["b268f5ef9ad51e4d78de3a750c2dc89b1e626d43505867999932e5db33af3d80","5f310d4b3c99b9ebb19f77d41c1dee018cf0d34fd4191614003e945a1216e423"],["ff07f3118a9df035e9fad85eb6c7bfe42b02f01ca99ceea3bf7ffdba93c4750d","438136d603e858a3a5c440c38eccbaddc1d2942114e2eddd4740d098ced1f0d8"],["8d8b9855c7c052a34146fd20ffb658bea4b9f69e0d825ebec16e8c3ce2b526a1","cdb559eedc2d79f926baf44fb84ea4d44bcf50fee51d7ceb30e2e7f463036758"],["52db0b5384dfbf05bfa9d472d7ae26dfe4b851ceca91b1eba54263180da32b63","c3b997d050ee5d423ebaf66a6db9f57b3180c902875679de924b69d84a7b375"],["e62f9490d3d51da6395efd24e80919cc7d0f29c3f3fa48c6fff543becbd43352","6d89ad7ba4876b0b22c2ca280c682862f342c8591f1daf5170e07bfd9ccafa7d"],["7f30ea2476b399b4957509c88f77d0191afa2ff5cb7b14fd6d8e7d65aaab1193","ca5ef7d4b231c94c3b15389a5f6311e9daff7bb67b103e9880ef4bff637acaec"],["5098ff1e1d9f14fb46a210fada6c903fef0fb7b4a1dd1d9ac60a0361800b7a00","9731141d81fc8f8084d37c6e7542006b3ee1b40d60dfe5362a5b132fd17ddc0"],["32b78c7de9ee512a72895be6b9cbefa6e2f3c4ccce445c96b9f2c81e2778ad58","ee1849f513df71e32efc3896ee28260c73bb80547ae2275ba497237794c8753c"],["e2cb74fddc8e9fbcd076eef2a7c72b0ce37d50f08269dfc074b581550547a4f7","d3aa2ed71c9dd2247a62df062736eb0baddea9e36122d2be8641abcb005cc4a4"],["8438447566d4d7bedadc299496ab357426009a35f235cb141be0d99cd10ae3a8","c4e1020916980a4da5d01ac5e6ad330734ef0d7906631c4f2390426b2edd791f"],["4162d488b89402039b584c6fc6c308870587d9c46f660b878ab65c82c711d67e","67163e903236289f776f22c25fb8a3afc1732f2b84b4e95dbda47ae5a0852649"],["3fad3fa84caf0f34f0f89bfd2dcf54fc175d767aec3e50684f3ba4a4bf5f683d","cd1bc7cb6cc407bb2f0ca647c718a730cf71872e7d0d2a53fa20efcdfe61826"],["674f2600a3007a00568c1a7ce05d0816c1fb84bf1370798f1c69532faeb1a86b","299d21f9413f33b3edf43b257004580b70db57da0b182259e09eecc69e0d38a5"],["d32f4da54ade74abb81b815ad1fb3b263d82d6c692714bcff87d29bd5ee9f08f","f9429e738b8e53b968e99016c059707782e14f4535359d582fc416910b3eea87"],["30e4e670435385556e593657135845d36fbb6931f72b08cb1ed954f1e3ce3ff6","462f9bce619898638499350113bbc9b10a878d35da70740dc695a559eb88db7b"],["be2062003c51cc3004682904330e4dee7f3dcd10b01e580bf1971b04d4cad297","62188bc49d61e5428573d48a74e1c655b1c61090905682a0d5558ed72dccb9bc"],["93144423ace3451ed29e0fb9ac2af211cb6e84a601df5993c419859fff5df04a","7c10dfb164c3425f5c71a3f9d7992038f1065224f72bb9d1d902a6d13037b47c"],["b015f8044f5fcbdcf21ca26d6c34fb8197829205c7b7d2a7cb66418c157b112c","ab8c1e086d04e813744a655b2df8d5f83b3cdc6faa3088c1d3aea1454e3a1d5f"],["d5e9e1da649d97d89e4868117a465a3a4f8a18de57a140d36b3f2af341a21b52","4cb04437f391ed73111a13cc1d4dd0db1693465c2240480d8955e8592f27447a"],["d3ae41047dd7ca065dbf8ed77b992439983005cd72e16d6f996a5316d36966bb","bd1aeb21ad22ebb22a10f0303417c6d964f8cdd7df0aca614b10dc14d125ac46"],["463e2763d885f958fc66cdd22800f0a487197d0a82e377b49f80af87c897b065","bfefacdb0e5d0fd7df3a311a94de062b26b80c61fbc97508b79992671ef7ca7f"],["7985fdfd127c0567c6f53ec1bb63ec3158e597c40bfe747c83cddfc910641917","603c12daf3d9862ef2b25fe1de289aed24ed291e0ec6708703a5bd567f32ed03"],["74a1ad6b5f76e39db2dd249410eac7f99e74c59cb83d2d0ed5ff1543da7703e9","cc6157ef18c9c63cd6193d83631bbea0093e0968942e8c33d5737fd790e0db08"],["30682a50703375f602d416664ba19b7fc9bab42c72747463a71d0896b22f6da3","553e04f6b018b4fa6c8f39e7f311d3176290d0e0f19ca73f17714d9977a22ff8"],["9e2158f0d7c0d5f26c3791efefa79597654e7a2b2464f52b1ee6c1347769ef57","712fcdd1b9053f09003a3481fa7762e9ffd7c8ef35a38509e2fbf2629008373"],["176e26989a43c9cfeba4029c202538c28172e566e3c4fce7322857f3be327d66","ed8cc9d04b29eb877d270b4878dc43c19aefd31f4eee09ee7b47834c1fa4b1c3"],["75d46efea3771e6e68abb89a13ad747ecf1892393dfc4f1b7004788c50374da8","9852390a99507679fd0b86fd2b39a868d7efc22151346e1a3ca4726586a6bed8"],["809a20c67d64900ffb698c4c825f6d5f2310fb0451c869345b7319f645605721","9e994980d9917e22b76b061927fa04143d096ccc54963e6a5ebfa5f3f8e286c1"],["1b38903a43f7f114ed4500b4eac7083fdefece1cf29c63528d563446f972c180","4036edc931a60ae889353f77fd53de4a2708b26b6f5da72ad3394119daf408f9"]]
}}},{}],15:[function(a,b,c){"use strict";function d(a,b){for(var c=[],d=1<<b+1,e=a.clone();e.cmpn(1)>=0;){var f;if(e.isOdd()){var g=e.andln(d-1);f=g>(d>>1)-1?(d>>1)-g:g,e.isubn(f)}else f=0;c.push(f);for(var h=0!==e.cmpn(0)&&0===e.andln(d-1)?b+1:1,i=1;i<h;i++)c.push(0);e.iushrn(h)}return c}function e(a,b){var c=[[],[]];a=a.clone(),b=b.clone();for(var d=0,e=0;a.cmpn(-d)>0||b.cmpn(-e)>0;){var f=a.andln(3)+d&3,g=b.andln(3)+e&3;3===f&&(f=-1),3===g&&(g=-1);var h;if(0===(1&f))h=0;else{var i=a.andln(7)+d&7;h=3!==i&&5!==i||2!==g?f:-f}c[0].push(h);var j;if(0===(1&g))j=0;else{var i=b.andln(7)+e&7;j=3!==i&&5!==i||2!==f?g:-g}c[1].push(j),2*d===h+1&&(d=1-d),2*e===j+1&&(e=1-e),a.iushrn(1),b.iushrn(1)}return c}function f(a,b,c){var d="_"+b;a.prototype[b]=function(){return void 0!==this[d]?this[d]:this[d]=c.call(this)}}function g(a){return"string"==typeof a?i.toArray(a,"hex"):a}function h(a){return new j(a,"hex","le")}var i=c,j=a("bn.js"),k=a("minimalistic-assert"),l=a("minimalistic-crypto-utils");i.assert=k,i.toArray=l.toArray,i.zero2=l.zero2,i.toHex=l.toHex,i.encode=l.encode,i.getNAF=d,i.getJSF=e,i.cachedProperty=f,i.parseBytes=g,i.intFromLE=h},{"bn.js":16,"minimalistic-assert":28,"minimalistic-crypto-utils":29}],16:[function(a,b,c){!function(b,c){"use strict";function d(a,b){if(!a)throw new Error(b||"Assertion failed")}function e(a,b){a.super_=b;var c=function(){};c.prototype=b.prototype,a.prototype=new c,a.prototype.constructor=a}function f(a,b,c){return f.isBN(a)?a:(this.negative=0,this.words=null,this.length=0,this.red=null,void(null!==a&&("le"!==b&&"be"!==b||(c=b,b=10),this._init(a||0,b||10,c||"be"))))}function g(a,b,c){for(var d=0,e=Math.min(a.length,c),f=b;f<e;f++){var g=a.charCodeAt(f)-48;d<<=4,d|=g>=49&&g<=54?g-49+10:g>=17&&g<=22?g-17+10:15&g}return d}function h(a,b,c,d){for(var e=0,f=Math.min(a.length,c),g=b;g<f;g++){var h=a.charCodeAt(g)-48;e*=d,e+=h>=49?h-49+10:h>=17?h-17+10:h}return e}function i(a){for(var b=new Array(a.bitLength()),c=0;c<b.length;c++){var d=c/26|0,e=c%26;b[c]=(a.words[d]&1<<e)>>>e}return b}function j(a,b,c){c.negative=b.negative^a.negative;var d=a.length+b.length|0;c.length=d,d=d-1|0;var e=0|a.words[0],f=0|b.words[0],g=e*f,h=67108863&g,i=g/67108864|0;c.words[0]=h;for(var j=1;j<d;j++){for(var k=i>>>26,l=67108863&i,m=Math.min(j,b.length-1),n=Math.max(0,j-a.length+1);n<=m;n++){var o=j-n|0;e=0|a.words[o],f=0|b.words[n],g=e*f+l,k+=g/67108864|0,l=67108863&g}c.words[j]=0|l,i=0|k}return 0!==i?c.words[j]=0|i:c.length--,c.strip()}function k(a,b,c){c.negative=b.negative^a.negative,c.length=a.length+b.length;for(var d=0,e=0,f=0;f<c.length-1;f++){var g=e;e=0;for(var h=67108863&d,i=Math.min(f,b.length-1),j=Math.max(0,f-a.length+1);j<=i;j++){var k=f-j,l=0|a.words[k],m=0|b.words[j],n=l*m,o=67108863&n;g=g+(n/67108864|0)|0,o=o+h|0,h=67108863&o,g=g+(o>>>26)|0,e+=g>>>26,g&=67108863}c.words[f]=h,d=g,g=e}return 0!==d?c.words[f]=d:c.length--,c.strip()}function l(a,b,c){var d=new m;return d.mulp(a,b,c)}function m(a,b){this.x=a,this.y=b}function n(a,b){this.name=a,this.p=new f(b,16),this.n=this.p.bitLength(),this.k=new f(1).iushln(this.n).isub(this.p),this.tmp=this._tmp()}function o(){n.call(this,"k256","ffffffff ffffffff ffffffff ffffffff ffffffff ffffffff fffffffe fffffc2f")}function p(){n.call(this,"p224","ffffffff ffffffff ffffffff ffffffff 00000000 00000000 00000001")}function q(){n.call(this,"p192","ffffffff ffffffff ffffffff fffffffe ffffffff ffffffff")}function r(){n.call(this,"25519","7fffffffffffffff ffffffffffffffff ffffffffffffffff ffffffffffffffed")}function s(a){if("string"==typeof a){var b=f._prime(a);this.m=b.p,this.prime=b}else d(a.gtn(1),"modulus must be greater than 1"),this.m=a,this.prime=null}function t(a){s.call(this,a),this.shift=this.m.bitLength(),this.shift%26!==0&&(this.shift+=26-this.shift%26),this.r=new f(1).iushln(this.shift),this.r2=this.imod(this.r.sqr()),this.rinv=this.r._invmp(this.m),this.minv=this.rinv.mul(this.r).isubn(1).div(this.m),this.minv=this.minv.umod(this.r),this.minv=this.r.sub(this.minv)}"object"==typeof b?b.exports=f:c.BN=f,f.BN=f,f.wordSize=26;var u;try{u=a("buffer").Buffer}catch(v){}f.isBN=function(a){return a instanceof f||null!==a&&"object"==typeof a&&a.constructor.wordSize===f.wordSize&&Array.isArray(a.words)},f.max=function(a,b){return a.cmp(b)>0?a:b},f.min=function(a,b){return a.cmp(b)<0?a:b},f.prototype._init=function(a,b,c){if("number"==typeof a)return this._initNumber(a,b,c);if("object"==typeof a)return this._initArray(a,b,c);"hex"===b&&(b=16),d(b===(0|b)&&b>=2&&b<=36),a=a.toString().replace(/\s+/g,"");var e=0;"-"===a[0]&&e++,16===b?this._parseHex(a,e):this._parseBase(a,b,e),"-"===a[0]&&(this.negative=1),this.strip(),"le"===c&&this._initArray(this.toArray(),b,c)},f.prototype._initNumber=function(a,b,c){a<0&&(this.negative=1,a=-a),a<67108864?(this.words=[67108863&a],this.length=1):a<4503599627370496?(this.words=[67108863&a,a/67108864&67108863],this.length=2):(d(a<9007199254740992),this.words=[67108863&a,a/67108864&67108863,1],this.length=3),"le"===c&&this._initArray(this.toArray(),b,c)},f.prototype._initArray=function(a,b,c){if(d("number"==typeof a.length),a.length<=0)return this.words=[0],this.length=1,this;this.length=Math.ceil(a.length/3),this.words=new Array(this.length);for(var e=0;e<this.length;e++)this.words[e]=0;var f,g,h=0;if("be"===c)for(e=a.length-1,f=0;e>=0;e-=3)g=a[e]|a[e-1]<<8|a[e-2]<<16,this.words[f]|=g<<h&67108863,this.words[f+1]=g>>>26-h&67108863,h+=24,h>=26&&(h-=26,f++);else if("le"===c)for(e=0,f=0;e<a.length;e+=3)g=a[e]|a[e+1]<<8|a[e+2]<<16,this.words[f]|=g<<h&67108863,this.words[f+1]=g>>>26-h&67108863,h+=24,h>=26&&(h-=26,f++);return this.strip()},f.prototype._parseHex=function(a,b){this.length=Math.ceil((a.length-b)/6),this.words=new Array(this.length);for(var c=0;c<this.length;c++)this.words[c]=0;var d,e,f=0;for(c=a.length-6,d=0;c>=b;c-=6)e=g(a,c,c+6),this.words[d]|=e<<f&67108863,this.words[d+1]|=e>>>26-f&4194303,f+=24,f>=26&&(f-=26,d++);c+6!==b&&(e=g(a,b,c+6),this.words[d]|=e<<f&67108863,this.words[d+1]|=e>>>26-f&4194303),this.strip()},f.prototype._parseBase=function(a,b,c){this.words=[0],this.length=1;for(var d=0,e=1;e<=67108863;e*=b)d++;d--,e=e/b|0;for(var f=a.length-c,g=f%d,i=Math.min(f,f-g)+c,j=0,k=c;k<i;k+=d)j=h(a,k,k+d,b),this.imuln(e),this.words[0]+j<67108864?this.words[0]+=j:this._iaddn(j);if(0!==g){var l=1;for(j=h(a,k,a.length,b),k=0;k<g;k++)l*=b;this.imuln(l),this.words[0]+j<67108864?this.words[0]+=j:this._iaddn(j)}},f.prototype.copy=function(a){a.words=new Array(this.length);for(var b=0;b<this.length;b++)a.words[b]=this.words[b];a.length=this.length,a.negative=this.negative,a.red=this.red},f.prototype.clone=function(){var a=new f(null);return this.copy(a),a},f.prototype._expand=function(a){for(;this.length<a;)this.words[this.length++]=0;return this},f.prototype.strip=function(){for(;this.length>1&&0===this.words[this.length-1];)this.length--;return this._normSign()},f.prototype._normSign=function(){return 1===this.length&&0===this.words[0]&&(this.negative=0),this},f.prototype.inspect=function(){return(this.red?"<BN-R: ":"<BN: ")+this.toString(16)+">"};var w=["","0","00","000","0000","00000","000000","0000000","00000000","000000000","0000000000","00000000000","000000000000","0000000000000","00000000000000","000000000000000","0000000000000000","00000000000000000","000000000000000000","0000000000000000000","00000000000000000000","000000000000000000000","0000000000000000000000","00000000000000000000000","000000000000000000000000","0000000000000000000000000"],x=[0,0,25,16,12,11,10,9,8,8,7,7,7,7,6,6,6,6,6,6,6,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5],y=[0,0,33554432,43046721,16777216,48828125,60466176,40353607,16777216,43046721,1e7,19487171,35831808,62748517,7529536,11390625,16777216,24137569,34012224,47045881,64e6,4084101,5153632,6436343,7962624,9765625,11881376,14348907,17210368,20511149,243e5,28629151,33554432,39135393,45435424,52521875,60466176];f.prototype.toString=function(a,b){a=a||10,b=0|b||1;var c;if(16===a||"hex"===a){c="";for(var e=0,f=0,g=0;g<this.length;g++){var h=this.words[g],i=(16777215&(h<<e|f)).toString(16);f=h>>>24-e&16777215,c=0!==f||g!==this.length-1?w[6-i.length]+i+c:i+c,e+=2,e>=26&&(e-=26,g--)}for(0!==f&&(c=f.toString(16)+c);c.length%b!==0;)c="0"+c;return 0!==this.negative&&(c="-"+c),c}if(a===(0|a)&&a>=2&&a<=36){var j=x[a],k=y[a];c="";var l=this.clone();for(l.negative=0;!l.isZero();){var m=l.modn(k).toString(a);l=l.idivn(k),c=l.isZero()?m+c:w[j-m.length]+m+c}for(this.isZero()&&(c="0"+c);c.length%b!==0;)c="0"+c;return 0!==this.negative&&(c="-"+c),c}d(!1,"Base should be between 2 and 36")},f.prototype.toNumber=function(){var a=this.words[0];return 2===this.length?a+=67108864*this.words[1]:3===this.length&&1===this.words[2]?a+=4503599627370496+67108864*this.words[1]:this.length>2&&d(!1,"Number can only safely store up to 53 bits"),0!==this.negative?-a:a},f.prototype.toJSON=function(){return this.toString(16)},f.prototype.toBuffer=function(a,b){return d("undefined"!=typeof u),this.toArrayLike(u,a,b)},f.prototype.toArray=function(a,b){return this.toArrayLike(Array,a,b)},f.prototype.toArrayLike=function(a,b,c){var e=this.byteLength(),f=c||Math.max(1,e);d(e<=f,"byte array longer than desired length"),d(f>0,"Requested array length <= 0"),this.strip();var g,h,i="le"===b,j=new a(f),k=this.clone();if(i){for(h=0;!k.isZero();h++)g=k.andln(255),k.iushrn(8),j[h]=g;for(;h<f;h++)j[h]=0}else{for(h=0;h<f-e;h++)j[h]=0;for(h=0;!k.isZero();h++)g=k.andln(255),k.iushrn(8),j[f-h-1]=g}return j},Math.clz32?f.prototype._countBits=function(a){return 32-Math.clz32(a)}:f.prototype._countBits=function(a){var b=a,c=0;return b>=4096&&(c+=13,b>>>=13),b>=64&&(c+=7,b>>>=7),b>=8&&(c+=4,b>>>=4),b>=2&&(c+=2,b>>>=2),c+b},f.prototype._zeroBits=function(a){if(0===a)return 26;var b=a,c=0;return 0===(8191&b)&&(c+=13,b>>>=13),0===(127&b)&&(c+=7,b>>>=7),0===(15&b)&&(c+=4,b>>>=4),0===(3&b)&&(c+=2,b>>>=2),0===(1&b)&&c++,c},f.prototype.bitLength=function(){var a=this.words[this.length-1],b=this._countBits(a);return 26*(this.length-1)+b},f.prototype.zeroBits=function(){if(this.isZero())return 0;for(var a=0,b=0;b<this.length;b++){var c=this._zeroBits(this.words[b]);if(a+=c,26!==c)break}return a},f.prototype.byteLength=function(){return Math.ceil(this.bitLength()/8)},f.prototype.toTwos=function(a){return 0!==this.negative?this.abs().inotn(a).iaddn(1):this.clone()},f.prototype.fromTwos=function(a){return this.testn(a-1)?this.notn(a).iaddn(1).ineg():this.clone()},f.prototype.isNeg=function(){return 0!==this.negative},f.prototype.neg=function(){return this.clone().ineg()},f.prototype.ineg=function(){return this.isZero()||(this.negative^=1),this},f.prototype.iuor=function(a){for(;this.length<a.length;)this.words[this.length++]=0;for(var b=0;b<a.length;b++)this.words[b]=this.words[b]|a.words[b];return this.strip()},f.prototype.ior=function(a){return d(0===(this.negative|a.negative)),this.iuor(a)},f.prototype.or=function(a){return this.length>a.length?this.clone().ior(a):a.clone().ior(this)},f.prototype.uor=function(a){return this.length>a.length?this.clone().iuor(a):a.clone().iuor(this)},f.prototype.iuand=function(a){var b;b=this.length>a.length?a:this;for(var c=0;c<b.length;c++)this.words[c]=this.words[c]&a.words[c];return this.length=b.length,this.strip()},f.prototype.iand=function(a){return d(0===(this.negative|a.negative)),this.iuand(a)},f.prototype.and=function(a){return this.length>a.length?this.clone().iand(a):a.clone().iand(this)},f.prototype.uand=function(a){return this.length>a.length?this.clone().iuand(a):a.clone().iuand(this)},f.prototype.iuxor=function(a){var b,c;this.length>a.length?(b=this,c=a):(b=a,c=this);for(var d=0;d<c.length;d++)this.words[d]=b.words[d]^c.words[d];if(this!==b)for(;d<b.length;d++)this.words[d]=b.words[d];return this.length=b.length,this.strip()},f.prototype.ixor=function(a){return d(0===(this.negative|a.negative)),this.iuxor(a)},f.prototype.xor=function(a){return this.length>a.length?this.clone().ixor(a):a.clone().ixor(this)},f.prototype.uxor=function(a){return this.length>a.length?this.clone().iuxor(a):a.clone().iuxor(this)},f.prototype.inotn=function(a){d("number"==typeof a&&a>=0);var b=0|Math.ceil(a/26),c=a%26;this._expand(b),c>0&&b--;for(var e=0;e<b;e++)this.words[e]=67108863&~this.words[e];return c>0&&(this.words[e]=~this.words[e]&67108863>>26-c),this.strip()},f.prototype.notn=function(a){return this.clone().inotn(a)},f.prototype.setn=function(a,b){d("number"==typeof a&&a>=0);var c=a/26|0,e=a%26;return this._expand(c+1),b?this.words[c]=this.words[c]|1<<e:this.words[c]=this.words[c]&~(1<<e),this.strip()},f.prototype.iadd=function(a){var b;if(0!==this.negative&&0===a.negative)return this.negative=0,b=this.isub(a),this.negative^=1,this._normSign();if(0===this.negative&&0!==a.negative)return a.negative=0,b=this.isub(a),a.negative=1,b._normSign();var c,d;this.length>a.length?(c=this,d=a):(c=a,d=this);for(var e=0,f=0;f<d.length;f++)b=(0|c.words[f])+(0|d.words[f])+e,this.words[f]=67108863&b,e=b>>>26;for(;0!==e&&f<c.length;f++)b=(0|c.words[f])+e,this.words[f]=67108863&b,e=b>>>26;if(this.length=c.length,0!==e)this.words[this.length]=e,this.length++;else if(c!==this)for(;f<c.length;f++)this.words[f]=c.words[f];return this},f.prototype.add=function(a){var b;return 0!==a.negative&&0===this.negative?(a.negative=0,b=this.sub(a),a.negative^=1,b):0===a.negative&&0!==this.negative?(this.negative=0,b=a.sub(this),this.negative=1,b):this.length>a.length?this.clone().iadd(a):a.clone().iadd(this)},f.prototype.isub=function(a){if(0!==a.negative){a.negative=0;var b=this.iadd(a);return a.negative=1,b._normSign()}if(0!==this.negative)return this.negative=0,this.iadd(a),this.negative=1,this._normSign();var c=this.cmp(a);if(0===c)return this.negative=0,this.length=1,this.words[0]=0,this;var d,e;c>0?(d=this,e=a):(d=a,e=this);for(var f=0,g=0;g<e.length;g++)b=(0|d.words[g])-(0|e.words[g])+f,f=b>>26,this.words[g]=67108863&b;for(;0!==f&&g<d.length;g++)b=(0|d.words[g])+f,f=b>>26,this.words[g]=67108863&b;if(0===f&&g<d.length&&d!==this)for(;g<d.length;g++)this.words[g]=d.words[g];return this.length=Math.max(this.length,g),d!==this&&(this.negative=1),this.strip()},f.prototype.sub=function(a){return this.clone().isub(a)};var z=function(a,b,c){var d,e,f,g=a.words,h=b.words,i=c.words,j=0,k=0|g[0],l=8191&k,m=k>>>13,n=0|g[1],o=8191&n,p=n>>>13,q=0|g[2],r=8191&q,s=q>>>13,t=0|g[3],u=8191&t,v=t>>>13,w=0|g[4],x=8191&w,y=w>>>13,z=0|g[5],A=8191&z,B=z>>>13,C=0|g[6],D=8191&C,E=C>>>13,F=0|g[7],G=8191&F,H=F>>>13,I=0|g[8],J=8191&I,K=I>>>13,L=0|g[9],M=8191&L,N=L>>>13,O=0|h[0],P=8191&O,Q=O>>>13,R=0|h[1],S=8191&R,T=R>>>13,U=0|h[2],V=8191&U,W=U>>>13,X=0|h[3],Y=8191&X,Z=X>>>13,$=0|h[4],_=8191&$,aa=$>>>13,ba=0|h[5],ca=8191&ba,da=ba>>>13,ea=0|h[6],fa=8191&ea,ga=ea>>>13,ha=0|h[7],ia=8191&ha,ja=ha>>>13,ka=0|h[8],la=8191&ka,ma=ka>>>13,na=0|h[9],oa=8191&na,pa=na>>>13;c.negative=a.negative^b.negative,c.length=19,d=Math.imul(l,P),e=Math.imul(l,Q),e=e+Math.imul(m,P)|0,f=Math.imul(m,Q);var qa=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(qa>>>26)|0,qa&=67108863,d=Math.imul(o,P),e=Math.imul(o,Q),e=e+Math.imul(p,P)|0,f=Math.imul(p,Q),d=d+Math.imul(l,S)|0,e=e+Math.imul(l,T)|0,e=e+Math.imul(m,S)|0,f=f+Math.imul(m,T)|0;var ra=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(ra>>>26)|0,ra&=67108863,d=Math.imul(r,P),e=Math.imul(r,Q),e=e+Math.imul(s,P)|0,f=Math.imul(s,Q),d=d+Math.imul(o,S)|0,e=e+Math.imul(o,T)|0,e=e+Math.imul(p,S)|0,f=f+Math.imul(p,T)|0,d=d+Math.imul(l,V)|0,e=e+Math.imul(l,W)|0,e=e+Math.imul(m,V)|0,f=f+Math.imul(m,W)|0;var sa=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(sa>>>26)|0,sa&=67108863,d=Math.imul(u,P),e=Math.imul(u,Q),e=e+Math.imul(v,P)|0,f=Math.imul(v,Q),d=d+Math.imul(r,S)|0,e=e+Math.imul(r,T)|0,e=e+Math.imul(s,S)|0,f=f+Math.imul(s,T)|0,d=d+Math.imul(o,V)|0,e=e+Math.imul(o,W)|0,e=e+Math.imul(p,V)|0,f=f+Math.imul(p,W)|0,d=d+Math.imul(l,Y)|0,e=e+Math.imul(l,Z)|0,e=e+Math.imul(m,Y)|0,f=f+Math.imul(m,Z)|0;var ta=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(ta>>>26)|0,ta&=67108863,d=Math.imul(x,P),e=Math.imul(x,Q),e=e+Math.imul(y,P)|0,f=Math.imul(y,Q),d=d+Math.imul(u,S)|0,e=e+Math.imul(u,T)|0,e=e+Math.imul(v,S)|0,f=f+Math.imul(v,T)|0,d=d+Math.imul(r,V)|0,e=e+Math.imul(r,W)|0,e=e+Math.imul(s,V)|0,f=f+Math.imul(s,W)|0,d=d+Math.imul(o,Y)|0,e=e+Math.imul(o,Z)|0,e=e+Math.imul(p,Y)|0,f=f+Math.imul(p,Z)|0,d=d+Math.imul(l,_)|0,e=e+Math.imul(l,aa)|0,e=e+Math.imul(m,_)|0,f=f+Math.imul(m,aa)|0;var ua=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(ua>>>26)|0,ua&=67108863,d=Math.imul(A,P),e=Math.imul(A,Q),e=e+Math.imul(B,P)|0,f=Math.imul(B,Q),d=d+Math.imul(x,S)|0,e=e+Math.imul(x,T)|0,e=e+Math.imul(y,S)|0,f=f+Math.imul(y,T)|0,d=d+Math.imul(u,V)|0,e=e+Math.imul(u,W)|0,e=e+Math.imul(v,V)|0,f=f+Math.imul(v,W)|0,d=d+Math.imul(r,Y)|0,e=e+Math.imul(r,Z)|0,e=e+Math.imul(s,Y)|0,f=f+Math.imul(s,Z)|0,d=d+Math.imul(o,_)|0,e=e+Math.imul(o,aa)|0,e=e+Math.imul(p,_)|0,f=f+Math.imul(p,aa)|0,d=d+Math.imul(l,ca)|0,e=e+Math.imul(l,da)|0,e=e+Math.imul(m,ca)|0,f=f+Math.imul(m,da)|0;var va=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(va>>>26)|0,va&=67108863,d=Math.imul(D,P),e=Math.imul(D,Q),e=e+Math.imul(E,P)|0,f=Math.imul(E,Q),d=d+Math.imul(A,S)|0,e=e+Math.imul(A,T)|0,e=e+Math.imul(B,S)|0,f=f+Math.imul(B,T)|0,d=d+Math.imul(x,V)|0,e=e+Math.imul(x,W)|0,e=e+Math.imul(y,V)|0,f=f+Math.imul(y,W)|0,d=d+Math.imul(u,Y)|0,e=e+Math.imul(u,Z)|0,e=e+Math.imul(v,Y)|0,f=f+Math.imul(v,Z)|0,d=d+Math.imul(r,_)|0,e=e+Math.imul(r,aa)|0,e=e+Math.imul(s,_)|0,f=f+Math.imul(s,aa)|0,d=d+Math.imul(o,ca)|0,e=e+Math.imul(o,da)|0,e=e+Math.imul(p,ca)|0,f=f+Math.imul(p,da)|0,d=d+Math.imul(l,fa)|0,e=e+Math.imul(l,ga)|0,e=e+Math.imul(m,fa)|0,f=f+Math.imul(m,ga)|0;var wa=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(wa>>>26)|0,wa&=67108863,d=Math.imul(G,P),e=Math.imul(G,Q),e=e+Math.imul(H,P)|0,f=Math.imul(H,Q),d=d+Math.imul(D,S)|0,e=e+Math.imul(D,T)|0,e=e+Math.imul(E,S)|0,f=f+Math.imul(E,T)|0,d=d+Math.imul(A,V)|0,e=e+Math.imul(A,W)|0,e=e+Math.imul(B,V)|0,f=f+Math.imul(B,W)|0,d=d+Math.imul(x,Y)|0,e=e+Math.imul(x,Z)|0,e=e+Math.imul(y,Y)|0,f=f+Math.imul(y,Z)|0,d=d+Math.imul(u,_)|0,e=e+Math.imul(u,aa)|0,e=e+Math.imul(v,_)|0,f=f+Math.imul(v,aa)|0,d=d+Math.imul(r,ca)|0,e=e+Math.imul(r,da)|0,e=e+Math.imul(s,ca)|0,f=f+Math.imul(s,da)|0,d=d+Math.imul(o,fa)|0,e=e+Math.imul(o,ga)|0,e=e+Math.imul(p,fa)|0,f=f+Math.imul(p,ga)|0,d=d+Math.imul(l,ia)|0,e=e+Math.imul(l,ja)|0,e=e+Math.imul(m,ia)|0,f=f+Math.imul(m,ja)|0;var xa=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(xa>>>26)|0,xa&=67108863,d=Math.imul(J,P),e=Math.imul(J,Q),e=e+Math.imul(K,P)|0,f=Math.imul(K,Q),d=d+Math.imul(G,S)|0,e=e+Math.imul(G,T)|0,e=e+Math.imul(H,S)|0,f=f+Math.imul(H,T)|0,d=d+Math.imul(D,V)|0,e=e+Math.imul(D,W)|0,e=e+Math.imul(E,V)|0,f=f+Math.imul(E,W)|0,d=d+Math.imul(A,Y)|0,e=e+Math.imul(A,Z)|0,e=e+Math.imul(B,Y)|0,f=f+Math.imul(B,Z)|0,d=d+Math.imul(x,_)|0,e=e+Math.imul(x,aa)|0,e=e+Math.imul(y,_)|0,f=f+Math.imul(y,aa)|0,d=d+Math.imul(u,ca)|0,e=e+Math.imul(u,da)|0,e=e+Math.imul(v,ca)|0,f=f+Math.imul(v,da)|0,d=d+Math.imul(r,fa)|0,e=e+Math.imul(r,ga)|0,e=e+Math.imul(s,fa)|0,f=f+Math.imul(s,ga)|0,d=d+Math.imul(o,ia)|0,e=e+Math.imul(o,ja)|0,e=e+Math.imul(p,ia)|0,f=f+Math.imul(p,ja)|0,d=d+Math.imul(l,la)|0,e=e+Math.imul(l,ma)|0,e=e+Math.imul(m,la)|0,f=f+Math.imul(m,ma)|0;var ya=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(ya>>>26)|0,ya&=67108863,d=Math.imul(M,P),e=Math.imul(M,Q),e=e+Math.imul(N,P)|0,f=Math.imul(N,Q),d=d+Math.imul(J,S)|0,e=e+Math.imul(J,T)|0,e=e+Math.imul(K,S)|0,f=f+Math.imul(K,T)|0,d=d+Math.imul(G,V)|0,e=e+Math.imul(G,W)|0,e=e+Math.imul(H,V)|0,f=f+Math.imul(H,W)|0,d=d+Math.imul(D,Y)|0,e=e+Math.imul(D,Z)|0,e=e+Math.imul(E,Y)|0,f=f+Math.imul(E,Z)|0,d=d+Math.imul(A,_)|0,e=e+Math.imul(A,aa)|0,e=e+Math.imul(B,_)|0,f=f+Math.imul(B,aa)|0,d=d+Math.imul(x,ca)|0,e=e+Math.imul(x,da)|0,e=e+Math.imul(y,ca)|0,f=f+Math.imul(y,da)|0,d=d+Math.imul(u,fa)|0,e=e+Math.imul(u,ga)|0,e=e+Math.imul(v,fa)|0,f=f+Math.imul(v,ga)|0,d=d+Math.imul(r,ia)|0,e=e+Math.imul(r,ja)|0,e=e+Math.imul(s,ia)|0,f=f+Math.imul(s,ja)|0,d=d+Math.imul(o,la)|0,e=e+Math.imul(o,ma)|0,e=e+Math.imul(p,la)|0,f=f+Math.imul(p,ma)|0,d=d+Math.imul(l,oa)|0,e=e+Math.imul(l,pa)|0,e=e+Math.imul(m,oa)|0,f=f+Math.imul(m,pa)|0;var za=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(za>>>26)|0,za&=67108863,d=Math.imul(M,S),e=Math.imul(M,T),e=e+Math.imul(N,S)|0,f=Math.imul(N,T),d=d+Math.imul(J,V)|0,e=e+Math.imul(J,W)|0,e=e+Math.imul(K,V)|0,f=f+Math.imul(K,W)|0,d=d+Math.imul(G,Y)|0,e=e+Math.imul(G,Z)|0,e=e+Math.imul(H,Y)|0,f=f+Math.imul(H,Z)|0,d=d+Math.imul(D,_)|0,e=e+Math.imul(D,aa)|0,e=e+Math.imul(E,_)|0,f=f+Math.imul(E,aa)|0,d=d+Math.imul(A,ca)|0,e=e+Math.imul(A,da)|0,e=e+Math.imul(B,ca)|0,f=f+Math.imul(B,da)|0,d=d+Math.imul(x,fa)|0,e=e+Math.imul(x,ga)|0,e=e+Math.imul(y,fa)|0,f=f+Math.imul(y,ga)|0,d=d+Math.imul(u,ia)|0,e=e+Math.imul(u,ja)|0,e=e+Math.imul(v,ia)|0,f=f+Math.imul(v,ja)|0,d=d+Math.imul(r,la)|0,e=e+Math.imul(r,ma)|0,e=e+Math.imul(s,la)|0,f=f+Math.imul(s,ma)|0,d=d+Math.imul(o,oa)|0,e=e+Math.imul(o,pa)|0,e=e+Math.imul(p,oa)|0,f=f+Math.imul(p,pa)|0;var Aa=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(Aa>>>26)|0,Aa&=67108863,d=Math.imul(M,V),e=Math.imul(M,W),e=e+Math.imul(N,V)|0,f=Math.imul(N,W),d=d+Math.imul(J,Y)|0,e=e+Math.imul(J,Z)|0,e=e+Math.imul(K,Y)|0,f=f+Math.imul(K,Z)|0,d=d+Math.imul(G,_)|0,e=e+Math.imul(G,aa)|0,e=e+Math.imul(H,_)|0,f=f+Math.imul(H,aa)|0,d=d+Math.imul(D,ca)|0,e=e+Math.imul(D,da)|0,e=e+Math.imul(E,ca)|0,f=f+Math.imul(E,da)|0,d=d+Math.imul(A,fa)|0,e=e+Math.imul(A,ga)|0,e=e+Math.imul(B,fa)|0,f=f+Math.imul(B,ga)|0,d=d+Math.imul(x,ia)|0,e=e+Math.imul(x,ja)|0,e=e+Math.imul(y,ia)|0,f=f+Math.imul(y,ja)|0,d=d+Math.imul(u,la)|0,e=e+Math.imul(u,ma)|0,e=e+Math.imul(v,la)|0,f=f+Math.imul(v,ma)|0,d=d+Math.imul(r,oa)|0,e=e+Math.imul(r,pa)|0,e=e+Math.imul(s,oa)|0,f=f+Math.imul(s,pa)|0;var Ba=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(Ba>>>26)|0,Ba&=67108863,d=Math.imul(M,Y),e=Math.imul(M,Z),e=e+Math.imul(N,Y)|0,f=Math.imul(N,Z),d=d+Math.imul(J,_)|0,e=e+Math.imul(J,aa)|0,e=e+Math.imul(K,_)|0,f=f+Math.imul(K,aa)|0,d=d+Math.imul(G,ca)|0,e=e+Math.imul(G,da)|0,e=e+Math.imul(H,ca)|0,f=f+Math.imul(H,da)|0,d=d+Math.imul(D,fa)|0,e=e+Math.imul(D,ga)|0,e=e+Math.imul(E,fa)|0,f=f+Math.imul(E,ga)|0,d=d+Math.imul(A,ia)|0,e=e+Math.imul(A,ja)|0,e=e+Math.imul(B,ia)|0,f=f+Math.imul(B,ja)|0,d=d+Math.imul(x,la)|0,e=e+Math.imul(x,ma)|0,e=e+Math.imul(y,la)|0,f=f+Math.imul(y,ma)|0,d=d+Math.imul(u,oa)|0,e=e+Math.imul(u,pa)|0,e=e+Math.imul(v,oa)|0,f=f+Math.imul(v,pa)|0;var Ca=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(Ca>>>26)|0,Ca&=67108863,d=Math.imul(M,_),e=Math.imul(M,aa),e=e+Math.imul(N,_)|0,f=Math.imul(N,aa),d=d+Math.imul(J,ca)|0,e=e+Math.imul(J,da)|0,e=e+Math.imul(K,ca)|0,f=f+Math.imul(K,da)|0,d=d+Math.imul(G,fa)|0,e=e+Math.imul(G,ga)|0,e=e+Math.imul(H,fa)|0,f=f+Math.imul(H,ga)|0,d=d+Math.imul(D,ia)|0,e=e+Math.imul(D,ja)|0,e=e+Math.imul(E,ia)|0,f=f+Math.imul(E,ja)|0,d=d+Math.imul(A,la)|0,e=e+Math.imul(A,ma)|0,e=e+Math.imul(B,la)|0,f=f+Math.imul(B,ma)|0,d=d+Math.imul(x,oa)|0,e=e+Math.imul(x,pa)|0,e=e+Math.imul(y,oa)|0,f=f+Math.imul(y,pa)|0;var Da=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(Da>>>26)|0,Da&=67108863,d=Math.imul(M,ca),e=Math.imul(M,da),e=e+Math.imul(N,ca)|0,f=Math.imul(N,da),d=d+Math.imul(J,fa)|0,e=e+Math.imul(J,ga)|0,e=e+Math.imul(K,fa)|0,f=f+Math.imul(K,ga)|0,d=d+Math.imul(G,ia)|0,e=e+Math.imul(G,ja)|0,e=e+Math.imul(H,ia)|0,f=f+Math.imul(H,ja)|0,d=d+Math.imul(D,la)|0,e=e+Math.imul(D,ma)|0,e=e+Math.imul(E,la)|0,f=f+Math.imul(E,ma)|0,d=d+Math.imul(A,oa)|0,e=e+Math.imul(A,pa)|0,e=e+Math.imul(B,oa)|0,f=f+Math.imul(B,pa)|0;var Ea=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(Ea>>>26)|0,Ea&=67108863,d=Math.imul(M,fa),e=Math.imul(M,ga),e=e+Math.imul(N,fa)|0,f=Math.imul(N,ga),d=d+Math.imul(J,ia)|0,e=e+Math.imul(J,ja)|0,e=e+Math.imul(K,ia)|0,f=f+Math.imul(K,ja)|0,d=d+Math.imul(G,la)|0,e=e+Math.imul(G,ma)|0,e=e+Math.imul(H,la)|0,f=f+Math.imul(H,ma)|0,d=d+Math.imul(D,oa)|0,e=e+Math.imul(D,pa)|0,e=e+Math.imul(E,oa)|0,f=f+Math.imul(E,pa)|0;var Fa=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(Fa>>>26)|0,Fa&=67108863,d=Math.imul(M,ia),e=Math.imul(M,ja),e=e+Math.imul(N,ia)|0,f=Math.imul(N,ja),d=d+Math.imul(J,la)|0,e=e+Math.imul(J,ma)|0,e=e+Math.imul(K,la)|0,f=f+Math.imul(K,ma)|0,d=d+Math.imul(G,oa)|0,e=e+Math.imul(G,pa)|0,e=e+Math.imul(H,oa)|0,f=f+Math.imul(H,pa)|0;var Ga=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(Ga>>>26)|0,Ga&=67108863,d=Math.imul(M,la),e=Math.imul(M,ma),e=e+Math.imul(N,la)|0,f=Math.imul(N,ma),d=d+Math.imul(J,oa)|0,e=e+Math.imul(J,pa)|0,e=e+Math.imul(K,oa)|0,f=f+Math.imul(K,pa)|0;var Ha=(j+d|0)+((8191&e)<<13)|0;j=(f+(e>>>13)|0)+(Ha>>>26)|0,Ha&=67108863,d=Math.imul(M,oa),e=Math.imul(M,pa),e=e+Math.imul(N,oa)|0,f=Math.imul(N,pa);var Ia=(j+d|0)+((8191&e)<<13)|0;return j=(f+(e>>>13)|0)+(Ia>>>26)|0,Ia&=67108863,i[0]=qa,i[1]=ra,i[2]=sa,i[3]=ta,i[4]=ua,i[5]=va,i[6]=wa,i[7]=xa,i[8]=ya,i[9]=za,i[10]=Aa,i[11]=Ba,i[12]=Ca,i[13]=Da,i[14]=Ea,i[15]=Fa,i[16]=Ga,i[17]=Ha,i[18]=Ia,0!==j&&(i[19]=j,c.length++),c};Math.imul||(z=j),f.prototype.mulTo=function(a,b){var c,d=this.length+a.length;return c=10===this.length&&10===a.length?z(this,a,b):d<63?j(this,a,b):d<1024?k(this,a,b):l(this,a,b)},m.prototype.makeRBT=function(a){for(var b=new Array(a),c=f.prototype._countBits(a)-1,d=0;d<a;d++)b[d]=this.revBin(d,c,a);return b},m.prototype.revBin=function(a,b,c){if(0===a||a===c-1)return a;for(var d=0,e=0;e<b;e++)d|=(1&a)<<b-e-1,a>>=1;return d},m.prototype.permute=function(a,b,c,d,e,f){for(var g=0;g<f;g++)d[g]=b[a[g]],e[g]=c[a[g]]},m.prototype.transform=function(a,b,c,d,e,f){this.permute(f,a,b,c,d,e);for(var g=1;g<e;g<<=1)for(var h=g<<1,i=Math.cos(2*Math.PI/h),j=Math.sin(2*Math.PI/h),k=0;k<e;k+=h)for(var l=i,m=j,n=0;n<g;n++){var o=c[k+n],p=d[k+n],q=c[k+n+g],r=d[k+n+g],s=l*q-m*r;r=l*r+m*q,q=s,c[k+n]=o+q,d[k+n]=p+r,c[k+n+g]=o-q,d[k+n+g]=p-r,n!==h&&(s=i*l-j*m,m=i*m+j*l,l=s)}},m.prototype.guessLen13b=function(a,b){var c=1|Math.max(b,a),d=1&c,e=0;for(c=c/2|0;c;c>>>=1)e++;return 1<<e+1+d},m.prototype.conjugate=function(a,b,c){if(!(c<=1))for(var d=0;d<c/2;d++){var e=a[d];a[d]=a[c-d-1],a[c-d-1]=e,e=b[d],b[d]=-b[c-d-1],b[c-d-1]=-e}},m.prototype.normalize13b=function(a,b){for(var c=0,d=0;d<b/2;d++){var e=8192*Math.round(a[2*d+1]/b)+Math.round(a[2*d]/b)+c;a[d]=67108863&e,c=e<67108864?0:e/67108864|0}return a},m.prototype.convert13b=function(a,b,c,e){for(var f=0,g=0;g<b;g++)f+=0|a[g],c[2*g]=8191&f,f>>>=13,c[2*g+1]=8191&f,f>>>=13;for(g=2*b;g<e;++g)c[g]=0;d(0===f),d(0===(f&-8192))},m.prototype.stub=function(a){for(var b=new Array(a),c=0;c<a;c++)b[c]=0;return b},m.prototype.mulp=function(a,b,c){var d=2*this.guessLen13b(a.length,b.length),e=this.makeRBT(d),f=this.stub(d),g=new Array(d),h=new Array(d),i=new Array(d),j=new Array(d),k=new Array(d),l=new Array(d),m=c.words;m.length=d,this.convert13b(a.words,a.length,g,d),this.convert13b(b.words,b.length,j,d),this.transform(g,f,h,i,d,e),this.transform(j,f,k,l,d,e);for(var n=0;n<d;n++){var o=h[n]*k[n]-i[n]*l[n];i[n]=h[n]*l[n]+i[n]*k[n],h[n]=o}return this.conjugate(h,i,d),this.transform(h,i,m,f,d,e),this.conjugate(m,f,d),this.normalize13b(m,d),c.negative=a.negative^b.negative,c.length=a.length+b.length,c.strip()},f.prototype.mul=function(a){var b=new f(null);return b.words=new Array(this.length+a.length),this.mulTo(a,b)},f.prototype.mulf=function(a){var b=new f(null);return b.words=new Array(this.length+a.length),l(this,a,b)},f.prototype.imul=function(a){return this.clone().mulTo(a,this)},f.prototype.imuln=function(a){d("number"==typeof a),d(a<67108864);for(var b=0,c=0;c<this.length;c++){var e=(0|this.words[c])*a,f=(67108863&e)+(67108863&b);b>>=26,b+=e/67108864|0,b+=f>>>26,this.words[c]=67108863&f}return 0!==b&&(this.words[c]=b,this.length++),this},f.prototype.muln=function(a){return this.clone().imuln(a)},f.prototype.sqr=function(){return this.mul(this)},f.prototype.isqr=function(){return this.imul(this.clone())},f.prototype.pow=function(a){var b=i(a);if(0===b.length)return new f(1);for(var c=this,d=0;d<b.length&&0===b[d];d++,c=c.sqr());if(++d<b.length)for(var e=c.sqr();d<b.length;d++,e=e.sqr())0!==b[d]&&(c=c.mul(e));return c},f.prototype.iushln=function(a){d("number"==typeof a&&a>=0);var b,c=a%26,e=(a-c)/26,f=67108863>>>26-c<<26-c;if(0!==c){var g=0;for(b=0;b<this.length;b++){var h=this.words[b]&f,i=(0|this.words[b])-h<<c;this.words[b]=i|g,g=h>>>26-c}g&&(this.words[b]=g,this.length++)}if(0!==e){for(b=this.length-1;b>=0;b--)this.words[b+e]=this.words[b];for(b=0;b<e;b++)this.words[b]=0;this.length+=e}return this.strip()},f.prototype.ishln=function(a){return d(0===this.negative),this.iushln(a)},f.prototype.iushrn=function(a,b,c){d("number"==typeof a&&a>=0);var e;e=b?(b-b%26)/26:0;var f=a%26,g=Math.min((a-f)/26,this.length),h=67108863^67108863>>>f<<f,i=c;if(e-=g,e=Math.max(0,e),i){for(var j=0;j<g;j++)i.words[j]=this.words[j];i.length=g}if(0===g);else if(this.length>g)for(this.length-=g,j=0;j<this.length;j++)this.words[j]=this.words[j+g];else this.words[0]=0,this.length=1;var k=0;for(j=this.length-1;j>=0&&(0!==k||j>=e);j--){var l=0|this.words[j];this.words[j]=k<<26-f|l>>>f,k=l&h}return i&&0!==k&&(i.words[i.length++]=k),0===this.length&&(this.words[0]=0,this.length=1),this.strip()},f.prototype.ishrn=function(a,b,c){return d(0===this.negative),this.iushrn(a,b,c)},f.prototype.shln=function(a){return this.clone().ishln(a)},f.prototype.ushln=function(a){return this.clone().iushln(a)},f.prototype.shrn=function(a){return this.clone().ishrn(a)},f.prototype.ushrn=function(a){return this.clone().iushrn(a)},f.prototype.testn=function(a){d("number"==typeof a&&a>=0);var b=a%26,c=(a-b)/26,e=1<<b;if(this.length<=c)return!1;var f=this.words[c];return!!(f&e)},f.prototype.imaskn=function(a){d("number"==typeof a&&a>=0);var b=a%26,c=(a-b)/26;if(d(0===this.negative,"imaskn works only with positive numbers"),this.length<=c)return this;if(0!==b&&c++,this.length=Math.min(c,this.length),0!==b){var e=67108863^67108863>>>b<<b;this.words[this.length-1]&=e}return this.strip()},f.prototype.maskn=function(a){return this.clone().imaskn(a)},f.prototype.iaddn=function(a){return d("number"==typeof a),d(a<67108864),a<0?this.isubn(-a):0!==this.negative?1===this.length&&(0|this.words[0])<a?(this.words[0]=a-(0|this.words[0]),this.negative=0,this):(this.negative=0,this.isubn(a),this.negative=1,this):this._iaddn(a)},f.prototype._iaddn=function(a){this.words[0]+=a;for(var b=0;b<this.length&&this.words[b]>=67108864;b++)this.words[b]-=67108864,b===this.length-1?this.words[b+1]=1:this.words[b+1]++;return this.length=Math.max(this.length,b+1),this},f.prototype.isubn=function(a){if(d("number"==typeof a),d(a<67108864),a<0)return this.iaddn(-a);if(0!==this.negative)return this.negative=0,this.iaddn(a),this.negative=1,this;if(this.words[0]-=a,1===this.length&&this.words[0]<0)this.words[0]=-this.words[0],this.negative=1;else for(var b=0;b<this.length&&this.words[b]<0;b++)this.words[b]+=67108864,this.words[b+1]-=1;return this.strip()},f.prototype.addn=function(a){return this.clone().iaddn(a)},f.prototype.subn=function(a){return this.clone().isubn(a)},f.prototype.iabs=function(){return this.negative=0,this},f.prototype.abs=function(){return this.clone().iabs()},f.prototype._ishlnsubmul=function(a,b,c){var e,f=a.length+c;this._expand(f);var g,h=0;for(e=0;e<a.length;e++){g=(0|this.words[e+c])+h;var i=(0|a.words[e])*b;g-=67108863&i,h=(g>>26)-(i/67108864|0),this.words[e+c]=67108863&g}for(;e<this.length-c;e++)g=(0|this.words[e+c])+h,h=g>>26,this.words[e+c]=67108863&g;if(0===h)return this.strip();for(d(h===-1),h=0,e=0;e<this.length;e++)g=-(0|this.words[e])+h,h=g>>26,this.words[e]=67108863&g;return this.negative=1,this.strip()},f.prototype._wordDiv=function(a,b){var c=this.length-a.length,d=this.clone(),e=a,g=0|e.words[e.length-1],h=this._countBits(g);c=26-h,0!==c&&(e=e.ushln(c),d.iushln(c),g=0|e.words[e.length-1]);var i,j=d.length-e.length;if("mod"!==b){i=new f(null),i.length=j+1,i.words=new Array(i.length);for(var k=0;k<i.length;k++)i.words[k]=0}var l=d.clone()._ishlnsubmul(e,1,j);0===l.negative&&(d=l,i&&(i.words[j]=1));for(var m=j-1;m>=0;m--){var n=67108864*(0|d.words[e.length+m])+(0|d.words[e.length+m-1]);for(n=Math.min(n/g|0,67108863),d._ishlnsubmul(e,n,m);0!==d.negative;)n--,d.negative=0,d._ishlnsubmul(e,1,m),d.isZero()||(d.negative^=1);
i&&(i.words[m]=n)}return i&&i.strip(),d.strip(),"div"!==b&&0!==c&&d.iushrn(c),{div:i||null,mod:d}},f.prototype.divmod=function(a,b,c){if(d(!a.isZero()),this.isZero())return{div:new f(0),mod:new f(0)};var e,g,h;return 0!==this.negative&&0===a.negative?(h=this.neg().divmod(a,b),"mod"!==b&&(e=h.div.neg()),"div"!==b&&(g=h.mod.neg(),c&&0!==g.negative&&g.iadd(a)),{div:e,mod:g}):0===this.negative&&0!==a.negative?(h=this.divmod(a.neg(),b),"mod"!==b&&(e=h.div.neg()),{div:e,mod:h.mod}):0!==(this.negative&a.negative)?(h=this.neg().divmod(a.neg(),b),"div"!==b&&(g=h.mod.neg(),c&&0!==g.negative&&g.isub(a)),{div:h.div,mod:g}):a.length>this.length||this.cmp(a)<0?{div:new f(0),mod:this}:1===a.length?"div"===b?{div:this.divn(a.words[0]),mod:null}:"mod"===b?{div:null,mod:new f(this.modn(a.words[0]))}:{div:this.divn(a.words[0]),mod:new f(this.modn(a.words[0]))}:this._wordDiv(a,b)},f.prototype.div=function(a){return this.divmod(a,"div",!1).div},f.prototype.mod=function(a){return this.divmod(a,"mod",!1).mod},f.prototype.umod=function(a){return this.divmod(a,"mod",!0).mod},f.prototype.divRound=function(a){var b=this.divmod(a);if(b.mod.isZero())return b.div;var c=0!==b.div.negative?b.mod.isub(a):b.mod,d=a.ushrn(1),e=a.andln(1),f=c.cmp(d);return f<0||1===e&&0===f?b.div:0!==b.div.negative?b.div.isubn(1):b.div.iaddn(1)},f.prototype.modn=function(a){d(a<=67108863);for(var b=(1<<26)%a,c=0,e=this.length-1;e>=0;e--)c=(b*c+(0|this.words[e]))%a;return c},f.prototype.idivn=function(a){d(a<=67108863);for(var b=0,c=this.length-1;c>=0;c--){var e=(0|this.words[c])+67108864*b;this.words[c]=e/a|0,b=e%a}return this.strip()},f.prototype.divn=function(a){return this.clone().idivn(a)},f.prototype.egcd=function(a){d(0===a.negative),d(!a.isZero());var b=this,c=a.clone();b=0!==b.negative?b.umod(a):b.clone();for(var e=new f(1),g=new f(0),h=new f(0),i=new f(1),j=0;b.isEven()&&c.isEven();)b.iushrn(1),c.iushrn(1),++j;for(var k=c.clone(),l=b.clone();!b.isZero();){for(var m=0,n=1;0===(b.words[0]&n)&&m<26;++m,n<<=1);if(m>0)for(b.iushrn(m);m-- >0;)(e.isOdd()||g.isOdd())&&(e.iadd(k),g.isub(l)),e.iushrn(1),g.iushrn(1);for(var o=0,p=1;0===(c.words[0]&p)&&o<26;++o,p<<=1);if(o>0)for(c.iushrn(o);o-- >0;)(h.isOdd()||i.isOdd())&&(h.iadd(k),i.isub(l)),h.iushrn(1),i.iushrn(1);b.cmp(c)>=0?(b.isub(c),e.isub(h),g.isub(i)):(c.isub(b),h.isub(e),i.isub(g))}return{a:h,b:i,gcd:c.iushln(j)}},f.prototype._invmp=function(a){d(0===a.negative),d(!a.isZero());var b=this,c=a.clone();b=0!==b.negative?b.umod(a):b.clone();for(var e=new f(1),g=new f(0),h=c.clone();b.cmpn(1)>0&&c.cmpn(1)>0;){for(var i=0,j=1;0===(b.words[0]&j)&&i<26;++i,j<<=1);if(i>0)for(b.iushrn(i);i-- >0;)e.isOdd()&&e.iadd(h),e.iushrn(1);for(var k=0,l=1;0===(c.words[0]&l)&&k<26;++k,l<<=1);if(k>0)for(c.iushrn(k);k-- >0;)g.isOdd()&&g.iadd(h),g.iushrn(1);b.cmp(c)>=0?(b.isub(c),e.isub(g)):(c.isub(b),g.isub(e))}var m;return m=0===b.cmpn(1)?e:g,m.cmpn(0)<0&&m.iadd(a),m},f.prototype.gcd=function(a){if(this.isZero())return a.abs();if(a.isZero())return this.abs();var b=this.clone(),c=a.clone();b.negative=0,c.negative=0;for(var d=0;b.isEven()&&c.isEven();d++)b.iushrn(1),c.iushrn(1);for(;;){for(;b.isEven();)b.iushrn(1);for(;c.isEven();)c.iushrn(1);var e=b.cmp(c);if(e<0){var f=b;b=c,c=f}else if(0===e||0===c.cmpn(1))break;b.isub(c)}return c.iushln(d)},f.prototype.invm=function(a){return this.egcd(a).a.umod(a)},f.prototype.isEven=function(){return 0===(1&this.words[0])},f.prototype.isOdd=function(){return 1===(1&this.words[0])},f.prototype.andln=function(a){return this.words[0]&a},f.prototype.bincn=function(a){d("number"==typeof a);var b=a%26,c=(a-b)/26,e=1<<b;if(this.length<=c)return this._expand(c+1),this.words[c]|=e,this;for(var f=e,g=c;0!==f&&g<this.length;g++){var h=0|this.words[g];h+=f,f=h>>>26,h&=67108863,this.words[g]=h}return 0!==f&&(this.words[g]=f,this.length++),this},f.prototype.isZero=function(){return 1===this.length&&0===this.words[0]},f.prototype.cmpn=function(a){var b=a<0;if(0!==this.negative&&!b)return-1;if(0===this.negative&&b)return 1;this.strip();var c;if(this.length>1)c=1;else{b&&(a=-a),d(a<=67108863,"Number is too big");var e=0|this.words[0];c=e===a?0:e<a?-1:1}return 0!==this.negative?0|-c:c},f.prototype.cmp=function(a){if(0!==this.negative&&0===a.negative)return-1;if(0===this.negative&&0!==a.negative)return 1;var b=this.ucmp(a);return 0!==this.negative?0|-b:b},f.prototype.ucmp=function(a){if(this.length>a.length)return 1;if(this.length<a.length)return-1;for(var b=0,c=this.length-1;c>=0;c--){var d=0|this.words[c],e=0|a.words[c];if(d!==e){d<e?b=-1:d>e&&(b=1);break}}return b},f.prototype.gtn=function(a){return 1===this.cmpn(a)},f.prototype.gt=function(a){return 1===this.cmp(a)},f.prototype.gten=function(a){return this.cmpn(a)>=0},f.prototype.gte=function(a){return this.cmp(a)>=0},f.prototype.ltn=function(a){return this.cmpn(a)===-1},f.prototype.lt=function(a){return this.cmp(a)===-1},f.prototype.lten=function(a){return this.cmpn(a)<=0},f.prototype.lte=function(a){return this.cmp(a)<=0},f.prototype.eqn=function(a){return 0===this.cmpn(a)},f.prototype.eq=function(a){return 0===this.cmp(a)},f.red=function(a){return new s(a)},f.prototype.toRed=function(a){return d(!this.red,"Already a number in reduction context"),d(0===this.negative,"red works only with positives"),a.convertTo(this)._forceRed(a)},f.prototype.fromRed=function(){return d(this.red,"fromRed works only with numbers in reduction context"),this.red.convertFrom(this)},f.prototype._forceRed=function(a){return this.red=a,this},f.prototype.forceRed=function(a){return d(!this.red,"Already a number in reduction context"),this._forceRed(a)},f.prototype.redAdd=function(a){return d(this.red,"redAdd works only with red numbers"),this.red.add(this,a)},f.prototype.redIAdd=function(a){return d(this.red,"redIAdd works only with red numbers"),this.red.iadd(this,a)},f.prototype.redSub=function(a){return d(this.red,"redSub works only with red numbers"),this.red.sub(this,a)},f.prototype.redISub=function(a){return d(this.red,"redISub works only with red numbers"),this.red.isub(this,a)},f.prototype.redShl=function(a){return d(this.red,"redShl works only with red numbers"),this.red.shl(this,a)},f.prototype.redMul=function(a){return d(this.red,"redMul works only with red numbers"),this.red._verify2(this,a),this.red.mul(this,a)},f.prototype.redIMul=function(a){return d(this.red,"redMul works only with red numbers"),this.red._verify2(this,a),this.red.imul(this,a)},f.prototype.redSqr=function(){return d(this.red,"redSqr works only with red numbers"),this.red._verify1(this),this.red.sqr(this)},f.prototype.redISqr=function(){return d(this.red,"redISqr works only with red numbers"),this.red._verify1(this),this.red.isqr(this)},f.prototype.redSqrt=function(){return d(this.red,"redSqrt works only with red numbers"),this.red._verify1(this),this.red.sqrt(this)},f.prototype.redInvm=function(){return d(this.red,"redInvm works only with red numbers"),this.red._verify1(this),this.red.invm(this)},f.prototype.redNeg=function(){return d(this.red,"redNeg works only with red numbers"),this.red._verify1(this),this.red.neg(this)},f.prototype.redPow=function(a){return d(this.red&&!a.red,"redPow(normalNum)"),this.red._verify1(this),this.red.pow(this,a)};var A={k256:null,p224:null,p192:null,p25519:null};n.prototype._tmp=function(){var a=new f(null);return a.words=new Array(Math.ceil(this.n/13)),a},n.prototype.ireduce=function(a){var b,c=a;do this.split(c,this.tmp),c=this.imulK(c),c=c.iadd(this.tmp),b=c.bitLength();while(b>this.n);var d=b<this.n?-1:c.ucmp(this.p);return 0===d?(c.words[0]=0,c.length=1):d>0?c.isub(this.p):c.strip(),c},n.prototype.split=function(a,b){a.iushrn(this.n,0,b)},n.prototype.imulK=function(a){return a.imul(this.k)},e(o,n),o.prototype.split=function(a,b){for(var c=4194303,d=Math.min(a.length,9),e=0;e<d;e++)b.words[e]=a.words[e];if(b.length=d,a.length<=9)return a.words[0]=0,void(a.length=1);var f=a.words[9];for(b.words[b.length++]=f&c,e=10;e<a.length;e++){var g=0|a.words[e];a.words[e-10]=(g&c)<<4|f>>>22,f=g}f>>>=22,a.words[e-10]=f,0===f&&a.length>10?a.length-=10:a.length-=9},o.prototype.imulK=function(a){a.words[a.length]=0,a.words[a.length+1]=0,a.length+=2;for(var b=0,c=0;c<a.length;c++){var d=0|a.words[c];b+=977*d,a.words[c]=67108863&b,b=64*d+(b/67108864|0)}return 0===a.words[a.length-1]&&(a.length--,0===a.words[a.length-1]&&a.length--),a},e(p,n),e(q,n),e(r,n),r.prototype.imulK=function(a){for(var b=0,c=0;c<a.length;c++){var d=19*(0|a.words[c])+b,e=67108863&d;d>>>=26,a.words[c]=e,b=d}return 0!==b&&(a.words[a.length++]=b),a},f._prime=function B(a){if(A[a])return A[a];var B;if("k256"===a)B=new o;else if("p224"===a)B=new p;else if("p192"===a)B=new q;else{if("p25519"!==a)throw new Error("Unknown prime "+a);B=new r}return A[a]=B,B},s.prototype._verify1=function(a){d(0===a.negative,"red works only with positives"),d(a.red,"red works only with red numbers")},s.prototype._verify2=function(a,b){d(0===(a.negative|b.negative),"red works only with positives"),d(a.red&&a.red===b.red,"red works only with red numbers")},s.prototype.imod=function(a){return this.prime?this.prime.ireduce(a)._forceRed(this):a.umod(this.m)._forceRed(this)},s.prototype.neg=function(a){return a.isZero()?a.clone():this.m.sub(a)._forceRed(this)},s.prototype.add=function(a,b){this._verify2(a,b);var c=a.add(b);return c.cmp(this.m)>=0&&c.isub(this.m),c._forceRed(this)},s.prototype.iadd=function(a,b){this._verify2(a,b);var c=a.iadd(b);return c.cmp(this.m)>=0&&c.isub(this.m),c},s.prototype.sub=function(a,b){this._verify2(a,b);var c=a.sub(b);return c.cmpn(0)<0&&c.iadd(this.m),c._forceRed(this)},s.prototype.isub=function(a,b){this._verify2(a,b);var c=a.isub(b);return c.cmpn(0)<0&&c.iadd(this.m),c},s.prototype.shl=function(a,b){return this._verify1(a),this.imod(a.ushln(b))},s.prototype.imul=function(a,b){return this._verify2(a,b),this.imod(a.imul(b))},s.prototype.mul=function(a,b){return this._verify2(a,b),this.imod(a.mul(b))},s.prototype.isqr=function(a){return this.imul(a,a.clone())},s.prototype.sqr=function(a){return this.mul(a,a)},s.prototype.sqrt=function(a){if(a.isZero())return a.clone();var b=this.m.andln(3);if(d(b%2===1),3===b){var c=this.m.add(new f(1)).iushrn(2);return this.pow(a,c)}for(var e=this.m.subn(1),g=0;!e.isZero()&&0===e.andln(1);)g++,e.iushrn(1);d(!e.isZero());var h=new f(1).toRed(this),i=h.redNeg(),j=this.m.subn(1).iushrn(1),k=this.m.bitLength();for(k=new f(2*k*k).toRed(this);0!==this.pow(k,j).cmp(i);)k.redIAdd(i);for(var l=this.pow(k,e),m=this.pow(a,e.addn(1).iushrn(1)),n=this.pow(a,e),o=g;0!==n.cmp(h);){for(var p=n,q=0;0!==p.cmp(h);q++)p=p.redSqr();d(q<o);var r=this.pow(l,new f(1).iushln(o-q-1));m=m.redMul(r),l=r.redSqr(),n=n.redMul(l),o=q}return m},s.prototype.invm=function(a){var b=a._invmp(this.m);return 0!==b.negative?(b.negative=0,this.imod(b).redNeg()):this.imod(b)},s.prototype.pow=function(a,b){if(b.isZero())return new f(1);if(0===b.cmpn(1))return a.clone();var c=4,d=new Array(1<<c);d[0]=new f(1).toRed(this),d[1]=a;for(var e=2;e<d.length;e++)d[e]=this.mul(d[e-1],a);var g=d[0],h=0,i=0,j=b.bitLength()%26;for(0===j&&(j=26),e=b.length-1;e>=0;e--){for(var k=b.words[e],l=j-1;l>=0;l--){var m=k>>l&1;g!==d[0]&&(g=this.sqr(g)),0!==m||0!==h?(h<<=1,h|=m,i++,(i===c||0===e&&0===l)&&(g=this.mul(g,d[h]),i=0,h=0)):i=0}j=26}return g},s.prototype.convertTo=function(a){var b=a.umod(this.m);return b===a?b.clone():b},s.prototype.convertFrom=function(a){var b=a.clone();return b.red=null,b},f.mont=function(a){return new t(a)},e(t,s),t.prototype.convertTo=function(a){return this.imod(a.ushln(this.shift))},t.prototype.convertFrom=function(a){var b=this.imod(a.mul(this.rinv));return b.red=null,b},t.prototype.imul=function(a,b){if(a.isZero()||b.isZero())return a.words[0]=0,a.length=1,a;var c=a.imul(b),d=c.maskn(this.shift).mul(this.minv).imaskn(this.shift).mul(this.m),e=c.isub(d).iushrn(this.shift),f=e;return e.cmp(this.m)>=0?f=e.isub(this.m):e.cmpn(0)<0&&(f=e.iadd(this.m)),f._forceRed(this)},t.prototype.mul=function(a,b){if(a.isZero()||b.isZero())return new f(0)._forceRed(this);var c=a.mul(b),d=c.maskn(this.shift).mul(this.minv).imaskn(this.shift).mul(this.m),e=c.isub(d).iushrn(this.shift),g=e;return e.cmp(this.m)>=0?g=e.isub(this.m):e.cmpn(0)<0&&(g=e.iadd(this.m)),g._forceRed(this)},t.prototype.invm=function(a){var b=this.imod(a._invmp(this.m).mul(this.r2));return b._forceRed(this)}}("undefined"==typeof b||b,this)},{}],17:[function(a,b,c){function d(a){this.rand=a}var e;if(b.exports=function(a){return e||(e=new d(null)),e.generate(a)},b.exports.Rand=d,d.prototype.generate=function(a){return this._rand(a)},"object"==typeof self)self.crypto&&self.crypto.getRandomValues?d.prototype._rand=function(a){var b=new Uint8Array(a);return self.crypto.getRandomValues(b),b}:self.msCrypto&&self.msCrypto.getRandomValues?d.prototype._rand=function(a){var b=new Uint8Array(a);return self.msCrypto.getRandomValues(b),b}:d.prototype._rand=function(){throw new Error("Not implemented yet")};else try{var f=a("crypto");d.prototype._rand=function(a){return f.randomBytes(a)}}catch(g){d.prototype._rand=function(a){for(var b=new Uint8Array(a),c=0;c<b.length;c++)b[c]=this.rand.getByte();return b}}},{crypto:18}],18:[function(a,b,c){},{}],19:[function(a,b,c){var d=c;d.utils=a("./hash/utils"),d.common=a("./hash/common"),d.sha=a("./hash/sha"),d.ripemd=a("./hash/ripemd"),d.hmac=a("./hash/hmac"),d.sha1=d.sha.sha1,d.sha256=d.sha.sha256,d.sha224=d.sha.sha224,d.sha384=d.sha.sha384,d.sha512=d.sha.sha512,d.ripemd160=d.ripemd.ripemd160},{"./hash/common":20,"./hash/hmac":21,"./hash/ripemd":22,"./hash/sha":23,"./hash/utils":24}],20:[function(a,b,c){function d(){this.pending=null,this.pendingTotal=0,this.blockSize=this.constructor.blockSize,this.outSize=this.constructor.outSize,this.hmacStrength=this.constructor.hmacStrength,this.padLength=this.constructor.padLength/8,this.endian="big",this._delta8=this.blockSize/8,this._delta32=this.blockSize/32}var e=a("../hash"),f=e.utils,g=f.assert;c.BlockHash=d,d.prototype.update=function(a,b){if(a=f.toArray(a,b),this.pending?this.pending=this.pending.concat(a):this.pending=a,this.pendingTotal+=a.length,this.pending.length>=this._delta8){a=this.pending;var c=a.length%this._delta8;this.pending=a.slice(a.length-c,a.length),0===this.pending.length&&(this.pending=null),a=f.join32(a,0,a.length-c,this.endian);for(var d=0;d<a.length;d+=this._delta32)this._update(a,d,d+this._delta32)}return this},d.prototype.digest=function(a){return this.update(this._pad()),g(null===this.pending),this._digest(a)},d.prototype._pad=function(){var a=this.pendingTotal,b=this._delta8,c=b-(a+this.padLength)%b,d=new Array(c+this.padLength);d[0]=128;for(var e=1;e<c;e++)d[e]=0;if(a<<=3,"big"===this.endian){for(var f=8;f<this.padLength;f++)d[e++]=0;d[e++]=0,d[e++]=0,d[e++]=0,d[e++]=0,d[e++]=a>>>24&255,d[e++]=a>>>16&255,d[e++]=a>>>8&255,d[e++]=255&a}else{d[e++]=255&a,d[e++]=a>>>8&255,d[e++]=a>>>16&255,d[e++]=a>>>24&255,d[e++]=0,d[e++]=0,d[e++]=0,d[e++]=0;for(var f=8;f<this.padLength;f++)d[e++]=0}return d}},{"../hash":19}],21:[function(a,b,c){function d(a,b,c){return this instanceof d?(this.Hash=a,this.blockSize=a.blockSize/8,this.outSize=a.outSize/8,this.inner=null,this.outer=null,void this._init(f.toArray(b,c))):new d(a,b,c)}var e=a("../hash"),f=e.utils,g=f.assert;b.exports=d,d.prototype._init=function(a){a.length>this.blockSize&&(a=(new this.Hash).update(a).digest()),g(a.length<=this.blockSize);for(var b=a.length;b<this.blockSize;b++)a.push(0);for(var b=0;b<a.length;b++)a[b]^=54;this.inner=(new this.Hash).update(a);for(var b=0;b<a.length;b++)a[b]^=106;this.outer=(new this.Hash).update(a)},d.prototype.update=function(a,b){return this.inner.update(a,b),this},d.prototype.digest=function(a){return this.outer.update(this.inner.digest()),this.outer.digest(a)}},{"../hash":19}],22:[function(a,b,c){function d(){return this instanceof d?(n.call(this),this.h=[1732584193,4023233417,2562383102,271733878,3285377520],void(this.endian="little")):new d}function e(a,b,c,d){return a<=15?b^c^d:a<=31?b&c|~b&d:a<=47?(b|~c)^d:a<=63?b&d|c&~d:b^(c|~d)}function f(a){return a<=15?0:a<=31?1518500249:a<=47?1859775393:a<=63?2400959708:2840853838}function g(a){return a<=15?1352829926:a<=31?1548603684:a<=47?1836072691:a<=63?2053994217:0}var h=a("../hash"),i=h.utils,j=i.rotl32,k=i.sum32,l=i.sum32_3,m=i.sum32_4,n=h.common.BlockHash;i.inherits(d,n),c.ripemd160=d,d.blockSize=512,d.outSize=160,d.hmacStrength=192,d.padLength=64,d.prototype._update=function(a,b){for(var c=this.h[0],d=this.h[1],h=this.h[2],i=this.h[3],n=this.h[4],s=c,t=d,u=h,v=i,w=n,x=0;x<80;x++){var y=k(j(m(c,e(x,d,h,i),a[o[x]+b],f(x)),q[x]),n);c=n,n=i,i=j(h,10),h=d,d=y,y=k(j(m(s,e(79-x,t,u,v),a[p[x]+b],g(x)),r[x]),w),s=w,w=v,v=j(u,10),u=t,t=y}y=l(this.h[1],h,v),this.h[1]=l(this.h[2],i,w),this.h[2]=l(this.h[3],n,s),this.h[3]=l(this.h[4],c,t),this.h[4]=l(this.h[0],d,u),this.h[0]=y},d.prototype._digest=function(a){return"hex"===a?i.toHex32(this.h,"little"):i.split32(this.h,"little")};var o=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,7,4,13,1,10,6,15,3,12,0,9,5,2,14,11,8,3,10,14,4,9,15,8,1,2,7,0,6,13,11,5,12,1,9,11,10,0,8,12,4,13,3,7,15,14,5,6,2,4,0,5,9,7,12,2,10,14,1,3,8,11,6,15,13],p=[5,14,7,0,9,2,11,4,13,6,15,8,1,10,3,12,6,11,3,7,0,13,5,10,14,15,8,12,4,9,1,2,15,5,1,3,7,14,6,9,11,8,12,2,10,0,4,13,8,6,4,1,3,11,15,0,5,12,2,13,9,7,10,14,12,15,10,4,1,5,8,7,6,2,13,14,0,3,9,11],q=[11,14,15,12,5,8,7,9,11,13,14,15,6,7,9,8,7,6,8,13,11,9,7,15,7,12,15,9,11,7,13,12,11,13,6,7,14,9,13,15,14,8,13,6,5,12,7,5,11,12,14,15,14,15,9,8,9,14,5,6,8,6,5,12,9,15,5,11,6,8,13,12,5,12,13,14,11,8,5,6],r=[8,9,9,11,13,15,15,5,7,7,8,11,14,14,12,6,9,13,15,7,12,8,9,11,7,7,12,7,6,15,13,11,9,7,15,11,8,6,6,14,12,13,5,14,13,13,7,5,15,5,8,11,14,14,6,14,6,9,12,9,12,5,15,8,8,5,12,9,12,5,14,6,8,13,6,5,15,13,11,11]},{"../hash":19}],23:[function(a,b,c){function d(){return this instanceof d?(V.call(this),this.h=[1779033703,3144134277,1013904242,2773480762,1359893119,2600822924,528734635,1541459225],this.k=W,void(this.W=new Array(64))):new d}function e(){return this instanceof e?(d.call(this),void(this.h=[3238371032,914150663,812702999,4144912697,4290775857,1750603025,1694076839,3204075428])):new e}function f(){return this instanceof f?(V.call(this),this.h=[1779033703,4089235720,3144134277,2227873595,1013904242,4271175723,2773480762,1595750129,1359893119,2917565137,2600822924,725511199,528734635,4215389547,1541459225,327033209],this.k=X,void(this.W=new Array(160))):new f}function g(){return this instanceof g?(f.call(this),void(this.h=[3418070365,3238371032,1654270250,914150663,2438529370,812702999,355462360,4144912697,1731405415,4290775857,2394180231,1750603025,3675008525,1694076839,1203062813,3204075428])):new g}function h(){return this instanceof h?(V.call(this),this.h=[1732584193,4023233417,2562383102,271733878,3285377520],void(this.W=new Array(80))):new h}function i(a,b,c){return a&b^~a&c}function j(a,b,c){return a&b^a&c^b&c}function k(a,b,c){return a^b^c}function l(a){return F(a,2)^F(a,13)^F(a,22)}function m(a){return F(a,6)^F(a,11)^F(a,25)}function n(a){return F(a,7)^F(a,18)^a>>>3}function o(a){return F(a,17)^F(a,19)^a>>>10}function p(a,b,c,d){return 0===a?i(b,c,d):1===a||3===a?k(b,c,d):2===a?j(b,c,d):void 0}function q(a,b,c,d,e,f){var g=a&c^~a&e;return g<0&&(g+=4294967296),g}function r(a,b,c,d,e,f){var g=b&d^~b&f;return g<0&&(g+=4294967296),g}function s(a,b,c,d,e,f){var g=a&c^a&e^c&e;return g<0&&(g+=4294967296),g}function t(a,b,c,d,e,f){var g=b&d^b&f^d&f;return g<0&&(g+=4294967296),g}function u(a,b){var c=K(a,b,28),d=K(b,a,2),e=K(b,a,7),f=c^d^e;return f<0&&(f+=4294967296),f}function v(a,b){var c=L(a,b,28),d=L(b,a,2),e=L(b,a,7),f=c^d^e;return f<0&&(f+=4294967296),f}function w(a,b){var c=K(a,b,14),d=K(a,b,18),e=K(b,a,9),f=c^d^e;return f<0&&(f+=4294967296),f}function x(a,b){var c=L(a,b,14),d=L(a,b,18),e=L(b,a,9),f=c^d^e;return f<0&&(f+=4294967296),f}function y(a,b){var c=K(a,b,1),d=K(a,b,8),e=M(a,b,7),f=c^d^e;return f<0&&(f+=4294967296),f}function z(a,b){var c=L(a,b,1),d=L(a,b,8),e=N(a,b,7),f=c^d^e;return f<0&&(f+=4294967296),f}function A(a,b){var c=K(a,b,19),d=K(b,a,29),e=M(a,b,6),f=c^d^e;return f<0&&(f+=4294967296),f}function B(a,b){var c=L(a,b,19),d=L(b,a,29),e=N(a,b,6),f=c^d^e;return f<0&&(f+=4294967296),f}var C=a("../hash"),D=C.utils,E=D.assert,F=D.rotr32,G=D.rotl32,H=D.sum32,I=D.sum32_4,J=D.sum32_5,K=D.rotr64_hi,L=D.rotr64_lo,M=D.shr64_hi,N=D.shr64_lo,O=D.sum64,P=D.sum64_hi,Q=D.sum64_lo,R=D.sum64_4_hi,S=D.sum64_4_lo,T=D.sum64_5_hi,U=D.sum64_5_lo,V=C.common.BlockHash,W=[1116352408,1899447441,3049323471,3921009573,961987163,1508970993,2453635748,2870763221,3624381080,310598401,607225278,1426881987,1925078388,2162078206,2614888103,3248222580,3835390401,4022224774,264347078,604807628,770255983,1249150122,1555081692,1996064986,2554220882,2821834349,2952996808,3210313671,3336571891,3584528711,113926993,338241895,666307205,773529912,1294757372,1396182291,1695183700,1986661051,2177026350,2456956037,2730485921,2820302411,3259730800,3345764771,3516065817,3600352804,4094571909,275423344,430227734,506948616,659060556,883997877,958139571,1322822218,1537002063,1747873779,1955562222,2024104815,2227730452,2361852424,2428436474,2756734187,3204031479,3329325298],X=[1116352408,3609767458,1899447441,602891725,3049323471,3964484399,3921009573,2173295548,961987163,4081628472,1508970993,3053834265,2453635748,2937671579,2870763221,3664609560,3624381080,2734883394,310598401,1164996542,607225278,1323610764,1426881987,3590304994,1925078388,4068182383,2162078206,991336113,2614888103,633803317,3248222580,3479774868,3835390401,2666613458,4022224774,944711139,264347078,2341262773,604807628,2007800933,770255983,1495990901,1249150122,1856431235,1555081692,3175218132,1996064986,2198950837,2554220882,3999719339,2821834349,766784016,2952996808,2566594879,3210313671,3203337956,3336571891,1034457026,3584528711,2466948901,113926993,3758326383,338241895,168717936,666307205,1188179964,773529912,1546045734,1294757372,1522805485,1396182291,2643833823,1695183700,2343527390,1986661051,1014477480,2177026350,1206759142,2456956037,344077627,2730485921,1290863460,2820302411,3158454273,3259730800,3505952657,3345764771,106217008,3516065817,3606008344,3600352804,1432725776,4094571909,1467031594,275423344,851169720,430227734,3100823752,506948616,1363258195,659060556,3750685593,883997877,3785050280,958139571,3318307427,1322822218,3812723403,1537002063,2003034995,1747873779,3602036899,1955562222,1575990012,2024104815,1125592928,2227730452,2716904306,2361852424,442776044,2428436474,593698344,2756734187,3733110249,3204031479,2999351573,3329325298,3815920427,3391569614,3928383900,3515267271,566280711,3940187606,3454069534,4118630271,4000239992,116418474,1914138554,174292421,2731055270,289380356,3203993006,460393269,320620315,685471733,587496836,852142971,1086792851,1017036298,365543100,1126000580,2618297676,1288033470,3409855158,1501505948,4234509866,1607167915,987167468,1816402316,1246189591],Y=[1518500249,1859775393,2400959708,3395469782];D.inherits(d,V),c.sha256=d,d.blockSize=512,d.outSize=256,d.hmacStrength=192,d.padLength=64,d.prototype._update=function(a,b){for(var c=this.W,d=0;d<16;d++)c[d]=a[b+d];for(;d<c.length;d++)c[d]=I(o(c[d-2]),c[d-7],n(c[d-15]),c[d-16]);var e=this.h[0],f=this.h[1],g=this.h[2],h=this.h[3],k=this.h[4],p=this.h[5],q=this.h[6],r=this.h[7];E(this.k.length===c.length);for(var d=0;d<c.length;d++){var s=J(r,m(k),i(k,p,q),this.k[d],c[d]),t=H(l(e),j(e,f,g));r=q,q=p,p=k,k=H(h,s),h=g,g=f,f=e,e=H(s,t)}this.h[0]=H(this.h[0],e),this.h[1]=H(this.h[1],f),this.h[2]=H(this.h[2],g),this.h[3]=H(this.h[3],h),this.h[4]=H(this.h[4],k),this.h[5]=H(this.h[5],p),this.h[6]=H(this.h[6],q),this.h[7]=H(this.h[7],r)},d.prototype._digest=function(a){return"hex"===a?D.toHex32(this.h,"big"):D.split32(this.h,"big")},D.inherits(e,d),c.sha224=e,e.blockSize=512,e.outSize=224,e.hmacStrength=192,e.padLength=64,e.prototype._digest=function(a){return"hex"===a?D.toHex32(this.h.slice(0,7),"big"):D.split32(this.h.slice(0,7),"big")},D.inherits(f,V),c.sha512=f,f.blockSize=1024,f.outSize=512,f.hmacStrength=192,f.padLength=128,f.prototype._prepareBlock=function(a,b){for(var c=this.W,d=0;d<32;d++)c[d]=a[b+d];for(;d<c.length;d+=2){var e=A(c[d-4],c[d-3]),f=B(c[d-4],c[d-3]),g=c[d-14],h=c[d-13],i=y(c[d-30],c[d-29]),j=z(c[d-30],c[d-29]),k=c[d-32],l=c[d-31];c[d]=R(e,f,g,h,i,j,k,l),c[d+1]=S(e,f,g,h,i,j,k,l)}},f.prototype._update=function(a,b){this._prepareBlock(a,b);var c=this.W,d=this.h[0],e=this.h[1],f=this.h[2],g=this.h[3],h=this.h[4],i=this.h[5],j=this.h[6],k=this.h[7],l=this.h[8],m=this.h[9],n=this.h[10],o=this.h[11],p=this.h[12],y=this.h[13],z=this.h[14],A=this.h[15];E(this.k.length===c.length);for(var B=0;B<c.length;B+=2){var C=z,D=A,F=w(l,m),G=x(l,m),H=q(l,m,n,o,p,y),I=r(l,m,n,o,p,y),J=this.k[B],K=this.k[B+1],L=c[B],M=c[B+1],N=T(C,D,F,G,H,I,J,K,L,M),R=U(C,D,F,G,H,I,J,K,L,M),C=u(d,e),D=v(d,e),F=s(d,e,f,g,h,i),G=t(d,e,f,g,h,i),S=P(C,D,F,G),V=Q(C,D,F,G);z=p,A=y,p=n,y=o,n=l,o=m,l=P(j,k,N,R),m=Q(k,k,N,R),j=h,k=i,h=f,i=g,f=d,g=e,d=P(N,R,S,V),e=Q(N,R,S,V)}O(this.h,0,d,e),O(this.h,2,f,g),O(this.h,4,h,i),O(this.h,6,j,k),O(this.h,8,l,m),O(this.h,10,n,o),O(this.h,12,p,y),O(this.h,14,z,A)},f.prototype._digest=function(a){return"hex"===a?D.toHex32(this.h,"big"):D.split32(this.h,"big")},D.inherits(g,f),c.sha384=g,g.blockSize=1024,g.outSize=384,g.hmacStrength=192,g.padLength=128,g.prototype._digest=function(a){return"hex"===a?D.toHex32(this.h.slice(0,12),"big"):D.split32(this.h.slice(0,12),"big")},D.inherits(h,V),c.sha1=h,h.blockSize=512,h.outSize=160,h.hmacStrength=80,h.padLength=64,h.prototype._update=function(a,b){for(var c=this.W,d=0;d<16;d++)c[d]=a[b+d];for(;d<c.length;d++)c[d]=G(c[d-3]^c[d-8]^c[d-14]^c[d-16],1);for(var e=this.h[0],f=this.h[1],g=this.h[2],h=this.h[3],i=this.h[4],d=0;d<c.length;d++){var j=~~(d/20),k=J(G(e,5),p(j,f,g,h),i,c[d],Y[j]);i=h,h=g,g=G(f,30),f=e,e=k}this.h[0]=H(this.h[0],e),this.h[1]=H(this.h[1],f),this.h[2]=H(this.h[2],g),this.h[3]=H(this.h[3],h),this.h[4]=H(this.h[4],i)},h.prototype._digest=function(a){return"hex"===a?D.toHex32(this.h,"big"):D.split32(this.h,"big")}},{"../hash":19}],24:[function(a,b,c){function d(a,b){if(Array.isArray(a))return a.slice();if(!a)return[];var c=[];if("string"==typeof a)if(b){if("hex"===b){a=a.replace(/[^a-z0-9]+/gi,""),a.length%2!==0&&(a="0"+a);for(var d=0;d<a.length;d+=2)c.push(parseInt(a[d]+a[d+1],16))}}else for(var d=0;d<a.length;d++){var e=a.charCodeAt(d),f=e>>8,g=255&e;f?c.push(f,g):c.push(g)}else for(var d=0;d<a.length;d++)c[d]=0|a[d];return c}function e(a){for(var b="",c=0;c<a.length;c++)b+=h(a[c].toString(16));return b}function f(a){var b=a>>>24|a>>>8&65280|a<<8&16711680|(255&a)<<24;return b>>>0}function g(a,b){for(var c="",d=0;d<a.length;d++){var e=a[d];"little"===b&&(e=f(e)),c+=i(e.toString(16))}return c}function h(a){return 1===a.length?"0"+a:a}function i(a){return 7===a.length?"0"+a:6===a.length?"00"+a:5===a.length?"000"+a:4===a.length?"0000"+a:3===a.length?"00000"+a:2===a.length?"000000"+a:1===a.length?"0000000"+a:a}function j(a,b,c,d){var e=c-b;r(e%4===0);for(var f=new Array(e/4),g=0,h=b;g<f.length;g++,h+=4){var i;i="big"===d?a[h]<<24|a[h+1]<<16|a[h+2]<<8|a[h+3]:a[h+3]<<24|a[h+2]<<16|a[h+1]<<8|a[h],f[g]=i>>>0}return f}function k(a,b){for(var c=new Array(4*a.length),d=0,e=0;d<a.length;d++,e+=4){var f=a[d];"big"===b?(c[e]=f>>>24,c[e+1]=f>>>16&255,c[e+2]=f>>>8&255,c[e+3]=255&f):(c[e+3]=f>>>24,c[e+2]=f>>>16&255,c[e+1]=f>>>8&255,c[e]=255&f)}return c}function l(a,b){return a>>>b|a<<32-b}function m(a,b){return a<<b|a>>>32-b}function n(a,b){return a+b>>>0}function o(a,b,c){return a+b+c>>>0}function p(a,b,c,d){return a+b+c+d>>>0}function q(a,b,c,d,e){return a+b+c+d+e>>>0}function r(a,b){if(!a)throw new Error(b||"Assertion failed")}function s(a,b,c,d){var e=a[b],f=a[b+1],g=d+f>>>0,h=(g<d?1:0)+c+e;a[b]=h>>>0,a[b+1]=g}function t(a,b,c,d){var e=b+d>>>0,f=(e<b?1:0)+a+c;return f>>>0}function u(a,b,c,d){var e=b+d;return e>>>0}function v(a,b,c,d,e,f,g,h){var i=0,j=b;j=j+d>>>0,i+=j<b?1:0,j=j+f>>>0,i+=j<f?1:0,j=j+h>>>0,i+=j<h?1:0;var k=a+c+e+g+i;return k>>>0}function w(a,b,c,d,e,f,g,h){var i=b+d+f+h;return i>>>0}function x(a,b,c,d,e,f,g,h,i,j){var k=0,l=b;l=l+d>>>0,k+=l<b?1:0,l=l+f>>>0,k+=l<f?1:0,l=l+h>>>0,k+=l<h?1:0,l=l+j>>>0,k+=l<j?1:0;var m=a+c+e+g+i+k;return m>>>0}function y(a,b,c,d,e,f,g,h,i,j){var k=b+d+f+h+j;return k>>>0}function z(a,b,c){var d=b<<32-c|a>>>c;return d>>>0}function A(a,b,c){var d=a<<32-c|b>>>c;return d>>>0}function B(a,b,c){return a>>>c}function C(a,b,c){var d=a<<32-c|b>>>c;return d>>>0}var D=c,E=a("inherits");D.toArray=d,D.toHex=e,D.htonl=f,D.toHex32=g,D.zero2=h,D.zero8=i,D.join32=j,D.split32=k,D.rotr32=l,D.rotl32=m,D.sum32=n,D.sum32_3=o,D.sum32_4=p,D.sum32_5=q,D.assert=r,D.inherits=E,c.sum64=s,c.sum64_hi=t,c.sum64_lo=u,c.sum64_4_hi=v,c.sum64_4_lo=w,c.sum64_5_hi=x,c.sum64_5_lo=y,c.rotr64_hi=z,c.rotr64_lo=A,c.shr64_hi=B,c.shr64_lo=C},{inherits:27}],25:[function(a,b,c){"use strict";function d(a){if(!(this instanceof d))return new d(a);this.hash=a.hash,this.predResist=!!a.predResist,this.outLen=this.hash.outSize,this.minEntropy=a.minEntropy||this.hash.hmacStrength,this.reseed=null,this.reseedInterval=null,this.K=null,this.V=null;var b=f.toArray(a.entropy,a.entropyEnc||"hex"),c=f.toArray(a.nonce,a.nonceEnc||"hex"),e=f.toArray(a.pers,a.persEnc||"hex");g(b.length>=this.minEntropy/8,"Not enough entropy. Minimum is: "+this.minEntropy+" bits"),this._init(b,c,e)}var e=a("hash.js"),f=a("minimalistic-crypto-utils"),g=a("minimalistic-assert");b.exports=d,d.prototype._init=function(a,b,c){var d=a.concat(b).concat(c);this.K=new Array(this.outLen/8),this.V=new Array(this.outLen/8);for(var e=0;e<this.V.length;e++)this.K[e]=0,this.V[e]=1;this._update(d),this.reseed=1,this.reseedInterval=281474976710656},d.prototype._hmac=function(){return new e.hmac(this.hash,this.K)},d.prototype._update=function(a){var b=this._hmac().update(this.V).update([0]);a&&(b=b.update(a)),this.K=b.digest(),this.V=this._hmac().update(this.V).digest(),a&&(this.K=this._hmac().update(this.V).update([1]).update(a).digest(),this.V=this._hmac().update(this.V).digest())},d.prototype.reseed=function(a,b,c,d){"string"!=typeof b&&(d=c,c=b,b=null),a=f.toArray(a,b),c=f.toArray(c,d),g(a.length>=this.minEntropy/8,"Not enough entropy. Minimum is: "+this.minEntropy+" bits"),this._update(a.concat(c||[])),this.reseed=1},d.prototype.generate=function(a,b,c,d){if(this.reseed>this.reseedInterval)throw new Error("Reseed is required");"string"!=typeof b&&(d=c,c=b,b=null),c&&(c=f.toArray(c,d||"hex"),this._update(c));for(var e=[];e.length<a;)this.V=this._hmac().update(this.V).digest(),e=e.concat(this.V);var g=e.slice(0,a);return this._update(c),this.reseed++,f.encode(g,b)}},{"hash.js":19,"minimalistic-assert":28,"minimalistic-crypto-utils":26}],26:[function(a,b,c){"use strict";function d(a,b){if(Array.isArray(a))return a.slice();if(!a)return[];var c=[];if("string"!=typeof a){for(var d=0;d<a.length;d++)c[d]=0|a[d];return c}if("hex"===b){a=a.replace(/[^a-z0-9]+/gi,""),a.length%2!==0&&(a="0"+a);for(var d=0;d<a.length;d+=2)c.push(parseInt(a[d]+a[d+1],16))}else for(var d=0;d<a.length;d++){var e=a.charCodeAt(d),f=e>>8,g=255&e;f?c.push(f,g):c.push(g)}return c}function e(a){return 1===a.length?"0"+a:a}function f(a){for(var b="",c=0;c<a.length;c++)b+=e(a[c].toString(16));return b}var g=c;g.toArray=d,g.zero2=e,g.toHex=f,g.encode=function(a,b){return"hex"===b?f(a):a}},{}],27:[function(a,b,c){"function"==typeof Object.create?b.exports=function(a,b){a.super_=b,a.prototype=Object.create(b.prototype,{constructor:{value:a,enumerable:!1,writable:!0,configurable:!0}})}:b.exports=function(a,b){a.super_=b;var c=function(){};c.prototype=b.prototype,a.prototype=new c,a.prototype.constructor=a}},{}],28:[function(a,b,c){function d(a,b){if(!a)throw new Error(b||"Assertion failed")}b.exports=d,d.equal=function(a,b,c){if(a!=b)throw new Error(c||"Assertion failed: "+a+" != "+b)}},{}],29:[function(a,b,c){"use strict";function d(a,b){if(Array.isArray(a))return a.slice();if(!a)return[];var c=[];if("string"!=typeof a){for(var d=0;d<a.length;d++)c[d]=0|a[d];return c}if(b){if("hex"===b){a=a.replace(/[^a-z0-9]+/gi,""),a.length%2!==0&&(a="0"+a);for(var d=0;d<a.length;d+=2)c.push(parseInt(a[d]+a[d+1],16));
}}else for(var d=0;d<a.length;d++){var e=a.charCodeAt(d),f=e>>8,g=255&e;f?c.push(f,g):c.push(g)}return c}function e(a){return 1===a.length?"0"+a:a}function f(a){for(var b="",c=0;c<a.length;c++)b+=e(a[c].toString(16));return b}var g=c;g.toArray=d,g.zero2=e,g.toHex=f,g.encode=function(a,b){return"hex"===b?f(a):a}},{}],30:[function(a,b,c){b.exports={name:"elliptic",version:"6.4.0",description:"EC cryptography",main:"lib/elliptic.js",files:["lib"],scripts:{jscs:"jscs benchmarks/*.js lib/*.js lib/**/*.js lib/**/**/*.js test/index.js",jshint:"jscs benchmarks/*.js lib/*.js lib/**/*.js lib/**/**/*.js test/index.js",lint:"npm run jscs && npm run jshint",unit:"istanbul test _mocha --reporter=spec test/index.js",test:"npm run lint && npm run unit",version:"grunt dist && git add dist/"},repository:{type:"git",url:"git@github.com:indutny/elliptic"},keywords:["EC","Elliptic","curve","Cryptography"],author:"Fedor Indutny <fedor@indutny.com>",license:"MIT",bugs:{url:"https://github.com/indutny/elliptic/issues"},homepage:"https://github.com/indutny/elliptic",devDependencies:{brfs:"^1.4.3",coveralls:"^2.11.3",grunt:"^0.4.5","grunt-browserify":"^5.0.0","grunt-cli":"^1.2.0","grunt-contrib-connect":"^1.0.0","grunt-contrib-copy":"^1.0.0","grunt-contrib-uglify":"^1.0.1","grunt-mocha-istanbul":"^3.0.1","grunt-saucelabs":"^8.6.2",istanbul:"^0.4.2",jscs:"^2.9.0",jshint:"^2.6.0",mocha:"^2.1.0"},dependencies:{"bn.js":"^4.4.0",brorand:"^1.0.1","hash.js":"^1.0.0","hmac-drbg":"^1.0.0",inherits:"^2.0.1","minimalistic-assert":"^1.0.0","minimalistic-crypto-utils":"^1.0.0"}}},{}]},{},[1])(1)});
function token_units() { return 100000 }; // mVEO
function s2c(x) { return x / token_units(); }
function c2s(x) {
    return Math.floor(parseFloat(x.value, 10) * token_units());
}
function array_to_int(l) {
    var x = 0;
    for (var i = 0; i < l.length; i++) {
        x = (256 * x) + l[i];
    }
    return x;
}
function toHex(str) {
    var hex = '';
    for(var i=0;i<str.length;i++) {
        l = str.charCodeAt(i).toString(16);
        var z = "";
        if (l.length < 2) { z = "0"; }
        hex += z;
	hex += ''+str.charCodeAt(i).toString(16);
    }
    return hex;
}
function fromHex(h) {
    var s = '';
    for(var i = 0; (2*i) < h.length;i++) {
        var m = h.slice((2*i), (2*(i+1)));
        var n = parseInt(m, 16);
        var l = String.fromCharCode(n);
        s = s.concat(l);
    }
    return s;
}
function string_to_array(x) {
    var a = new Uint8Array(x.length);
    for (var i=0; i<x.length; i++) {
        a[i] = x.charCodeAt(i);
    }
    return Array.from(a);
}
function integer_to_array(i, size) {
    var a = [];
    for ( var b = 0; b < size ; b++ ) {
        a.push(((i % 256) + 256) % 256);
        i = Math.floor(i/256);
    }
    return a.reverse();
}
function array_to_string(x) {
    var a = "";
    for (var i=0; i<x.length ; i++) {
        a += String.fromCharCode(x[i]);
    }
    return a;
}
function hash2integer(h) {
    function hash2integer2(h, i, n) {
        var x = h[i];
        if  ( x == 0 ) {
            return hash2integer2(h, i+1, n+(256*8));
        } else {
            return n + hash2integer3(x, h[i+1]);
        }
    }
    function dec2bin(dec){
        n = (dec).toString(2);
        n="00000000".substr(n.length)+n;
        return n;
    }
    function hash2integer3(byte1, byte2) {
        var x = dec2bin(byte1).concat(dec2bin(byte2));
        return hash2integer4(x, 0, 0);
    }
    function hash2integer4(binary, i, n) {
        var x = binary[i];
        if ( x == "0" ) { return hash2integer4(binary, i+1, n+256) }
        else {
            var b2 = binary.slice(i, i+8);
            var y = hash2integer5(b2) + n;
            return y;
        }
    }
    function hash2integer5(bin) {
        var x = 0;
        for (var i=0; i < bin.length; i++) {
            var y = bin[i];
            if ( y == "0" ) { x = x * 2; }
            else { x = 1 + (x * 2) }
        }
        return x;
    }
    return hash2integer2(h.concat([255]), 0, 0);
}
function newhash2integer(h) {
    function hash2integer2(h, i, n) {
        var x = h[i];
        if  ( x == 0 ) {
            return hash2integer2(h, i+1, n+(256*8));
        } else {
            return n + hash2integer3(x, h[i+1]);
        }
    }
    function dec2bin(dec){
        n = (dec).toString(2);
        n="00000000".substr(n.length)+n;
        return n;
    }
    function hash2integer3(byte1, byte2) {
        var x = dec2bin(byte1).concat(dec2bin(byte2));
        return hash2integer4(x, 0, 0);
    }
    function hash2integer4(binary, i, n) {
        var x = binary[i];
        if ( x == "0" ) { return hash2integer4(binary, i+1, n+256) }
        else {
            var b2 = binary.slice(i+1, i+9);//this is the only line that is different between hash2integer and newhash2integer
            var y = hash2integer5(b2) + n;
            return y;
        }
    }
    function hash2integer5(bin) {
        var x = 0;
        for (var i=0; i < bin.length; i++) {
            var y = bin[i];
            if ( y == "0" ) { x = x * 2; }
            else { x = 1 + (x * 2) }
        }
        return x;
    }

    return hash2integer2(h.concat([255]), 0, 0);
}

function input_maker2(vl, fn) {
    var inp = document.createElement("input");
    inp.type = "button";
    inp.value = vl;
    inp.className = "btn";
    inp.onclick = fn;

    return inp;
}
function button_maker2(val, fun) {
    var button = document.createElement("button");
    //button.type = "button";
    button.innerHTML = val;
    button.className = "btn";
    button.onclick = fun;
    return button;
}
function br() {
    return document.createElement("br");
};
function hr() {
    return document.createElement("hr");
};
function append_children(d, l) {
    for (var i = 0; i < l.length; i++) {
        d.appendChild(l[i]);
    }
}
function wrapper(wd, wl) {
    var wr = document.createElement("div");
    wr.className = wd;
    for (var i = 0; i < wl.length; i++) {
        wr.appendChild(wl[i]);
    }
    return wr;
}
function text(a) {
    var x2 = document.createElement("h8");
    x2.innerHTML = a;
    return x2;
};
function htitle(a) {
    var ttl = document.createElement("h3");
    ttl.innerHTML = a;
    return ttl;
};
function msg(a) {
    var m = document.createElement("p");
    m.className = "msg";
    m.innerHTML = a;
    return m;
};
function pre(a) {
    var txt = document.createElement("pre");
    txt.innerHTML = a;
    return txt;
};

function tree_number_to_value(t) {
    if (t < 101) {
        return t;
    } else {
        var top = 101;
        var bottom = 100;
	var t2 = t - 100;
        var x = tree_number_det_power(10000, top, bottom, t2);
        return Math.floor(x / 100);
    }
}
function tree_number_det_power(base, top, bottom, t) {
    if (t == 1) {
        return Math.floor((base * top) / bottom);
    }
    var r = Math.floor(t % 2);
    if (r == 1) {
        var base2 = Math.floor((base * top) / bottom);
        return tree_number_det_power(base2, top, bottom, t-1);
    } else if (r == 0) {
        var top2 = Math.floor((top * top)  / bottom);
        return tree_number_det_power(base, top2, bottom,
                                     Math.floor(t / 2));
    }
}
function parse_address(A) {
    //remove spaces or periods. " " "."
    A2 = A.replace(/\ /g,'');
    A3 = A2.replace(/\./g,'');
    A4 = A3.replace(/\n/g,'');
    //if it is the wrong length, make an error.
    //88
    B = ((A4).length == 88);
    if (B) { return A4; } else { return 0; };
}

function getter(t, u, callback){
    var xmlhttp=new XMLHttpRequest();
    xmlhttp.onreadystatechange = callback;
    xmlhttp.open("POST",u,true);
    xmlhttp.send(JSON.stringify(t));
    return xmlhttp
}
function get(t, callback) {
    u = url(get_port(), get_ip());
    return getter(t, u, callback);
}
function url(port, ip) { return "http://".concat(ip).concat(":").concat(port.toString().concat("/")); }
function xml_check(x) {
    return ((x.readyState === 4) && (x.status === 200)); };
function xml_out(x) { return x.responseText; }
function refresh_helper(x, cmd, innercallback, callback, n) {
    if (n < 1) { return "failed to connect"; }
    else if (x.status == 400) {
        //the data we sent to the server got mixed up along the way, so it looks invalid to the server.
        //So lets re-send the command.
        setTimeout(function() {
            return variable_public_get(cmd, innercallback);
        }, 200); }
    else if (x.status == 0) {
        //this means that the server got our message, and it is still processing a response for us. So lets wait a bit, and then check if it is ready.
        setTimeout(function() {
                       return refresh_helper(x, cmd, innercallback,
                                             callback, n - 1);
                   }, 150);
    }
    else if (xml_check(x)) {
        //this means that the server got our message, and it sent a response. The response is ready to read, so lets read it.
        callback(xml_out(x));}
    else {
        //console.log(x.readyState);
        //console.log(x.status);
        setTimeout(function() {return refresh_helper(x, cmd, innercallback, callback, n);}, 10);}
}

my_status = "nil";

//function variable_get(cmd, callback) {
//    var x = local_get(cmd);
//    var_get(x, callback);
//}
function variable_public_get(cmd, callback) {
    var foobar = get(cmd);
    var_get(foobar, callback, cmd);
}
function var_get(x, callback, cmd) {
    refresh_helper(x, cmd, callback, function(){
	p = JSON.parse(xml_out(x));
	callback(p[1]);
    }, 100);
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

/** @fileOverview Bit array codec implementations.
 *
 * @author Emily Stark
 * @author Mike Hamburg
 * @author Dan Boneh
 */
/**
 * Arrays of bytes
 * @namespace
 */
sjcl.codec.bytes = {
    /** Convert from a bitArray to an array of bytes. */
    fromBits: function (arr) {
        var out = [], bl = sjcl.bitArray.bitLength(arr), i, tmp;
        for (i=0; i<bl/8; i++) {
            if ((i&3) === 0) {
                tmp = arr[i/4];
            }
            out.push(tmp >>> 24);
            tmp <<= 8;
        }
        return out;
    },
    /** Convert from an array of bytes to a bitArray. */
    toBits: function (bytes) {
        var out = [], i, tmp=0;
        for (i=0; i<bytes.length; i++) {
            tmp = tmp << 8 | bytes[i];
            if ((i&3) === 3) {
                out.push(tmp);
                tmp = 0;
            }
        }
        if (i&3) {
            out.push(sjcl.bitArray.partial(8*(i&3), tmp));
        }
        return out;
    }
};

function hash(input) {//array of bytes -- array of bytes
    var b = sjcl.codec.bytes.toBits(input);
    var x = sjcl.hash.sha256.hash(b);
    return sjcl.codec.bytes.fromBits(x);
}

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

"use strict";

(function(root) {

    function checkInt(value) {
        return (parseInt(value) === value);
    }

    function checkInts(arrayish) {
        if (!checkInt(arrayish.length)) { return false; }

        for (var i = 0; i < arrayish.length; i++) {
            if (!checkInt(arrayish[i]) || arrayish[i] < 0 || arrayish[i] > 255) {
                return false;
            }
        }

        return true;
    }

    function coerceArray(arg, copy) {

        // ArrayBuffer view
        if (arg.buffer && ArrayBuffer.isView(arg) && arg.name === 'Uint8Array') {

            if (copy) {
                if (arg.slice) {
                    arg = arg.slice();
                } else {
                    arg = Array.prototype.slice.call(arg);
                }
            }

            return arg;
        }

        // It's an array; check it is a valid representation of a byte
        if (Array.isArray(arg)) {
            if (!checkInts(arg)) {
                throw new Error('Array contains invalid value: ' + arg);
            }

            return new Uint8Array(arg);
        }

        // Something else, but behaves like an array (maybe a Buffer? Arguments?)
        if (checkInt(arg.length) && checkInts(arg)) {
            return new Uint8Array(arg);
        }

        throw new Error('unsupported array-like object');
    }

    function createArray(length) {
        return new Uint8Array(length);
    }

    function copyArray(sourceArray, targetArray, targetStart, sourceStart, sourceEnd) {
        if (sourceStart != null || sourceEnd != null) {
            if (sourceArray.slice) {
                sourceArray = sourceArray.slice(sourceStart, sourceEnd);
            } else {
                sourceArray = Array.prototype.slice.call(sourceArray, sourceStart, sourceEnd);
            }
        }
        targetArray.set(sourceArray, targetStart);
    }



    var convertUtf8 = (function() {
        function toBytes(text) {
            var result = [], i = 0;
            text = encodeURI(text);
            while (i < text.length) {
                var c = text.charCodeAt(i++);

                // if it is a % sign, encode the following 2 bytes as a hex value
                if (c === 37) {
                    result.push(parseInt(text.substr(i, 2), 16))
                    i += 2;

                // otherwise, just the actual byte
                } else {
                    result.push(c)
                }
            }

            return coerceArray(result);
        }

        function fromBytes(bytes) {
            var result = [], i = 0;

            while (i < bytes.length) {
                var c = bytes[i];

                if (c < 128) {
                    result.push(String.fromCharCode(c));
                    i++;
                } else if (c > 191 && c < 224) {
                    result.push(String.fromCharCode(((c & 0x1f) << 6) | (bytes[i + 1] & 0x3f)));
                    i += 2;
                } else {
                    result.push(String.fromCharCode(((c & 0x0f) << 12) | ((bytes[i + 1] & 0x3f) << 6) | (bytes[i + 2] & 0x3f)));
                    i += 3;
                }
            }

            return result.join('');
        }

        return {
            toBytes: toBytes,
            fromBytes: fromBytes,
        }
    })();

    var convertHex = (function() {
        function toBytes(text) {
            var result = [];
            for (var i = 0; i < text.length; i += 2) {
                result.push(parseInt(text.substr(i, 2), 16));
            }

            return result;
        }

        // http://ixti.net/development/javascript/2011/11/11/base64-encodedecode-of-utf8-in-browser-with-js.html
        var Hex = '0123456789abcdef';

        function fromBytes(bytes) {
                var result = [];
                for (var i = 0; i < bytes.length; i++) {
                    var v = bytes[i];
                    result.push(Hex[(v & 0xf0) >> 4] + Hex[v & 0x0f]);
                }
                return result.join('');
        }

        return {
            toBytes: toBytes,
            fromBytes: fromBytes,
        }
    })();


    // Number of rounds by keysize
    var numberOfRounds = {16: 10, 24: 12, 32: 14}

    // Round constant words
    var rcon = [0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91];

    // S-box and Inverse S-box (S is for Substitution)
    var S = [0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76, 0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0, 0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15, 0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75, 0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84, 0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf, 0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8, 0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2, 0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73, 0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb, 0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79, 0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08, 0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a, 0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e, 0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf, 0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16];
    var Si =[0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb, 0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb, 0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e, 0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25, 0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92, 0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84, 0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06, 0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b, 0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73, 0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e, 0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b, 0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4, 0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f, 0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef, 0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61, 0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d];

    // Transformations for encryption
    var T1 = [0xc66363a5, 0xf87c7c84, 0xee777799, 0xf67b7b8d, 0xfff2f20d, 0xd66b6bbd, 0xde6f6fb1, 0x91c5c554, 0x60303050, 0x02010103, 0xce6767a9, 0x562b2b7d, 0xe7fefe19, 0xb5d7d762, 0x4dababe6, 0xec76769a, 0x8fcaca45, 0x1f82829d, 0x89c9c940, 0xfa7d7d87, 0xeffafa15, 0xb25959eb, 0x8e4747c9, 0xfbf0f00b, 0x41adadec, 0xb3d4d467, 0x5fa2a2fd, 0x45afafea, 0x239c9cbf, 0x53a4a4f7, 0xe4727296, 0x9bc0c05b, 0x75b7b7c2, 0xe1fdfd1c, 0x3d9393ae, 0x4c26266a, 0x6c36365a, 0x7e3f3f41, 0xf5f7f702, 0x83cccc4f, 0x6834345c, 0x51a5a5f4, 0xd1e5e534, 0xf9f1f108, 0xe2717193, 0xabd8d873, 0x62313153, 0x2a15153f, 0x0804040c, 0x95c7c752, 0x46232365, 0x9dc3c35e, 0x30181828, 0x379696a1, 0x0a05050f, 0x2f9a9ab5, 0x0e070709, 0x24121236, 0x1b80809b, 0xdfe2e23d, 0xcdebeb26, 0x4e272769, 0x7fb2b2cd, 0xea75759f, 0x1209091b, 0x1d83839e, 0x582c2c74, 0x341a1a2e, 0x361b1b2d, 0xdc6e6eb2, 0xb45a5aee, 0x5ba0a0fb, 0xa45252f6, 0x763b3b4d, 0xb7d6d661, 0x7db3b3ce, 0x5229297b, 0xdde3e33e, 0x5e2f2f71, 0x13848497, 0xa65353f5, 0xb9d1d168, 0x00000000, 0xc1eded2c, 0x40202060, 0xe3fcfc1f, 0x79b1b1c8, 0xb65b5bed, 0xd46a6abe, 0x8dcbcb46, 0x67bebed9, 0x7239394b, 0x944a4ade, 0x984c4cd4, 0xb05858e8, 0x85cfcf4a, 0xbbd0d06b, 0xc5efef2a, 0x4faaaae5, 0xedfbfb16, 0x864343c5, 0x9a4d4dd7, 0x66333355, 0x11858594, 0x8a4545cf, 0xe9f9f910, 0x04020206, 0xfe7f7f81, 0xa05050f0, 0x783c3c44, 0x259f9fba, 0x4ba8a8e3, 0xa25151f3, 0x5da3a3fe, 0x804040c0, 0x058f8f8a, 0x3f9292ad, 0x219d9dbc, 0x70383848, 0xf1f5f504, 0x63bcbcdf, 0x77b6b6c1, 0xafdada75, 0x42212163, 0x20101030, 0xe5ffff1a, 0xfdf3f30e, 0xbfd2d26d, 0x81cdcd4c, 0x180c0c14, 0x26131335, 0xc3ecec2f, 0xbe5f5fe1, 0x359797a2, 0x884444cc, 0x2e171739, 0x93c4c457, 0x55a7a7f2, 0xfc7e7e82, 0x7a3d3d47, 0xc86464ac, 0xba5d5de7, 0x3219192b, 0xe6737395, 0xc06060a0, 0x19818198, 0x9e4f4fd1, 0xa3dcdc7f, 0x44222266, 0x542a2a7e, 0x3b9090ab, 0x0b888883, 0x8c4646ca, 0xc7eeee29, 0x6bb8b8d3, 0x2814143c, 0xa7dede79, 0xbc5e5ee2, 0x160b0b1d, 0xaddbdb76, 0xdbe0e03b, 0x64323256, 0x743a3a4e, 0x140a0a1e, 0x924949db, 0x0c06060a, 0x4824246c, 0xb85c5ce4, 0x9fc2c25d, 0xbdd3d36e, 0x43acacef, 0xc46262a6, 0x399191a8, 0x319595a4, 0xd3e4e437, 0xf279798b, 0xd5e7e732, 0x8bc8c843, 0x6e373759, 0xda6d6db7, 0x018d8d8c, 0xb1d5d564, 0x9c4e4ed2, 0x49a9a9e0, 0xd86c6cb4, 0xac5656fa, 0xf3f4f407, 0xcfeaea25, 0xca6565af, 0xf47a7a8e, 0x47aeaee9, 0x10080818, 0x6fbabad5, 0xf0787888, 0x4a25256f, 0x5c2e2e72, 0x381c1c24, 0x57a6a6f1, 0x73b4b4c7, 0x97c6c651, 0xcbe8e823, 0xa1dddd7c, 0xe874749c, 0x3e1f1f21, 0x964b4bdd, 0x61bdbddc, 0x0d8b8b86, 0x0f8a8a85, 0xe0707090, 0x7c3e3e42, 0x71b5b5c4, 0xcc6666aa, 0x904848d8, 0x06030305, 0xf7f6f601, 0x1c0e0e12, 0xc26161a3, 0x6a35355f, 0xae5757f9, 0x69b9b9d0, 0x17868691, 0x99c1c158, 0x3a1d1d27, 0x279e9eb9, 0xd9e1e138, 0xebf8f813, 0x2b9898b3, 0x22111133, 0xd26969bb, 0xa9d9d970, 0x078e8e89, 0x339494a7, 0x2d9b9bb6, 0x3c1e1e22, 0x15878792, 0xc9e9e920, 0x87cece49, 0xaa5555ff, 0x50282878, 0xa5dfdf7a, 0x038c8c8f, 0x59a1a1f8, 0x09898980, 0x1a0d0d17, 0x65bfbfda, 0xd7e6e631, 0x844242c6, 0xd06868b8, 0x824141c3, 0x299999b0, 0x5a2d2d77, 0x1e0f0f11, 0x7bb0b0cb, 0xa85454fc, 0x6dbbbbd6, 0x2c16163a];
    var T2 = [0xa5c66363, 0x84f87c7c, 0x99ee7777, 0x8df67b7b, 0x0dfff2f2, 0xbdd66b6b, 0xb1de6f6f, 0x5491c5c5, 0x50603030, 0x03020101, 0xa9ce6767, 0x7d562b2b, 0x19e7fefe, 0x62b5d7d7, 0xe64dabab, 0x9aec7676, 0x458fcaca, 0x9d1f8282, 0x4089c9c9, 0x87fa7d7d, 0x15effafa, 0xebb25959, 0xc98e4747, 0x0bfbf0f0, 0xec41adad, 0x67b3d4d4, 0xfd5fa2a2, 0xea45afaf, 0xbf239c9c, 0xf753a4a4, 0x96e47272, 0x5b9bc0c0, 0xc275b7b7, 0x1ce1fdfd, 0xae3d9393, 0x6a4c2626, 0x5a6c3636, 0x417e3f3f, 0x02f5f7f7, 0x4f83cccc, 0x5c683434, 0xf451a5a5, 0x34d1e5e5, 0x08f9f1f1, 0x93e27171, 0x73abd8d8, 0x53623131, 0x3f2a1515, 0x0c080404, 0x5295c7c7, 0x65462323, 0x5e9dc3c3, 0x28301818, 0xa1379696, 0x0f0a0505, 0xb52f9a9a, 0x090e0707, 0x36241212, 0x9b1b8080, 0x3ddfe2e2, 0x26cdebeb, 0x694e2727, 0xcd7fb2b2, 0x9fea7575, 0x1b120909, 0x9e1d8383, 0x74582c2c, 0x2e341a1a, 0x2d361b1b, 0xb2dc6e6e, 0xeeb45a5a, 0xfb5ba0a0, 0xf6a45252, 0x4d763b3b, 0x61b7d6d6, 0xce7db3b3, 0x7b522929, 0x3edde3e3, 0x715e2f2f, 0x97138484, 0xf5a65353, 0x68b9d1d1, 0x00000000, 0x2cc1eded, 0x60402020, 0x1fe3fcfc, 0xc879b1b1, 0xedb65b5b, 0xbed46a6a, 0x468dcbcb, 0xd967bebe, 0x4b723939, 0xde944a4a, 0xd4984c4c, 0xe8b05858, 0x4a85cfcf, 0x6bbbd0d0, 0x2ac5efef, 0xe54faaaa, 0x16edfbfb, 0xc5864343, 0xd79a4d4d, 0x55663333, 0x94118585, 0xcf8a4545, 0x10e9f9f9, 0x06040202, 0x81fe7f7f, 0xf0a05050, 0x44783c3c, 0xba259f9f, 0xe34ba8a8, 0xf3a25151, 0xfe5da3a3, 0xc0804040, 0x8a058f8f, 0xad3f9292, 0xbc219d9d, 0x48703838, 0x04f1f5f5, 0xdf63bcbc, 0xc177b6b6, 0x75afdada, 0x63422121, 0x30201010, 0x1ae5ffff, 0x0efdf3f3, 0x6dbfd2d2, 0x4c81cdcd, 0x14180c0c, 0x35261313, 0x2fc3ecec, 0xe1be5f5f, 0xa2359797, 0xcc884444, 0x392e1717, 0x5793c4c4, 0xf255a7a7, 0x82fc7e7e, 0x477a3d3d, 0xacc86464, 0xe7ba5d5d, 0x2b321919, 0x95e67373, 0xa0c06060, 0x98198181, 0xd19e4f4f, 0x7fa3dcdc, 0x66442222, 0x7e542a2a, 0xab3b9090, 0x830b8888, 0xca8c4646, 0x29c7eeee, 0xd36bb8b8, 0x3c281414, 0x79a7dede, 0xe2bc5e5e, 0x1d160b0b, 0x76addbdb, 0x3bdbe0e0, 0x56643232, 0x4e743a3a, 0x1e140a0a, 0xdb924949, 0x0a0c0606, 0x6c482424, 0xe4b85c5c, 0x5d9fc2c2, 0x6ebdd3d3, 0xef43acac, 0xa6c46262, 0xa8399191, 0xa4319595, 0x37d3e4e4, 0x8bf27979, 0x32d5e7e7, 0x438bc8c8, 0x596e3737, 0xb7da6d6d, 0x8c018d8d, 0x64b1d5d5, 0xd29c4e4e, 0xe049a9a9, 0xb4d86c6c, 0xfaac5656, 0x07f3f4f4, 0x25cfeaea, 0xafca6565, 0x8ef47a7a, 0xe947aeae, 0x18100808, 0xd56fbaba, 0x88f07878, 0x6f4a2525, 0x725c2e2e, 0x24381c1c, 0xf157a6a6, 0xc773b4b4, 0x5197c6c6, 0x23cbe8e8, 0x7ca1dddd, 0x9ce87474, 0x213e1f1f, 0xdd964b4b, 0xdc61bdbd, 0x860d8b8b, 0x850f8a8a, 0x90e07070, 0x427c3e3e, 0xc471b5b5, 0xaacc6666, 0xd8904848, 0x05060303, 0x01f7f6f6, 0x121c0e0e, 0xa3c26161, 0x5f6a3535, 0xf9ae5757, 0xd069b9b9, 0x91178686, 0x5899c1c1, 0x273a1d1d, 0xb9279e9e, 0x38d9e1e1, 0x13ebf8f8, 0xb32b9898, 0x33221111, 0xbbd26969, 0x70a9d9d9, 0x89078e8e, 0xa7339494, 0xb62d9b9b, 0x223c1e1e, 0x92158787, 0x20c9e9e9, 0x4987cece, 0xffaa5555, 0x78502828, 0x7aa5dfdf, 0x8f038c8c, 0xf859a1a1, 0x80098989, 0x171a0d0d, 0xda65bfbf, 0x31d7e6e6, 0xc6844242, 0xb8d06868, 0xc3824141, 0xb0299999, 0x775a2d2d, 0x111e0f0f, 0xcb7bb0b0, 0xfca85454, 0xd66dbbbb, 0x3a2c1616];
    var T3 = [0x63a5c663, 0x7c84f87c, 0x7799ee77, 0x7b8df67b, 0xf20dfff2, 0x6bbdd66b, 0x6fb1de6f, 0xc55491c5, 0x30506030, 0x01030201, 0x67a9ce67, 0x2b7d562b, 0xfe19e7fe, 0xd762b5d7, 0xabe64dab, 0x769aec76, 0xca458fca, 0x829d1f82, 0xc94089c9, 0x7d87fa7d, 0xfa15effa, 0x59ebb259, 0x47c98e47, 0xf00bfbf0, 0xadec41ad, 0xd467b3d4, 0xa2fd5fa2, 0xafea45af, 0x9cbf239c, 0xa4f753a4, 0x7296e472, 0xc05b9bc0, 0xb7c275b7, 0xfd1ce1fd, 0x93ae3d93, 0x266a4c26, 0x365a6c36, 0x3f417e3f, 0xf702f5f7, 0xcc4f83cc, 0x345c6834, 0xa5f451a5, 0xe534d1e5, 0xf108f9f1, 0x7193e271, 0xd873abd8, 0x31536231, 0x153f2a15, 0x040c0804, 0xc75295c7, 0x23654623, 0xc35e9dc3, 0x18283018, 0x96a13796, 0x050f0a05, 0x9ab52f9a, 0x07090e07, 0x12362412, 0x809b1b80, 0xe23ddfe2, 0xeb26cdeb, 0x27694e27, 0xb2cd7fb2, 0x759fea75, 0x091b1209, 0x839e1d83, 0x2c74582c, 0x1a2e341a, 0x1b2d361b, 0x6eb2dc6e, 0x5aeeb45a, 0xa0fb5ba0, 0x52f6a452, 0x3b4d763b, 0xd661b7d6, 0xb3ce7db3, 0x297b5229, 0xe33edde3, 0x2f715e2f, 0x84971384, 0x53f5a653, 0xd168b9d1, 0x00000000, 0xed2cc1ed, 0x20604020, 0xfc1fe3fc, 0xb1c879b1, 0x5bedb65b, 0x6abed46a, 0xcb468dcb, 0xbed967be, 0x394b7239, 0x4ade944a, 0x4cd4984c, 0x58e8b058, 0xcf4a85cf, 0xd06bbbd0, 0xef2ac5ef, 0xaae54faa, 0xfb16edfb, 0x43c58643, 0x4dd79a4d, 0x33556633, 0x85941185, 0x45cf8a45, 0xf910e9f9, 0x02060402, 0x7f81fe7f, 0x50f0a050, 0x3c44783c, 0x9fba259f, 0xa8e34ba8, 0x51f3a251, 0xa3fe5da3, 0x40c08040, 0x8f8a058f, 0x92ad3f92, 0x9dbc219d, 0x38487038, 0xf504f1f5, 0xbcdf63bc, 0xb6c177b6, 0xda75afda, 0x21634221, 0x10302010, 0xff1ae5ff, 0xf30efdf3, 0xd26dbfd2, 0xcd4c81cd, 0x0c14180c, 0x13352613, 0xec2fc3ec, 0x5fe1be5f, 0x97a23597, 0x44cc8844, 0x17392e17, 0xc45793c4, 0xa7f255a7, 0x7e82fc7e, 0x3d477a3d, 0x64acc864, 0x5de7ba5d, 0x192b3219, 0x7395e673, 0x60a0c060, 0x81981981, 0x4fd19e4f, 0xdc7fa3dc, 0x22664422, 0x2a7e542a, 0x90ab3b90, 0x88830b88, 0x46ca8c46, 0xee29c7ee, 0xb8d36bb8, 0x143c2814, 0xde79a7de, 0x5ee2bc5e, 0x0b1d160b, 0xdb76addb, 0xe03bdbe0, 0x32566432, 0x3a4e743a, 0x0a1e140a, 0x49db9249, 0x060a0c06, 0x246c4824, 0x5ce4b85c, 0xc25d9fc2, 0xd36ebdd3, 0xacef43ac, 0x62a6c462, 0x91a83991, 0x95a43195, 0xe437d3e4, 0x798bf279, 0xe732d5e7, 0xc8438bc8, 0x37596e37, 0x6db7da6d, 0x8d8c018d, 0xd564b1d5, 0x4ed29c4e, 0xa9e049a9, 0x6cb4d86c, 0x56faac56, 0xf407f3f4, 0xea25cfea, 0x65afca65, 0x7a8ef47a, 0xaee947ae, 0x08181008, 0xbad56fba, 0x7888f078, 0x256f4a25, 0x2e725c2e, 0x1c24381c, 0xa6f157a6, 0xb4c773b4, 0xc65197c6, 0xe823cbe8, 0xdd7ca1dd, 0x749ce874, 0x1f213e1f, 0x4bdd964b, 0xbddc61bd, 0x8b860d8b, 0x8a850f8a, 0x7090e070, 0x3e427c3e, 0xb5c471b5, 0x66aacc66, 0x48d89048, 0x03050603, 0xf601f7f6, 0x0e121c0e, 0x61a3c261, 0x355f6a35, 0x57f9ae57, 0xb9d069b9, 0x86911786, 0xc15899c1, 0x1d273a1d, 0x9eb9279e, 0xe138d9e1, 0xf813ebf8, 0x98b32b98, 0x11332211, 0x69bbd269, 0xd970a9d9, 0x8e89078e, 0x94a73394, 0x9bb62d9b, 0x1e223c1e, 0x87921587, 0xe920c9e9, 0xce4987ce, 0x55ffaa55, 0x28785028, 0xdf7aa5df, 0x8c8f038c, 0xa1f859a1, 0x89800989, 0x0d171a0d, 0xbfda65bf, 0xe631d7e6, 0x42c68442, 0x68b8d068, 0x41c38241, 0x99b02999, 0x2d775a2d, 0x0f111e0f, 0xb0cb7bb0, 0x54fca854, 0xbbd66dbb, 0x163a2c16];
    var T4 = [0x6363a5c6, 0x7c7c84f8, 0x777799ee, 0x7b7b8df6, 0xf2f20dff, 0x6b6bbdd6, 0x6f6fb1de, 0xc5c55491, 0x30305060, 0x01010302, 0x6767a9ce, 0x2b2b7d56, 0xfefe19e7, 0xd7d762b5, 0xababe64d, 0x76769aec, 0xcaca458f, 0x82829d1f, 0xc9c94089, 0x7d7d87fa, 0xfafa15ef, 0x5959ebb2, 0x4747c98e, 0xf0f00bfb, 0xadadec41, 0xd4d467b3, 0xa2a2fd5f, 0xafafea45, 0x9c9cbf23, 0xa4a4f753, 0x727296e4, 0xc0c05b9b, 0xb7b7c275, 0xfdfd1ce1, 0x9393ae3d, 0x26266a4c, 0x36365a6c, 0x3f3f417e, 0xf7f702f5, 0xcccc4f83, 0x34345c68, 0xa5a5f451, 0xe5e534d1, 0xf1f108f9, 0x717193e2, 0xd8d873ab, 0x31315362, 0x15153f2a, 0x04040c08, 0xc7c75295, 0x23236546, 0xc3c35e9d, 0x18182830, 0x9696a137, 0x05050f0a, 0x9a9ab52f, 0x0707090e, 0x12123624, 0x80809b1b, 0xe2e23ddf, 0xebeb26cd, 0x2727694e, 0xb2b2cd7f, 0x75759fea, 0x09091b12, 0x83839e1d, 0x2c2c7458, 0x1a1a2e34, 0x1b1b2d36, 0x6e6eb2dc, 0x5a5aeeb4, 0xa0a0fb5b, 0x5252f6a4, 0x3b3b4d76, 0xd6d661b7, 0xb3b3ce7d, 0x29297b52, 0xe3e33edd, 0x2f2f715e, 0x84849713, 0x5353f5a6, 0xd1d168b9, 0x00000000, 0xeded2cc1, 0x20206040, 0xfcfc1fe3, 0xb1b1c879, 0x5b5bedb6, 0x6a6abed4, 0xcbcb468d, 0xbebed967, 0x39394b72, 0x4a4ade94, 0x4c4cd498, 0x5858e8b0, 0xcfcf4a85, 0xd0d06bbb, 0xefef2ac5, 0xaaaae54f, 0xfbfb16ed, 0x4343c586, 0x4d4dd79a, 0x33335566, 0x85859411, 0x4545cf8a, 0xf9f910e9, 0x02020604, 0x7f7f81fe, 0x5050f0a0, 0x3c3c4478, 0x9f9fba25, 0xa8a8e34b, 0x5151f3a2, 0xa3a3fe5d, 0x4040c080, 0x8f8f8a05, 0x9292ad3f, 0x9d9dbc21, 0x38384870, 0xf5f504f1, 0xbcbcdf63, 0xb6b6c177, 0xdada75af, 0x21216342, 0x10103020, 0xffff1ae5, 0xf3f30efd, 0xd2d26dbf, 0xcdcd4c81, 0x0c0c1418, 0x13133526, 0xecec2fc3, 0x5f5fe1be, 0x9797a235, 0x4444cc88, 0x1717392e, 0xc4c45793, 0xa7a7f255, 0x7e7e82fc, 0x3d3d477a, 0x6464acc8, 0x5d5de7ba, 0x19192b32, 0x737395e6, 0x6060a0c0, 0x81819819, 0x4f4fd19e, 0xdcdc7fa3, 0x22226644, 0x2a2a7e54, 0x9090ab3b, 0x8888830b, 0x4646ca8c, 0xeeee29c7, 0xb8b8d36b, 0x14143c28, 0xdede79a7, 0x5e5ee2bc, 0x0b0b1d16, 0xdbdb76ad, 0xe0e03bdb, 0x32325664, 0x3a3a4e74, 0x0a0a1e14, 0x4949db92, 0x06060a0c, 0x24246c48, 0x5c5ce4b8, 0xc2c25d9f, 0xd3d36ebd, 0xacacef43, 0x6262a6c4, 0x9191a839, 0x9595a431, 0xe4e437d3, 0x79798bf2, 0xe7e732d5, 0xc8c8438b, 0x3737596e, 0x6d6db7da, 0x8d8d8c01, 0xd5d564b1, 0x4e4ed29c, 0xa9a9e049, 0x6c6cb4d8, 0x5656faac, 0xf4f407f3, 0xeaea25cf, 0x6565afca, 0x7a7a8ef4, 0xaeaee947, 0x08081810, 0xbabad56f, 0x787888f0, 0x25256f4a, 0x2e2e725c, 0x1c1c2438, 0xa6a6f157, 0xb4b4c773, 0xc6c65197, 0xe8e823cb, 0xdddd7ca1, 0x74749ce8, 0x1f1f213e, 0x4b4bdd96, 0xbdbddc61, 0x8b8b860d, 0x8a8a850f, 0x707090e0, 0x3e3e427c, 0xb5b5c471, 0x6666aacc, 0x4848d890, 0x03030506, 0xf6f601f7, 0x0e0e121c, 0x6161a3c2, 0x35355f6a, 0x5757f9ae, 0xb9b9d069, 0x86869117, 0xc1c15899, 0x1d1d273a, 0x9e9eb927, 0xe1e138d9, 0xf8f813eb, 0x9898b32b, 0x11113322, 0x6969bbd2, 0xd9d970a9, 0x8e8e8907, 0x9494a733, 0x9b9bb62d, 0x1e1e223c, 0x87879215, 0xe9e920c9, 0xcece4987, 0x5555ffaa, 0x28287850, 0xdfdf7aa5, 0x8c8c8f03, 0xa1a1f859, 0x89898009, 0x0d0d171a, 0xbfbfda65, 0xe6e631d7, 0x4242c684, 0x6868b8d0, 0x4141c382, 0x9999b029, 0x2d2d775a, 0x0f0f111e, 0xb0b0cb7b, 0x5454fca8, 0xbbbbd66d, 0x16163a2c];

    // Transformations for decryption
    var T5 = [0x51f4a750, 0x7e416553, 0x1a17a4c3, 0x3a275e96, 0x3bab6bcb, 0x1f9d45f1, 0xacfa58ab, 0x4be30393, 0x2030fa55, 0xad766df6, 0x88cc7691, 0xf5024c25, 0x4fe5d7fc, 0xc52acbd7, 0x26354480, 0xb562a38f, 0xdeb15a49, 0x25ba1b67, 0x45ea0e98, 0x5dfec0e1, 0xc32f7502, 0x814cf012, 0x8d4697a3, 0x6bd3f9c6, 0x038f5fe7, 0x15929c95, 0xbf6d7aeb, 0x955259da, 0xd4be832d, 0x587421d3, 0x49e06929, 0x8ec9c844, 0x75c2896a, 0xf48e7978, 0x99583e6b, 0x27b971dd, 0xbee14fb6, 0xf088ad17, 0xc920ac66, 0x7dce3ab4, 0x63df4a18, 0xe51a3182, 0x97513360, 0x62537f45, 0xb16477e0, 0xbb6bae84, 0xfe81a01c, 0xf9082b94, 0x70486858, 0x8f45fd19, 0x94de6c87, 0x527bf8b7, 0xab73d323, 0x724b02e2, 0xe31f8f57, 0x6655ab2a, 0xb2eb2807, 0x2fb5c203, 0x86c57b9a, 0xd33708a5, 0x302887f2, 0x23bfa5b2, 0x02036aba, 0xed16825c, 0x8acf1c2b, 0xa779b492, 0xf307f2f0, 0x4e69e2a1, 0x65daf4cd, 0x0605bed5, 0xd134621f, 0xc4a6fe8a, 0x342e539d, 0xa2f355a0, 0x058ae132, 0xa4f6eb75, 0x0b83ec39, 0x4060efaa, 0x5e719f06, 0xbd6e1051, 0x3e218af9, 0x96dd063d, 0xdd3e05ae, 0x4de6bd46, 0x91548db5, 0x71c45d05, 0x0406d46f, 0x605015ff, 0x1998fb24, 0xd6bde997, 0x894043cc, 0x67d99e77, 0xb0e842bd, 0x07898b88, 0xe7195b38, 0x79c8eedb, 0xa17c0a47, 0x7c420fe9, 0xf8841ec9, 0x00000000, 0x09808683, 0x322bed48, 0x1e1170ac, 0x6c5a724e, 0xfd0efffb, 0x0f853856, 0x3daed51e, 0x362d3927, 0x0a0fd964, 0x685ca621, 0x9b5b54d1, 0x24362e3a, 0x0c0a67b1, 0x9357e70f, 0xb4ee96d2, 0x1b9b919e, 0x80c0c54f, 0x61dc20a2, 0x5a774b69, 0x1c121a16, 0xe293ba0a, 0xc0a02ae5, 0x3c22e043, 0x121b171d, 0x0e090d0b, 0xf28bc7ad, 0x2db6a8b9, 0x141ea9c8, 0x57f11985, 0xaf75074c, 0xee99ddbb, 0xa37f60fd, 0xf701269f, 0x5c72f5bc, 0x44663bc5, 0x5bfb7e34, 0x8b432976, 0xcb23c6dc, 0xb6edfc68, 0xb8e4f163, 0xd731dcca, 0x42638510, 0x13972240, 0x84c61120, 0x854a247d, 0xd2bb3df8, 0xaef93211, 0xc729a16d, 0x1d9e2f4b, 0xdcb230f3, 0x0d8652ec, 0x77c1e3d0, 0x2bb3166c, 0xa970b999, 0x119448fa, 0x47e96422, 0xa8fc8cc4, 0xa0f03f1a, 0x567d2cd8, 0x223390ef, 0x87494ec7, 0xd938d1c1, 0x8ccaa2fe, 0x98d40b36, 0xa6f581cf, 0xa57ade28, 0xdab78e26, 0x3fadbfa4, 0x2c3a9de4, 0x5078920d, 0x6a5fcc9b, 0x547e4662, 0xf68d13c2, 0x90d8b8e8, 0x2e39f75e, 0x82c3aff5, 0x9f5d80be, 0x69d0937c, 0x6fd52da9, 0xcf2512b3, 0xc8ac993b, 0x10187da7, 0xe89c636e, 0xdb3bbb7b, 0xcd267809, 0x6e5918f4, 0xec9ab701, 0x834f9aa8, 0xe6956e65, 0xaaffe67e, 0x21bccf08, 0xef15e8e6, 0xbae79bd9, 0x4a6f36ce, 0xea9f09d4, 0x29b07cd6, 0x31a4b2af, 0x2a3f2331, 0xc6a59430, 0x35a266c0, 0x744ebc37, 0xfc82caa6, 0xe090d0b0, 0x33a7d815, 0xf104984a, 0x41ecdaf7, 0x7fcd500e, 0x1791f62f, 0x764dd68d, 0x43efb04d, 0xccaa4d54, 0xe49604df, 0x9ed1b5e3, 0x4c6a881b, 0xc12c1fb8, 0x4665517f, 0x9d5eea04, 0x018c355d, 0xfa877473, 0xfb0b412e, 0xb3671d5a, 0x92dbd252, 0xe9105633, 0x6dd64713, 0x9ad7618c, 0x37a10c7a, 0x59f8148e, 0xeb133c89, 0xcea927ee, 0xb761c935, 0xe11ce5ed, 0x7a47b13c, 0x9cd2df59, 0x55f2733f, 0x1814ce79, 0x73c737bf, 0x53f7cdea, 0x5ffdaa5b, 0xdf3d6f14, 0x7844db86, 0xcaaff381, 0xb968c43e, 0x3824342c, 0xc2a3405f, 0x161dc372, 0xbce2250c, 0x283c498b, 0xff0d9541, 0x39a80171, 0x080cb3de, 0xd8b4e49c, 0x6456c190, 0x7bcb8461, 0xd532b670, 0x486c5c74, 0xd0b85742];
    var T6 = [0x5051f4a7, 0x537e4165, 0xc31a17a4, 0x963a275e, 0xcb3bab6b, 0xf11f9d45, 0xabacfa58, 0x934be303, 0x552030fa, 0xf6ad766d, 0x9188cc76, 0x25f5024c, 0xfc4fe5d7, 0xd7c52acb, 0x80263544, 0x8fb562a3, 0x49deb15a, 0x6725ba1b, 0x9845ea0e, 0xe15dfec0, 0x02c32f75, 0x12814cf0, 0xa38d4697, 0xc66bd3f9, 0xe7038f5f, 0x9515929c, 0xebbf6d7a, 0xda955259, 0x2dd4be83, 0xd3587421, 0x2949e069, 0x448ec9c8, 0x6a75c289, 0x78f48e79, 0x6b99583e, 0xdd27b971, 0xb6bee14f, 0x17f088ad, 0x66c920ac, 0xb47dce3a, 0x1863df4a, 0x82e51a31, 0x60975133, 0x4562537f, 0xe0b16477, 0x84bb6bae, 0x1cfe81a0, 0x94f9082b, 0x58704868, 0x198f45fd, 0x8794de6c, 0xb7527bf8, 0x23ab73d3, 0xe2724b02, 0x57e31f8f, 0x2a6655ab, 0x07b2eb28, 0x032fb5c2, 0x9a86c57b, 0xa5d33708, 0xf2302887, 0xb223bfa5, 0xba02036a, 0x5ced1682, 0x2b8acf1c, 0x92a779b4, 0xf0f307f2, 0xa14e69e2, 0xcd65daf4, 0xd50605be, 0x1fd13462, 0x8ac4a6fe, 0x9d342e53, 0xa0a2f355, 0x32058ae1, 0x75a4f6eb, 0x390b83ec, 0xaa4060ef, 0x065e719f, 0x51bd6e10, 0xf93e218a, 0x3d96dd06, 0xaedd3e05, 0x464de6bd, 0xb591548d, 0x0571c45d, 0x6f0406d4, 0xff605015, 0x241998fb, 0x97d6bde9, 0xcc894043, 0x7767d99e, 0xbdb0e842, 0x8807898b, 0x38e7195b, 0xdb79c8ee, 0x47a17c0a, 0xe97c420f, 0xc9f8841e, 0x00000000, 0x83098086, 0x48322bed, 0xac1e1170, 0x4e6c5a72, 0xfbfd0eff, 0x560f8538, 0x1e3daed5, 0x27362d39, 0x640a0fd9, 0x21685ca6, 0xd19b5b54, 0x3a24362e, 0xb10c0a67, 0x0f9357e7, 0xd2b4ee96, 0x9e1b9b91, 0x4f80c0c5, 0xa261dc20, 0x695a774b, 0x161c121a, 0x0ae293ba, 0xe5c0a02a, 0x433c22e0, 0x1d121b17, 0x0b0e090d, 0xadf28bc7, 0xb92db6a8, 0xc8141ea9, 0x8557f119, 0x4caf7507, 0xbbee99dd, 0xfda37f60, 0x9ff70126, 0xbc5c72f5, 0xc544663b, 0x345bfb7e, 0x768b4329, 0xdccb23c6, 0x68b6edfc, 0x63b8e4f1, 0xcad731dc, 0x10426385, 0x40139722, 0x2084c611, 0x7d854a24, 0xf8d2bb3d, 0x11aef932, 0x6dc729a1, 0x4b1d9e2f, 0xf3dcb230, 0xec0d8652, 0xd077c1e3, 0x6c2bb316, 0x99a970b9, 0xfa119448, 0x2247e964, 0xc4a8fc8c, 0x1aa0f03f, 0xd8567d2c, 0xef223390, 0xc787494e, 0xc1d938d1, 0xfe8ccaa2, 0x3698d40b, 0xcfa6f581, 0x28a57ade, 0x26dab78e, 0xa43fadbf, 0xe42c3a9d, 0x0d507892, 0x9b6a5fcc, 0x62547e46, 0xc2f68d13, 0xe890d8b8, 0x5e2e39f7, 0xf582c3af, 0xbe9f5d80, 0x7c69d093, 0xa96fd52d, 0xb3cf2512, 0x3bc8ac99, 0xa710187d, 0x6ee89c63, 0x7bdb3bbb, 0x09cd2678, 0xf46e5918, 0x01ec9ab7, 0xa8834f9a, 0x65e6956e, 0x7eaaffe6, 0x0821bccf, 0xe6ef15e8, 0xd9bae79b, 0xce4a6f36, 0xd4ea9f09, 0xd629b07c, 0xaf31a4b2, 0x312a3f23, 0x30c6a594, 0xc035a266, 0x37744ebc, 0xa6fc82ca, 0xb0e090d0, 0x1533a7d8, 0x4af10498, 0xf741ecda, 0x0e7fcd50, 0x2f1791f6, 0x8d764dd6, 0x4d43efb0, 0x54ccaa4d, 0xdfe49604, 0xe39ed1b5, 0x1b4c6a88, 0xb8c12c1f, 0x7f466551, 0x049d5eea, 0x5d018c35, 0x73fa8774, 0x2efb0b41, 0x5ab3671d, 0x5292dbd2, 0x33e91056, 0x136dd647, 0x8c9ad761, 0x7a37a10c, 0x8e59f814, 0x89eb133c, 0xeecea927, 0x35b761c9, 0xede11ce5, 0x3c7a47b1, 0x599cd2df, 0x3f55f273, 0x791814ce, 0xbf73c737, 0xea53f7cd, 0x5b5ffdaa, 0x14df3d6f, 0x867844db, 0x81caaff3, 0x3eb968c4, 0x2c382434, 0x5fc2a340, 0x72161dc3, 0x0cbce225, 0x8b283c49, 0x41ff0d95, 0x7139a801, 0xde080cb3, 0x9cd8b4e4, 0x906456c1, 0x617bcb84, 0x70d532b6, 0x74486c5c, 0x42d0b857];
    var T7 = [0xa75051f4, 0x65537e41, 0xa4c31a17, 0x5e963a27, 0x6bcb3bab, 0x45f11f9d, 0x58abacfa, 0x03934be3, 0xfa552030, 0x6df6ad76, 0x769188cc, 0x4c25f502, 0xd7fc4fe5, 0xcbd7c52a, 0x44802635, 0xa38fb562, 0x5a49deb1, 0x1b6725ba, 0x0e9845ea, 0xc0e15dfe, 0x7502c32f, 0xf012814c, 0x97a38d46, 0xf9c66bd3, 0x5fe7038f, 0x9c951592, 0x7aebbf6d, 0x59da9552, 0x832dd4be, 0x21d35874, 0x692949e0, 0xc8448ec9, 0x896a75c2, 0x7978f48e, 0x3e6b9958, 0x71dd27b9, 0x4fb6bee1, 0xad17f088, 0xac66c920, 0x3ab47dce, 0x4a1863df, 0x3182e51a, 0x33609751, 0x7f456253, 0x77e0b164, 0xae84bb6b, 0xa01cfe81, 0x2b94f908, 0x68587048, 0xfd198f45, 0x6c8794de, 0xf8b7527b, 0xd323ab73, 0x02e2724b, 0x8f57e31f, 0xab2a6655, 0x2807b2eb, 0xc2032fb5, 0x7b9a86c5, 0x08a5d337, 0x87f23028, 0xa5b223bf, 0x6aba0203, 0x825ced16, 0x1c2b8acf, 0xb492a779, 0xf2f0f307, 0xe2a14e69, 0xf4cd65da, 0xbed50605, 0x621fd134, 0xfe8ac4a6, 0x539d342e, 0x55a0a2f3, 0xe132058a, 0xeb75a4f6, 0xec390b83, 0xefaa4060, 0x9f065e71, 0x1051bd6e, 0x8af93e21, 0x063d96dd, 0x05aedd3e, 0xbd464de6, 0x8db59154, 0x5d0571c4, 0xd46f0406, 0x15ff6050, 0xfb241998, 0xe997d6bd, 0x43cc8940, 0x9e7767d9, 0x42bdb0e8, 0x8b880789, 0x5b38e719, 0xeedb79c8, 0x0a47a17c, 0x0fe97c42, 0x1ec9f884, 0x00000000, 0x86830980, 0xed48322b, 0x70ac1e11, 0x724e6c5a, 0xfffbfd0e, 0x38560f85, 0xd51e3dae, 0x3927362d, 0xd9640a0f, 0xa621685c, 0x54d19b5b, 0x2e3a2436, 0x67b10c0a, 0xe70f9357, 0x96d2b4ee, 0x919e1b9b, 0xc54f80c0, 0x20a261dc, 0x4b695a77, 0x1a161c12, 0xba0ae293, 0x2ae5c0a0, 0xe0433c22, 0x171d121b, 0x0d0b0e09, 0xc7adf28b, 0xa8b92db6, 0xa9c8141e, 0x198557f1, 0x074caf75, 0xddbbee99, 0x60fda37f, 0x269ff701, 0xf5bc5c72, 0x3bc54466, 0x7e345bfb, 0x29768b43, 0xc6dccb23, 0xfc68b6ed, 0xf163b8e4, 0xdccad731, 0x85104263, 0x22401397, 0x112084c6, 0x247d854a, 0x3df8d2bb, 0x3211aef9, 0xa16dc729, 0x2f4b1d9e, 0x30f3dcb2, 0x52ec0d86, 0xe3d077c1, 0x166c2bb3, 0xb999a970, 0x48fa1194, 0x642247e9, 0x8cc4a8fc, 0x3f1aa0f0, 0x2cd8567d, 0x90ef2233, 0x4ec78749, 0xd1c1d938, 0xa2fe8cca, 0x0b3698d4, 0x81cfa6f5, 0xde28a57a, 0x8e26dab7, 0xbfa43fad, 0x9de42c3a, 0x920d5078, 0xcc9b6a5f, 0x4662547e, 0x13c2f68d, 0xb8e890d8, 0xf75e2e39, 0xaff582c3, 0x80be9f5d, 0x937c69d0, 0x2da96fd5, 0x12b3cf25, 0x993bc8ac, 0x7da71018, 0x636ee89c, 0xbb7bdb3b, 0x7809cd26, 0x18f46e59, 0xb701ec9a, 0x9aa8834f, 0x6e65e695, 0xe67eaaff, 0xcf0821bc, 0xe8e6ef15, 0x9bd9bae7, 0x36ce4a6f, 0x09d4ea9f, 0x7cd629b0, 0xb2af31a4, 0x23312a3f, 0x9430c6a5, 0x66c035a2, 0xbc37744e, 0xcaa6fc82, 0xd0b0e090, 0xd81533a7, 0x984af104, 0xdaf741ec, 0x500e7fcd, 0xf62f1791, 0xd68d764d, 0xb04d43ef, 0x4d54ccaa, 0x04dfe496, 0xb5e39ed1, 0x881b4c6a, 0x1fb8c12c, 0x517f4665, 0xea049d5e, 0x355d018c, 0x7473fa87, 0x412efb0b, 0x1d5ab367, 0xd25292db, 0x5633e910, 0x47136dd6, 0x618c9ad7, 0x0c7a37a1, 0x148e59f8, 0x3c89eb13, 0x27eecea9, 0xc935b761, 0xe5ede11c, 0xb13c7a47, 0xdf599cd2, 0x733f55f2, 0xce791814, 0x37bf73c7, 0xcdea53f7, 0xaa5b5ffd, 0x6f14df3d, 0xdb867844, 0xf381caaf, 0xc43eb968, 0x342c3824, 0x405fc2a3, 0xc372161d, 0x250cbce2, 0x498b283c, 0x9541ff0d, 0x017139a8, 0xb3de080c, 0xe49cd8b4, 0xc1906456, 0x84617bcb, 0xb670d532, 0x5c74486c, 0x5742d0b8];
    var T8 = [0xf4a75051, 0x4165537e, 0x17a4c31a, 0x275e963a, 0xab6bcb3b, 0x9d45f11f, 0xfa58abac, 0xe303934b, 0x30fa5520, 0x766df6ad, 0xcc769188, 0x024c25f5, 0xe5d7fc4f, 0x2acbd7c5, 0x35448026, 0x62a38fb5, 0xb15a49de, 0xba1b6725, 0xea0e9845, 0xfec0e15d, 0x2f7502c3, 0x4cf01281, 0x4697a38d, 0xd3f9c66b, 0x8f5fe703, 0x929c9515, 0x6d7aebbf, 0x5259da95, 0xbe832dd4, 0x7421d358, 0xe0692949, 0xc9c8448e, 0xc2896a75, 0x8e7978f4, 0x583e6b99, 0xb971dd27, 0xe14fb6be, 0x88ad17f0, 0x20ac66c9, 0xce3ab47d, 0xdf4a1863, 0x1a3182e5, 0x51336097, 0x537f4562, 0x6477e0b1, 0x6bae84bb, 0x81a01cfe, 0x082b94f9, 0x48685870, 0x45fd198f, 0xde6c8794, 0x7bf8b752, 0x73d323ab, 0x4b02e272, 0x1f8f57e3, 0x55ab2a66, 0xeb2807b2, 0xb5c2032f, 0xc57b9a86, 0x3708a5d3, 0x2887f230, 0xbfa5b223, 0x036aba02, 0x16825ced, 0xcf1c2b8a, 0x79b492a7, 0x07f2f0f3, 0x69e2a14e, 0xdaf4cd65, 0x05bed506, 0x34621fd1, 0xa6fe8ac4, 0x2e539d34, 0xf355a0a2, 0x8ae13205, 0xf6eb75a4, 0x83ec390b, 0x60efaa40, 0x719f065e, 0x6e1051bd, 0x218af93e, 0xdd063d96, 0x3e05aedd, 0xe6bd464d, 0x548db591, 0xc45d0571, 0x06d46f04, 0x5015ff60, 0x98fb2419, 0xbde997d6, 0x4043cc89, 0xd99e7767, 0xe842bdb0, 0x898b8807, 0x195b38e7, 0xc8eedb79, 0x7c0a47a1, 0x420fe97c, 0x841ec9f8, 0x00000000, 0x80868309, 0x2bed4832, 0x1170ac1e, 0x5a724e6c, 0x0efffbfd, 0x8538560f, 0xaed51e3d, 0x2d392736, 0x0fd9640a, 0x5ca62168, 0x5b54d19b, 0x362e3a24, 0x0a67b10c, 0x57e70f93, 0xee96d2b4, 0x9b919e1b, 0xc0c54f80, 0xdc20a261, 0x774b695a, 0x121a161c, 0x93ba0ae2, 0xa02ae5c0, 0x22e0433c, 0x1b171d12, 0x090d0b0e, 0x8bc7adf2, 0xb6a8b92d, 0x1ea9c814, 0xf1198557, 0x75074caf, 0x99ddbbee, 0x7f60fda3, 0x01269ff7, 0x72f5bc5c, 0x663bc544, 0xfb7e345b, 0x4329768b, 0x23c6dccb, 0xedfc68b6, 0xe4f163b8, 0x31dccad7, 0x63851042, 0x97224013, 0xc6112084, 0x4a247d85, 0xbb3df8d2, 0xf93211ae, 0x29a16dc7, 0x9e2f4b1d, 0xb230f3dc, 0x8652ec0d, 0xc1e3d077, 0xb3166c2b, 0x70b999a9, 0x9448fa11, 0xe9642247, 0xfc8cc4a8, 0xf03f1aa0, 0x7d2cd856, 0x3390ef22, 0x494ec787, 0x38d1c1d9, 0xcaa2fe8c, 0xd40b3698, 0xf581cfa6, 0x7ade28a5, 0xb78e26da, 0xadbfa43f, 0x3a9de42c, 0x78920d50, 0x5fcc9b6a, 0x7e466254, 0x8d13c2f6, 0xd8b8e890, 0x39f75e2e, 0xc3aff582, 0x5d80be9f, 0xd0937c69, 0xd52da96f, 0x2512b3cf, 0xac993bc8, 0x187da710, 0x9c636ee8, 0x3bbb7bdb, 0x267809cd, 0x5918f46e, 0x9ab701ec, 0x4f9aa883, 0x956e65e6, 0xffe67eaa, 0xbccf0821, 0x15e8e6ef, 0xe79bd9ba, 0x6f36ce4a, 0x9f09d4ea, 0xb07cd629, 0xa4b2af31, 0x3f23312a, 0xa59430c6, 0xa266c035, 0x4ebc3774, 0x82caa6fc, 0x90d0b0e0, 0xa7d81533, 0x04984af1, 0xecdaf741, 0xcd500e7f, 0x91f62f17, 0x4dd68d76, 0xefb04d43, 0xaa4d54cc, 0x9604dfe4, 0xd1b5e39e, 0x6a881b4c, 0x2c1fb8c1, 0x65517f46, 0x5eea049d, 0x8c355d01, 0x877473fa, 0x0b412efb, 0x671d5ab3, 0xdbd25292, 0x105633e9, 0xd647136d, 0xd7618c9a, 0xa10c7a37, 0xf8148e59, 0x133c89eb, 0xa927eece, 0x61c935b7, 0x1ce5ede1, 0x47b13c7a, 0xd2df599c, 0xf2733f55, 0x14ce7918, 0xc737bf73, 0xf7cdea53, 0xfdaa5b5f, 0x3d6f14df, 0x44db8678, 0xaff381ca, 0x68c43eb9, 0x24342c38, 0xa3405fc2, 0x1dc37216, 0xe2250cbc, 0x3c498b28, 0x0d9541ff, 0xa8017139, 0x0cb3de08, 0xb4e49cd8, 0x56c19064, 0xcb84617b, 0x32b670d5, 0x6c5c7448, 0xb85742d0];

    // Transformations for decryption key expansion
    var U1 = [0x00000000, 0x0e090d0b, 0x1c121a16, 0x121b171d, 0x3824342c, 0x362d3927, 0x24362e3a, 0x2a3f2331, 0x70486858, 0x7e416553, 0x6c5a724e, 0x62537f45, 0x486c5c74, 0x4665517f, 0x547e4662, 0x5a774b69, 0xe090d0b0, 0xee99ddbb, 0xfc82caa6, 0xf28bc7ad, 0xd8b4e49c, 0xd6bde997, 0xc4a6fe8a, 0xcaaff381, 0x90d8b8e8, 0x9ed1b5e3, 0x8ccaa2fe, 0x82c3aff5, 0xa8fc8cc4, 0xa6f581cf, 0xb4ee96d2, 0xbae79bd9, 0xdb3bbb7b, 0xd532b670, 0xc729a16d, 0xc920ac66, 0xe31f8f57, 0xed16825c, 0xff0d9541, 0xf104984a, 0xab73d323, 0xa57ade28, 0xb761c935, 0xb968c43e, 0x9357e70f, 0x9d5eea04, 0x8f45fd19, 0x814cf012, 0x3bab6bcb, 0x35a266c0, 0x27b971dd, 0x29b07cd6, 0x038f5fe7, 0x0d8652ec, 0x1f9d45f1, 0x119448fa, 0x4be30393, 0x45ea0e98, 0x57f11985, 0x59f8148e, 0x73c737bf, 0x7dce3ab4, 0x6fd52da9, 0x61dc20a2, 0xad766df6, 0xa37f60fd, 0xb16477e0, 0xbf6d7aeb, 0x955259da, 0x9b5b54d1, 0x894043cc, 0x87494ec7, 0xdd3e05ae, 0xd33708a5, 0xc12c1fb8, 0xcf2512b3, 0xe51a3182, 0xeb133c89, 0xf9082b94, 0xf701269f, 0x4de6bd46, 0x43efb04d, 0x51f4a750, 0x5ffdaa5b, 0x75c2896a, 0x7bcb8461, 0x69d0937c, 0x67d99e77, 0x3daed51e, 0x33a7d815, 0x21bccf08, 0x2fb5c203, 0x058ae132, 0x0b83ec39, 0x1998fb24, 0x1791f62f, 0x764dd68d, 0x7844db86, 0x6a5fcc9b, 0x6456c190, 0x4e69e2a1, 0x4060efaa, 0x527bf8b7, 0x5c72f5bc, 0x0605bed5, 0x080cb3de, 0x1a17a4c3, 0x141ea9c8, 0x3e218af9, 0x302887f2, 0x223390ef, 0x2c3a9de4, 0x96dd063d, 0x98d40b36, 0x8acf1c2b, 0x84c61120, 0xaef93211, 0xa0f03f1a, 0xb2eb2807, 0xbce2250c, 0xe6956e65, 0xe89c636e, 0xfa877473, 0xf48e7978, 0xdeb15a49, 0xd0b85742, 0xc2a3405f, 0xccaa4d54, 0x41ecdaf7, 0x4fe5d7fc, 0x5dfec0e1, 0x53f7cdea, 0x79c8eedb, 0x77c1e3d0, 0x65daf4cd, 0x6bd3f9c6, 0x31a4b2af, 0x3fadbfa4, 0x2db6a8b9, 0x23bfa5b2, 0x09808683, 0x07898b88, 0x15929c95, 0x1b9b919e, 0xa17c0a47, 0xaf75074c, 0xbd6e1051, 0xb3671d5a, 0x99583e6b, 0x97513360, 0x854a247d, 0x8b432976, 0xd134621f, 0xdf3d6f14, 0xcd267809, 0xc32f7502, 0xe9105633, 0xe7195b38, 0xf5024c25, 0xfb0b412e, 0x9ad7618c, 0x94de6c87, 0x86c57b9a, 0x88cc7691, 0xa2f355a0, 0xacfa58ab, 0xbee14fb6, 0xb0e842bd, 0xea9f09d4, 0xe49604df, 0xf68d13c2, 0xf8841ec9, 0xd2bb3df8, 0xdcb230f3, 0xcea927ee, 0xc0a02ae5, 0x7a47b13c, 0x744ebc37, 0x6655ab2a, 0x685ca621, 0x42638510, 0x4c6a881b, 0x5e719f06, 0x5078920d, 0x0a0fd964, 0x0406d46f, 0x161dc372, 0x1814ce79, 0x322bed48, 0x3c22e043, 0x2e39f75e, 0x2030fa55, 0xec9ab701, 0xe293ba0a, 0xf088ad17, 0xfe81a01c, 0xd4be832d, 0xdab78e26, 0xc8ac993b, 0xc6a59430, 0x9cd2df59, 0x92dbd252, 0x80c0c54f, 0x8ec9c844, 0xa4f6eb75, 0xaaffe67e, 0xb8e4f163, 0xb6edfc68, 0x0c0a67b1, 0x02036aba, 0x10187da7, 0x1e1170ac, 0x342e539d, 0x3a275e96, 0x283c498b, 0x26354480, 0x7c420fe9, 0x724b02e2, 0x605015ff, 0x6e5918f4, 0x44663bc5, 0x4a6f36ce, 0x587421d3, 0x567d2cd8, 0x37a10c7a, 0x39a80171, 0x2bb3166c, 0x25ba1b67, 0x0f853856, 0x018c355d, 0x13972240, 0x1d9e2f4b, 0x47e96422, 0x49e06929, 0x5bfb7e34, 0x55f2733f, 0x7fcd500e, 0x71c45d05, 0x63df4a18, 0x6dd64713, 0xd731dcca, 0xd938d1c1, 0xcb23c6dc, 0xc52acbd7, 0xef15e8e6, 0xe11ce5ed, 0xf307f2f0, 0xfd0efffb, 0xa779b492, 0xa970b999, 0xbb6bae84, 0xb562a38f, 0x9f5d80be, 0x91548db5, 0x834f9aa8, 0x8d4697a3];
    var U2 = [0x00000000, 0x0b0e090d, 0x161c121a, 0x1d121b17, 0x2c382434, 0x27362d39, 0x3a24362e, 0x312a3f23, 0x58704868, 0x537e4165, 0x4e6c5a72, 0x4562537f, 0x74486c5c, 0x7f466551, 0x62547e46, 0x695a774b, 0xb0e090d0, 0xbbee99dd, 0xa6fc82ca, 0xadf28bc7, 0x9cd8b4e4, 0x97d6bde9, 0x8ac4a6fe, 0x81caaff3, 0xe890d8b8, 0xe39ed1b5, 0xfe8ccaa2, 0xf582c3af, 0xc4a8fc8c, 0xcfa6f581, 0xd2b4ee96, 0xd9bae79b, 0x7bdb3bbb, 0x70d532b6, 0x6dc729a1, 0x66c920ac, 0x57e31f8f, 0x5ced1682, 0x41ff0d95, 0x4af10498, 0x23ab73d3, 0x28a57ade, 0x35b761c9, 0x3eb968c4, 0x0f9357e7, 0x049d5eea, 0x198f45fd, 0x12814cf0, 0xcb3bab6b, 0xc035a266, 0xdd27b971, 0xd629b07c, 0xe7038f5f, 0xec0d8652, 0xf11f9d45, 0xfa119448, 0x934be303, 0x9845ea0e, 0x8557f119, 0x8e59f814, 0xbf73c737, 0xb47dce3a, 0xa96fd52d, 0xa261dc20, 0xf6ad766d, 0xfda37f60, 0xe0b16477, 0xebbf6d7a, 0xda955259, 0xd19b5b54, 0xcc894043, 0xc787494e, 0xaedd3e05, 0xa5d33708, 0xb8c12c1f, 0xb3cf2512, 0x82e51a31, 0x89eb133c, 0x94f9082b, 0x9ff70126, 0x464de6bd, 0x4d43efb0, 0x5051f4a7, 0x5b5ffdaa, 0x6a75c289, 0x617bcb84, 0x7c69d093, 0x7767d99e, 0x1e3daed5, 0x1533a7d8, 0x0821bccf, 0x032fb5c2, 0x32058ae1, 0x390b83ec, 0x241998fb, 0x2f1791f6, 0x8d764dd6, 0x867844db, 0x9b6a5fcc, 0x906456c1, 0xa14e69e2, 0xaa4060ef, 0xb7527bf8, 0xbc5c72f5, 0xd50605be, 0xde080cb3, 0xc31a17a4, 0xc8141ea9, 0xf93e218a, 0xf2302887, 0xef223390, 0xe42c3a9d, 0x3d96dd06, 0x3698d40b, 0x2b8acf1c, 0x2084c611, 0x11aef932, 0x1aa0f03f, 0x07b2eb28, 0x0cbce225, 0x65e6956e, 0x6ee89c63, 0x73fa8774, 0x78f48e79, 0x49deb15a, 0x42d0b857, 0x5fc2a340, 0x54ccaa4d, 0xf741ecda, 0xfc4fe5d7, 0xe15dfec0, 0xea53f7cd, 0xdb79c8ee, 0xd077c1e3, 0xcd65daf4, 0xc66bd3f9, 0xaf31a4b2, 0xa43fadbf, 0xb92db6a8, 0xb223bfa5, 0x83098086, 0x8807898b, 0x9515929c, 0x9e1b9b91, 0x47a17c0a, 0x4caf7507, 0x51bd6e10, 0x5ab3671d, 0x6b99583e, 0x60975133, 0x7d854a24, 0x768b4329, 0x1fd13462, 0x14df3d6f, 0x09cd2678, 0x02c32f75, 0x33e91056, 0x38e7195b, 0x25f5024c, 0x2efb0b41, 0x8c9ad761, 0x8794de6c, 0x9a86c57b, 0x9188cc76, 0xa0a2f355, 0xabacfa58, 0xb6bee14f, 0xbdb0e842, 0xd4ea9f09, 0xdfe49604, 0xc2f68d13, 0xc9f8841e, 0xf8d2bb3d, 0xf3dcb230, 0xeecea927, 0xe5c0a02a, 0x3c7a47b1, 0x37744ebc, 0x2a6655ab, 0x21685ca6, 0x10426385, 0x1b4c6a88, 0x065e719f, 0x0d507892, 0x640a0fd9, 0x6f0406d4, 0x72161dc3, 0x791814ce, 0x48322bed, 0x433c22e0, 0x5e2e39f7, 0x552030fa, 0x01ec9ab7, 0x0ae293ba, 0x17f088ad, 0x1cfe81a0, 0x2dd4be83, 0x26dab78e, 0x3bc8ac99, 0x30c6a594, 0x599cd2df, 0x5292dbd2, 0x4f80c0c5, 0x448ec9c8, 0x75a4f6eb, 0x7eaaffe6, 0x63b8e4f1, 0x68b6edfc, 0xb10c0a67, 0xba02036a, 0xa710187d, 0xac1e1170, 0x9d342e53, 0x963a275e, 0x8b283c49, 0x80263544, 0xe97c420f, 0xe2724b02, 0xff605015, 0xf46e5918, 0xc544663b, 0xce4a6f36, 0xd3587421, 0xd8567d2c, 0x7a37a10c, 0x7139a801, 0x6c2bb316, 0x6725ba1b, 0x560f8538, 0x5d018c35, 0x40139722, 0x4b1d9e2f, 0x2247e964, 0x2949e069, 0x345bfb7e, 0x3f55f273, 0x0e7fcd50, 0x0571c45d, 0x1863df4a, 0x136dd647, 0xcad731dc, 0xc1d938d1, 0xdccb23c6, 0xd7c52acb, 0xe6ef15e8, 0xede11ce5, 0xf0f307f2, 0xfbfd0eff, 0x92a779b4, 0x99a970b9, 0x84bb6bae, 0x8fb562a3, 0xbe9f5d80, 0xb591548d, 0xa8834f9a, 0xa38d4697];
    var U3 = [0x00000000, 0x0d0b0e09, 0x1a161c12, 0x171d121b, 0x342c3824, 0x3927362d, 0x2e3a2436, 0x23312a3f, 0x68587048, 0x65537e41, 0x724e6c5a, 0x7f456253, 0x5c74486c, 0x517f4665, 0x4662547e, 0x4b695a77, 0xd0b0e090, 0xddbbee99, 0xcaa6fc82, 0xc7adf28b, 0xe49cd8b4, 0xe997d6bd, 0xfe8ac4a6, 0xf381caaf, 0xb8e890d8, 0xb5e39ed1, 0xa2fe8cca, 0xaff582c3, 0x8cc4a8fc, 0x81cfa6f5, 0x96d2b4ee, 0x9bd9bae7, 0xbb7bdb3b, 0xb670d532, 0xa16dc729, 0xac66c920, 0x8f57e31f, 0x825ced16, 0x9541ff0d, 0x984af104, 0xd323ab73, 0xde28a57a, 0xc935b761, 0xc43eb968, 0xe70f9357, 0xea049d5e, 0xfd198f45, 0xf012814c, 0x6bcb3bab, 0x66c035a2, 0x71dd27b9, 0x7cd629b0, 0x5fe7038f, 0x52ec0d86, 0x45f11f9d, 0x48fa1194, 0x03934be3, 0x0e9845ea, 0x198557f1, 0x148e59f8, 0x37bf73c7, 0x3ab47dce, 0x2da96fd5, 0x20a261dc, 0x6df6ad76, 0x60fda37f, 0x77e0b164, 0x7aebbf6d, 0x59da9552, 0x54d19b5b, 0x43cc8940, 0x4ec78749, 0x05aedd3e, 0x08a5d337, 0x1fb8c12c, 0x12b3cf25, 0x3182e51a, 0x3c89eb13, 0x2b94f908, 0x269ff701, 0xbd464de6, 0xb04d43ef, 0xa75051f4, 0xaa5b5ffd, 0x896a75c2, 0x84617bcb, 0x937c69d0, 0x9e7767d9, 0xd51e3dae, 0xd81533a7, 0xcf0821bc, 0xc2032fb5, 0xe132058a, 0xec390b83, 0xfb241998, 0xf62f1791, 0xd68d764d, 0xdb867844, 0xcc9b6a5f, 0xc1906456, 0xe2a14e69, 0xefaa4060, 0xf8b7527b, 0xf5bc5c72, 0xbed50605, 0xb3de080c, 0xa4c31a17, 0xa9c8141e, 0x8af93e21, 0x87f23028, 0x90ef2233, 0x9de42c3a, 0x063d96dd, 0x0b3698d4, 0x1c2b8acf, 0x112084c6, 0x3211aef9, 0x3f1aa0f0, 0x2807b2eb, 0x250cbce2, 0x6e65e695, 0x636ee89c, 0x7473fa87, 0x7978f48e, 0x5a49deb1, 0x5742d0b8, 0x405fc2a3, 0x4d54ccaa, 0xdaf741ec, 0xd7fc4fe5, 0xc0e15dfe, 0xcdea53f7, 0xeedb79c8, 0xe3d077c1, 0xf4cd65da, 0xf9c66bd3, 0xb2af31a4, 0xbfa43fad, 0xa8b92db6, 0xa5b223bf, 0x86830980, 0x8b880789, 0x9c951592, 0x919e1b9b, 0x0a47a17c, 0x074caf75, 0x1051bd6e, 0x1d5ab367, 0x3e6b9958, 0x33609751, 0x247d854a, 0x29768b43, 0x621fd134, 0x6f14df3d, 0x7809cd26, 0x7502c32f, 0x5633e910, 0x5b38e719, 0x4c25f502, 0x412efb0b, 0x618c9ad7, 0x6c8794de, 0x7b9a86c5, 0x769188cc, 0x55a0a2f3, 0x58abacfa, 0x4fb6bee1, 0x42bdb0e8, 0x09d4ea9f, 0x04dfe496, 0x13c2f68d, 0x1ec9f884, 0x3df8d2bb, 0x30f3dcb2, 0x27eecea9, 0x2ae5c0a0, 0xb13c7a47, 0xbc37744e, 0xab2a6655, 0xa621685c, 0x85104263, 0x881b4c6a, 0x9f065e71, 0x920d5078, 0xd9640a0f, 0xd46f0406, 0xc372161d, 0xce791814, 0xed48322b, 0xe0433c22, 0xf75e2e39, 0xfa552030, 0xb701ec9a, 0xba0ae293, 0xad17f088, 0xa01cfe81, 0x832dd4be, 0x8e26dab7, 0x993bc8ac, 0x9430c6a5, 0xdf599cd2, 0xd25292db, 0xc54f80c0, 0xc8448ec9, 0xeb75a4f6, 0xe67eaaff, 0xf163b8e4, 0xfc68b6ed, 0x67b10c0a, 0x6aba0203, 0x7da71018, 0x70ac1e11, 0x539d342e, 0x5e963a27, 0x498b283c, 0x44802635, 0x0fe97c42, 0x02e2724b, 0x15ff6050, 0x18f46e59, 0x3bc54466, 0x36ce4a6f, 0x21d35874, 0x2cd8567d, 0x0c7a37a1, 0x017139a8, 0x166c2bb3, 0x1b6725ba, 0x38560f85, 0x355d018c, 0x22401397, 0x2f4b1d9e, 0x642247e9, 0x692949e0, 0x7e345bfb, 0x733f55f2, 0x500e7fcd, 0x5d0571c4, 0x4a1863df, 0x47136dd6, 0xdccad731, 0xd1c1d938, 0xc6dccb23, 0xcbd7c52a, 0xe8e6ef15, 0xe5ede11c, 0xf2f0f307, 0xfffbfd0e, 0xb492a779, 0xb999a970, 0xae84bb6b, 0xa38fb562, 0x80be9f5d, 0x8db59154, 0x9aa8834f, 0x97a38d46];
    var U4 = [0x00000000, 0x090d0b0e, 0x121a161c, 0x1b171d12, 0x24342c38, 0x2d392736, 0x362e3a24, 0x3f23312a, 0x48685870, 0x4165537e, 0x5a724e6c, 0x537f4562, 0x6c5c7448, 0x65517f46, 0x7e466254, 0x774b695a, 0x90d0b0e0, 0x99ddbbee, 0x82caa6fc, 0x8bc7adf2, 0xb4e49cd8, 0xbde997d6, 0xa6fe8ac4, 0xaff381ca, 0xd8b8e890, 0xd1b5e39e, 0xcaa2fe8c, 0xc3aff582, 0xfc8cc4a8, 0xf581cfa6, 0xee96d2b4, 0xe79bd9ba, 0x3bbb7bdb, 0x32b670d5, 0x29a16dc7, 0x20ac66c9, 0x1f8f57e3, 0x16825ced, 0x0d9541ff, 0x04984af1, 0x73d323ab, 0x7ade28a5, 0x61c935b7, 0x68c43eb9, 0x57e70f93, 0x5eea049d, 0x45fd198f, 0x4cf01281, 0xab6bcb3b, 0xa266c035, 0xb971dd27, 0xb07cd629, 0x8f5fe703, 0x8652ec0d, 0x9d45f11f, 0x9448fa11, 0xe303934b, 0xea0e9845, 0xf1198557, 0xf8148e59, 0xc737bf73, 0xce3ab47d, 0xd52da96f, 0xdc20a261, 0x766df6ad, 0x7f60fda3, 0x6477e0b1, 0x6d7aebbf, 0x5259da95, 0x5b54d19b, 0x4043cc89, 0x494ec787, 0x3e05aedd, 0x3708a5d3, 0x2c1fb8c1, 0x2512b3cf, 0x1a3182e5, 0x133c89eb, 0x082b94f9, 0x01269ff7, 0xe6bd464d, 0xefb04d43, 0xf4a75051, 0xfdaa5b5f, 0xc2896a75, 0xcb84617b, 0xd0937c69, 0xd99e7767, 0xaed51e3d, 0xa7d81533, 0xbccf0821, 0xb5c2032f, 0x8ae13205, 0x83ec390b, 0x98fb2419, 0x91f62f17, 0x4dd68d76, 0x44db8678, 0x5fcc9b6a, 0x56c19064, 0x69e2a14e, 0x60efaa40, 0x7bf8b752, 0x72f5bc5c, 0x05bed506, 0x0cb3de08, 0x17a4c31a, 0x1ea9c814, 0x218af93e, 0x2887f230, 0x3390ef22, 0x3a9de42c, 0xdd063d96, 0xd40b3698, 0xcf1c2b8a, 0xc6112084, 0xf93211ae, 0xf03f1aa0, 0xeb2807b2, 0xe2250cbc, 0x956e65e6, 0x9c636ee8, 0x877473fa, 0x8e7978f4, 0xb15a49de, 0xb85742d0, 0xa3405fc2, 0xaa4d54cc, 0xecdaf741, 0xe5d7fc4f, 0xfec0e15d, 0xf7cdea53, 0xc8eedb79, 0xc1e3d077, 0xdaf4cd65, 0xd3f9c66b, 0xa4b2af31, 0xadbfa43f, 0xb6a8b92d, 0xbfa5b223, 0x80868309, 0x898b8807, 0x929c9515, 0x9b919e1b, 0x7c0a47a1, 0x75074caf, 0x6e1051bd, 0x671d5ab3, 0x583e6b99, 0x51336097, 0x4a247d85, 0x4329768b, 0x34621fd1, 0x3d6f14df, 0x267809cd, 0x2f7502c3, 0x105633e9, 0x195b38e7, 0x024c25f5, 0x0b412efb, 0xd7618c9a, 0xde6c8794, 0xc57b9a86, 0xcc769188, 0xf355a0a2, 0xfa58abac, 0xe14fb6be, 0xe842bdb0, 0x9f09d4ea, 0x9604dfe4, 0x8d13c2f6, 0x841ec9f8, 0xbb3df8d2, 0xb230f3dc, 0xa927eece, 0xa02ae5c0, 0x47b13c7a, 0x4ebc3774, 0x55ab2a66, 0x5ca62168, 0x63851042, 0x6a881b4c, 0x719f065e, 0x78920d50, 0x0fd9640a, 0x06d46f04, 0x1dc37216, 0x14ce7918, 0x2bed4832, 0x22e0433c, 0x39f75e2e, 0x30fa5520, 0x9ab701ec, 0x93ba0ae2, 0x88ad17f0, 0x81a01cfe, 0xbe832dd4, 0xb78e26da, 0xac993bc8, 0xa59430c6, 0xd2df599c, 0xdbd25292, 0xc0c54f80, 0xc9c8448e, 0xf6eb75a4, 0xffe67eaa, 0xe4f163b8, 0xedfc68b6, 0x0a67b10c, 0x036aba02, 0x187da710, 0x1170ac1e, 0x2e539d34, 0x275e963a, 0x3c498b28, 0x35448026, 0x420fe97c, 0x4b02e272, 0x5015ff60, 0x5918f46e, 0x663bc544, 0x6f36ce4a, 0x7421d358, 0x7d2cd856, 0xa10c7a37, 0xa8017139, 0xb3166c2b, 0xba1b6725, 0x8538560f, 0x8c355d01, 0x97224013, 0x9e2f4b1d, 0xe9642247, 0xe0692949, 0xfb7e345b, 0xf2733f55, 0xcd500e7f, 0xc45d0571, 0xdf4a1863, 0xd647136d, 0x31dccad7, 0x38d1c1d9, 0x23c6dccb, 0x2acbd7c5, 0x15e8e6ef, 0x1ce5ede1, 0x07f2f0f3, 0x0efffbfd, 0x79b492a7, 0x70b999a9, 0x6bae84bb, 0x62a38fb5, 0x5d80be9f, 0x548db591, 0x4f9aa883, 0x4697a38d];

    function convertToInt32(bytes) {
        var result = [];
        for (var i = 0; i < bytes.length; i += 4) {
            result.push(
                (bytes[i    ] << 24) |
                (bytes[i + 1] << 16) |
                (bytes[i + 2] <<  8) |
                 bytes[i + 3]
            );
        }
        return result;
    }

    var AES = function(key) {
        if (!(this instanceof AES)) {
            throw Error('AES must be instanitated with `new`');
        }

        Object.defineProperty(this, 'key', {
            value: coerceArray(key, true)
        });

        this._prepare();
    }


    AES.prototype._prepare = function() {

        var rounds = numberOfRounds[this.key.length];
        if (rounds == null) {
            throw new Error('invalid key size (must be 16, 24 or 32 bytes)');
        }

        // encryption round keys
        this._Ke = [];

        // decryption round keys
        this._Kd = [];

        for (var i = 0; i <= rounds; i++) {
            this._Ke.push([0, 0, 0, 0]);
            this._Kd.push([0, 0, 0, 0]);
        }

        var roundKeyCount = (rounds + 1) * 4;
        var KC = this.key.length / 4;

        // convert the key into ints
        var tk = convertToInt32(this.key);

        // copy values into round key arrays
        var index;
        for (var i = 0; i < KC; i++) {
            index = i >> 2;
            this._Ke[index][i % 4] = tk[i];
            this._Kd[rounds - index][i % 4] = tk[i];
        }

        // key expansion (fips-197 section 5.2)
        var rconpointer = 0;
        var t = KC, tt;
        while (t < roundKeyCount) {
            tt = tk[KC - 1];
            tk[0] ^= ((S[(tt >> 16) & 0xFF] << 24) ^
                      (S[(tt >>  8) & 0xFF] << 16) ^
                      (S[ tt        & 0xFF] <<  8) ^
                       S[(tt >> 24) & 0xFF]        ^
                      (rcon[rconpointer] << 24));
            rconpointer += 1;

            // key expansion (for non-256 bit)
            if (KC != 8) {
                for (var i = 1; i < KC; i++) {
                    tk[i] ^= tk[i - 1];
                }

            // key expansion for 256-bit keys is "slightly different" (fips-197)
            } else {
                for (var i = 1; i < (KC / 2); i++) {
                    tk[i] ^= tk[i - 1];
                }
                tt = tk[(KC / 2) - 1];

                tk[KC / 2] ^= (S[ tt        & 0xFF]        ^
                              (S[(tt >>  8) & 0xFF] <<  8) ^
                              (S[(tt >> 16) & 0xFF] << 16) ^
                              (S[(tt >> 24) & 0xFF] << 24));

                for (var i = (KC / 2) + 1; i < KC; i++) {
                    tk[i] ^= tk[i - 1];
                }
            }

            // copy values into round key arrays
            var i = 0, r, c;
            while (i < KC && t < roundKeyCount) {
                r = t >> 2;
                c = t % 4;
                this._Ke[r][c] = tk[i];
                this._Kd[rounds - r][c] = tk[i++];
                t++;
            }
        }

        // inverse-cipher-ify the decryption round key (fips-197 section 5.3)
        for (var r = 1; r < rounds; r++) {
            for (var c = 0; c < 4; c++) {
                tt = this._Kd[r][c];
                this._Kd[r][c] = (U1[(tt >> 24) & 0xFF] ^
                                  U2[(tt >> 16) & 0xFF] ^
                                  U3[(tt >>  8) & 0xFF] ^
                                  U4[ tt        & 0xFF]);
            }
        }
    }

    AES.prototype.encrypt = function(plaintext) {
        if (plaintext.length != 16) {
            throw new Error('invalid plaintext size (must be 16 bytes)');
        }

        var rounds = this._Ke.length - 1;
        var a = [0, 0, 0, 0];

        // convert plaintext to (ints ^ key)
        var t = convertToInt32(plaintext);
        for (var i = 0; i < 4; i++) {
            t[i] ^= this._Ke[0][i];
        }

        // apply round transforms
        for (var r = 1; r < rounds; r++) {
            for (var i = 0; i < 4; i++) {
                a[i] = (T1[(t[ i         ] >> 24) & 0xff] ^
                        T2[(t[(i + 1) % 4] >> 16) & 0xff] ^
                        T3[(t[(i + 2) % 4] >>  8) & 0xff] ^
                        T4[ t[(i + 3) % 4]        & 0xff] ^
                        this._Ke[r][i]);
            }
            t = a.slice();
        }

        // the last round is special
        var result = createArray(16), tt;
        for (var i = 0; i < 4; i++) {
            tt = this._Ke[rounds][i];
            result[4 * i    ] = (S[(t[ i         ] >> 24) & 0xff] ^ (tt >> 24)) & 0xff;
            result[4 * i + 1] = (S[(t[(i + 1) % 4] >> 16) & 0xff] ^ (tt >> 16)) & 0xff;
            result[4 * i + 2] = (S[(t[(i + 2) % 4] >>  8) & 0xff] ^ (tt >>  8)) & 0xff;
            result[4 * i + 3] = (S[ t[(i + 3) % 4]        & 0xff] ^  tt       ) & 0xff;
        }

        return result;
    }

    AES.prototype.decrypt = function(ciphertext) {
        if (ciphertext.length != 16) {
            throw new Error('invalid ciphertext size (must be 16 bytes)');
        }

        var rounds = this._Kd.length - 1;
        var a = [0, 0, 0, 0];

        // convert plaintext to (ints ^ key)
        var t = convertToInt32(ciphertext);
        for (var i = 0; i < 4; i++) {
            t[i] ^= this._Kd[0][i];
        }

        // apply round transforms
        for (var r = 1; r < rounds; r++) {
            for (var i = 0; i < 4; i++) {
                a[i] = (T5[(t[ i          ] >> 24) & 0xff] ^
                        T6[(t[(i + 3) % 4] >> 16) & 0xff] ^
                        T7[(t[(i + 2) % 4] >>  8) & 0xff] ^
                        T8[ t[(i + 1) % 4]        & 0xff] ^
                        this._Kd[r][i]);
            }
            t = a.slice();
        }

        // the last round is special
        var result = createArray(16), tt;
        for (var i = 0; i < 4; i++) {
            tt = this._Kd[rounds][i];
            result[4 * i    ] = (Si[(t[ i         ] >> 24) & 0xff] ^ (tt >> 24)) & 0xff;
            result[4 * i + 1] = (Si[(t[(i + 3) % 4] >> 16) & 0xff] ^ (tt >> 16)) & 0xff;
            result[4 * i + 2] = (Si[(t[(i + 2) % 4] >>  8) & 0xff] ^ (tt >>  8)) & 0xff;
            result[4 * i + 3] = (Si[ t[(i + 1) % 4]        & 0xff] ^  tt       ) & 0xff;
        }

        return result;
    }


    /**
     *  Mode Of Operation - Electonic Codebook (ECB)
     */
    var ModeOfOperationECB = function(key) {
        if (!(this instanceof ModeOfOperationECB)) {
            throw Error('AES must be instanitated with `new`');
        }

        this.description = "Electronic Code Block";
        this.name = "ecb";

        this._aes = new AES(key);
    }

    ModeOfOperationECB.prototype.encrypt = function(plaintext) {
        plaintext = coerceArray(plaintext);

        if ((plaintext.length % 16) !== 0) {
            throw new Error('invalid plaintext size (must be multiple of 16 bytes)');
        }

        var ciphertext = createArray(plaintext.length);
        var block = createArray(16);

        for (var i = 0; i < plaintext.length; i += 16) {
            copyArray(plaintext, block, 0, i, i + 16);
            block = this._aes.encrypt(block);
            copyArray(block, ciphertext, i);
        }

        return ciphertext;
    }

    ModeOfOperationECB.prototype.decrypt = function(ciphertext) {
        ciphertext = coerceArray(ciphertext);

        if ((ciphertext.length % 16) !== 0) {
            throw new Error('invalid ciphertext size (must be multiple of 16 bytes)');
        }

        var plaintext = createArray(ciphertext.length);
        var block = createArray(16);

        for (var i = 0; i < ciphertext.length; i += 16) {
            copyArray(ciphertext, block, 0, i, i + 16);
            block = this._aes.decrypt(block);
            copyArray(block, plaintext, i);
        }

        return plaintext;
    }


    /**
     *  Mode Of Operation - Cipher Block Chaining (CBC)
     */
    var ModeOfOperationCBC = function(key, iv) {
        if (!(this instanceof ModeOfOperationCBC)) {
            throw Error('AES must be instanitated with `new`');
        }

        this.description = "Cipher Block Chaining";
        this.name = "cbc";

        if (!iv) {
            iv = createArray(16);

        } else if (iv.length != 16) {
            throw new Error('invalid initialation vector size (must be 16 bytes)');
        }

        this._lastCipherblock = coerceArray(iv, true);

        this._aes = new AES(key);
    }

    ModeOfOperationCBC.prototype.encrypt = function(plaintext) {
        plaintext = coerceArray(plaintext);

        if ((plaintext.length % 16) !== 0) {
            throw new Error('invalid plaintext size (must be multiple of 16 bytes)');
        }

        var ciphertext = createArray(plaintext.length);
        var block = createArray(16);

        for (var i = 0; i < plaintext.length; i += 16) {
            copyArray(plaintext, block, 0, i, i + 16);

            for (var j = 0; j < 16; j++) {
                block[j] ^= this._lastCipherblock[j];
            }

            this._lastCipherblock = this._aes.encrypt(block);
            copyArray(this._lastCipherblock, ciphertext, i);
        }

        return ciphertext;
    }

    ModeOfOperationCBC.prototype.decrypt = function(ciphertext) {
        ciphertext = coerceArray(ciphertext);

        if ((ciphertext.length % 16) !== 0) {
            throw new Error('invalid ciphertext size (must be multiple of 16 bytes)');
        }

        var plaintext = createArray(ciphertext.length);
        var block = createArray(16);

        for (var i = 0; i < ciphertext.length; i += 16) {
            copyArray(ciphertext, block, 0, i, i + 16);
            block = this._aes.decrypt(block);

            for (var j = 0; j < 16; j++) {
                plaintext[i + j] = block[j] ^ this._lastCipherblock[j];
            }

            copyArray(ciphertext, this._lastCipherblock, 0, i, i + 16);
        }

        return plaintext;
    }


    /**
     *  Mode Of Operation - Cipher Feedback (CFB)
     */
    var ModeOfOperationCFB = function(key, iv, segmentSize) {
        if (!(this instanceof ModeOfOperationCFB)) {
            throw Error('AES must be instanitated with `new`');
        }

        this.description = "Cipher Feedback";
        this.name = "cfb";

        if (!iv) {
            iv = createArray(16);

        } else if (iv.length != 16) {
            throw new Error('invalid initialation vector size (must be 16 size)');
        }

        if (!segmentSize) { segmentSize = 1; }

        this.segmentSize = segmentSize;

        this._shiftRegister = coerceArray(iv, true);

        this._aes = new AES(key);
    }

    ModeOfOperationCFB.prototype.encrypt = function(plaintext) {
        if ((plaintext.length % this.segmentSize) != 0) {
            throw new Error('invalid plaintext size (must be segmentSize bytes)');
        }

        var encrypted = coerceArray(plaintext, true);

        var xorSegment;
        for (var i = 0; i < encrypted.length; i += this.segmentSize) {
            xorSegment = this._aes.encrypt(this._shiftRegister);
            for (var j = 0; j < this.segmentSize; j++) {
                encrypted[i + j] ^= xorSegment[j];
            }

            // Shift the register
            copyArray(this._shiftRegister, this._shiftRegister, 0, this.segmentSize);
            copyArray(encrypted, this._shiftRegister, 16 - this.segmentSize, i, i + this.segmentSize);
        }

        return encrypted;
    }

    ModeOfOperationCFB.prototype.decrypt = function(ciphertext) {
        if ((ciphertext.length % this.segmentSize) != 0) {
            throw new Error('invalid ciphertext size (must be segmentSize bytes)');
        }

        var plaintext = coerceArray(ciphertext, true);

        var xorSegment;
        for (var i = 0; i < plaintext.length; i += this.segmentSize) {
            xorSegment = this._aes.encrypt(this._shiftRegister);

            for (var j = 0; j < this.segmentSize; j++) {
                plaintext[i + j] ^= xorSegment[j];
            }

            // Shift the register
            copyArray(this._shiftRegister, this._shiftRegister, 0, this.segmentSize);
            copyArray(ciphertext, this._shiftRegister, 16 - this.segmentSize, i, i + this.segmentSize);
        }

        return plaintext;
    }

    /**
     *  Mode Of Operation - Output Feedback (OFB)
     */
    var ModeOfOperationOFB = function(key, iv) {
        if (!(this instanceof ModeOfOperationOFB)) {
            throw Error('AES must be instanitated with `new`');
        }

        this.description = "Output Feedback";
        this.name = "ofb";

        if (!iv) {
            iv = createArray(16);

        } else if (iv.length != 16) {
            throw new Error('invalid initialation vector size (must be 16 bytes)');
        }

        this._lastPrecipher = coerceArray(iv, true);
        this._lastPrecipherIndex = 16;

        this._aes = new AES(key);
    }

    ModeOfOperationOFB.prototype.encrypt = function(plaintext) {
        var encrypted = coerceArray(plaintext, true);

        for (var i = 0; i < encrypted.length; i++) {
            if (this._lastPrecipherIndex === 16) {
                this._lastPrecipher = this._aes.encrypt(this._lastPrecipher);
                this._lastPrecipherIndex = 0;
            }
            encrypted[i] ^= this._lastPrecipher[this._lastPrecipherIndex++];
        }

        return encrypted;
    }

    // Decryption is symetric
    ModeOfOperationOFB.prototype.decrypt = ModeOfOperationOFB.prototype.encrypt;


    /**
     *  Counter object for CTR common mode of operation
     */
    var Counter = function(initialValue) {
        if (!(this instanceof Counter)) {
            throw Error('Counter must be instanitated with `new`');
        }

        // We allow 0, but anything false-ish uses the default 1
        if (initialValue !== 0 && !initialValue) { initialValue = 1; }

        if (typeof(initialValue) === 'number') {
            this._counter = createArray(16);
            this.setValue(initialValue);

        } else {
            this.setBytes(initialValue);
        }
    }

    Counter.prototype.setValue = function(value) {
        if (typeof(value) !== 'number' || parseInt(value) != value) {
            throw new Error('invalid counter value (must be an integer)');
        }

        // We cannot safely handle numbers beyond the safe range for integers
        if (value > Number.MAX_SAFE_INTEGER) {
            throw new Error('integer value out of safe range');
        }

        for (var index = 15; index >= 0; --index) {
            this._counter[index] = value % 256;
            value = parseInt(value / 256);
        }
    }

    Counter.prototype.setBytes = function(bytes) {
        bytes = coerceArray(bytes, true);

        if (bytes.length != 16) {
            throw new Error('invalid counter bytes size (must be 16 bytes)');
        }

        this._counter = bytes;
    };

    Counter.prototype.increment = function() {
        for (var i = 15; i >= 0; i--) {
            if (this._counter[i] === 255) {
                this._counter[i] = 0;
            } else {
                this._counter[i]++;
                break;
            }
        }
    }


    /**
     *  Mode Of Operation - Counter (CTR)
     */
    var ModeOfOperationCTR = function(key, counter) {
        if (!(this instanceof ModeOfOperationCTR)) {
            throw Error('AES must be instanitated with `new`');
        }

        this.description = "Counter";
        this.name = "ctr";

        if (!(counter instanceof Counter)) {
            counter = new Counter(counter)
        }

        this._counter = counter;

        this._remainingCounter = null;
        this._remainingCounterIndex = 16;

        this._aes = new AES(key);
    }

    ModeOfOperationCTR.prototype.encrypt = function(plaintext) {
        var encrypted = coerceArray(plaintext, true);

        for (var i = 0; i < encrypted.length; i++) {
            if (this._remainingCounterIndex === 16) {
                this._remainingCounter = this._aes.encrypt(this._counter._counter);
                this._remainingCounterIndex = 0;
                this._counter.increment();
            }
            encrypted[i] ^= this._remainingCounter[this._remainingCounterIndex++];
        }

        return encrypted;
    }

    // Decryption is symetric
    ModeOfOperationCTR.prototype.decrypt = ModeOfOperationCTR.prototype.encrypt;


    ///////////////////////
    // Padding

    // See:https://tools.ietf.org/html/rfc2315
    function pkcs7pad(data) {
        data = coerceArray(data, true);
        var padder = 16 - (data.length % 16);
        var result = createArray(data.length + padder);
        copyArray(data, result);
        for (var i = data.length; i < result.length; i++) {
            result[i] = padder;
        }
        return result;
    }

    function pkcs7strip(data) {
        data = coerceArray(data, true);
        if (data.length < 16) { throw new Error('PKCS#7 invalid length'); }

        var padder = data[data.length - 1];
        if (padder > 16) { throw new Error('PKCS#7 padding byte out of range'); }

        var length = data.length - padder;
        for (var i = 0; i < padder; i++) {
            if (data[length + i] !== padder) {
                throw new Error('PKCS#7 invalid padding byte');
            }
        }

        var result = createArray(length);
        copyArray(data, result, 0, 0, length);
        return result;
    }

    ///////////////////////
    // Exporting


    // The block cipher
    var aesjs = {
        AES: AES,
        Counter: Counter,

        ModeOfOperation: {
            ecb: ModeOfOperationECB,
            cbc: ModeOfOperationCBC,
            cfb: ModeOfOperationCFB,
            ofb: ModeOfOperationOFB,
            ctr: ModeOfOperationCTR
        },

        utils: {
            hex: convertHex,
            utf8: convertUtf8
        },

        padding: {
            pkcs7: {
                pad: pkcs7pad,
                strip: pkcs7strip
            }
        },

        _arrayTest: {
            coerceArray: coerceArray,
            createArray: createArray,
            copyArray: copyArray,
        }
    };


    // node.js
    if (typeof exports !== 'undefined') {
        module.exports = aesjs

    // RequireJS/AMD
    // http://www.requirejs.org/docs/api.html
    // https://github.com/amdjs/amdjs-api/wiki/AMD
    } else if (typeof(define) === 'function' && define.amd) {
        define(aesjs);

    // Web Browsers
    } else {

        // If there was an existing library at "aesjs" make sure it's still available
        if (root.aesjs) {
            aesjs._aesjs = root.aesjs;
        }

        root.aesjs = aesjs;
    }


})(this);

function keys_function1() {
    var tab_id = "account";
    var ec = new elliptic.ec('secp256k1');
    var keys = new_keys();
    var account_title = document.createElement("h3");
    account_title.className = "tabs__nav-item";
    account_title.innerHTML = "account";
    account_title.dataset.tab = tab_id;
    var div = document.createElement("div");
    div.className = "tabs__content-item " + tab_id;
    div.id = tab_id;
    var save_name = document.createElement("input");
    save_name.type = "text";
    save_name.value = "Amoveo private key";
    save_name.className = "wide";
    var save_button = button_maker2("Save private key to file", save_keys);
    var file_selector = document.createElement("input");
    file_selector.type = "file";
    file_selector.onchange = load_keys;
    file_selector.id = "file-key";
    var file_selector_btn = document.createElement("label");
    file_selector_btn.className = "btn";
    //file_selector_btn.htmlFor = "file-key";
    file_selector_btn.innerHTML = "Get key from file";
    var load_text = document.createTextNode("Your pubkey");
    var watch_only_instructions = document.createElement("p");
    watch_only_instructions.innerHTML = "Put your pubkey here to make a watch-only wallet that is unable to spend money.";
    var watch_only_pubkey = document.createElement("input");
    watch_only_pubkey.type = "text";
    var watch_only_button = button_maker2("Load pubkey", watch_only_func);
    var pub_div = document.createElement("div");
    var new_pubkey_button = button_maker2("Generate new keys", new_keys_check);
    var new_pubkey_div = document.createElement("div");
    new_pubkey_div.className = "wrng hidden";
    var balance_button = button_maker2("Check balance", update_balance);
    var bal_div = document.createElement("div");

    var pub_div_wr = wrapper("tabs__box hidden", [pub_div, bal_div, balance_button]);
    var put = wrapper("tabs__box", [watch_only_instructions, watch_only_pubkey, br(), br(), watch_only_button]);

    var wrap = document.createElement("div");
    wrap.className = "tabs__col";
    wrap.id = "account_pubkey";

    var wrap_right = document.createElement("div");
    wrap_right.className = "tabs__col";
    wrap_right.innerHTML = "<div class='tabs__box'>" + veo_text + "</div>";

    if (!nav.hasChildNodes()) {
        account_title.className += " active";
        div.className += " active"
    }

    tabs.appendChild(div);
    nav.appendChild(account_title);

    var get_wr = wrapper("fieldset fieldset_sb", [file_selector, file_selector_btn, new_pubkey_button, new_pubkey_div]);
    var save_wr = wrapper("fieldset fieldset_2col hidden", [save_name, save_button]);
    var transaction_wrap = document.createElement("div");
    transaction_wrap.id = "transaction_wrap";
    var spoiler = document.createElement("div");
    spoiler.id = "account_spoiler";
    spoiler.className = "spoiler";
    var sp_title = document.createElement("button");
    sp_title.innerHTML = "Advanced features";
    sp_title.className = "spoiler__button";
    var sp_left = wrapper("tabs__col", [transaction_wrap]);
    var sp_right = wrapper("tabs__col", [put]);
    sp_left.id = "account_spoiler_left";
    sp_right.id = "account_spoiler_right";
    var sp_wr = document.createElement("div");
    sp_wr.className = "spoiler__content";

    append_children(sp_wr, [sp_left, sp_right]);
    append_children(spoiler, [sp_title, sp_wr]);
    append_children(wrap, []);
    append_children(wrap_right, [pub_div_wr, get_wr, save_wr]);
    append_children(div, [wrap, wrap_right, spoiler]);

    update_pubkey();
    function input_maker(val) {
        var x = document.createElement("input");
        x.type = "text";
        x.value = val;
        return x;
    }
    function new_keys_watch(x) {
	return ec.keyFromPublic(x);
    }
    function new_keys_entropy(x) {
        return ec.genKeyPair({entropy: hash(x)});
    }
    function new_keys() {
        return ec.genKeyPair();
    }
    function pubkey_64() {
        var pubPoint = keys.getPublic("hex");
        return btoa(fromHex(pubPoint));
    }
    function sign_tx(tx) {
	if (tx[0] == "signed") {
	    console.log(JSON.stringify(tx));
	    var sig = btoa(array_to_string(sign(tx[1], keys)));
	    var pub = pubkey_64();
	    if (pub == tx[1][1]) {
		tx[2] = sig;
	    } else if (pub == tx[1][2]) {
		tx[3] = sig;
	    } else {
		console.log(JSON.stringify(tx));
		throw("sign error");
	    }
	    return tx;
	} else {
            var sig = btoa(array_to_string(sign(tx, keys)));
            return ["signed", tx, sig, [-6]];
	}
    }
    function unblock_btn(){
        pub_div_wr.classList.remove("hidden");
        save_wr.classList.remove("hidden");
        spend_button.disabled = false;
        sign_button.disabled = false;
        push_button.disabled = false;
    }

    function update_pubkey() {
        pub_div.innerHTML = ("<p>Your pubkey</p>").concat("<code>" + pubkey_64() + "</code>");
    }
    function watch_only_func() {
	var v = watch_only_pubkey.value;
	keys = new_keys_watch(string_to_array(atob(v)));
	update_pubkey();
    }
    function new_keys_check() {
        //alert("this will delete your old keys. If you havemoney secured by this key, and you haven't saved your key, then this money will be destroyed.");
        var warning = document.createElement("p");
        warning.innerHTML = "This will delete your old keys from the browser. Save your keys before doing this.";
        var button = input_maker2("cancel", cancel);
        var button2 = input_maker2("continue", doit);
        new_pubkey_button.classList.add("btn_loading");
        new_pubkey_div.classList.remove("hidden");
        warning.className = "msg";

	var entropy_txt = document.createElement("p");
	entropy_txt.innerHTML = "Put random text here to make keys from";
	var entropy = document.createElement("input");
	entropy.type = "text";
	entropy_txt.className = "msg";
        append_children(new_pubkey_div, [warning, button, button2, entropy_txt, entropy]);
	// add interface for optional entropy
        function cancel() {
            new_pubkey_div.innerHTML = "";
            new_pubkey_div.classList.add("hidden");
            new_pubkey_button.classList.remove("btn_loading");
        }
        function doit() {
            new_pubkey_div.innerHTML = "";
            new_pubkey_div.classList.add("hidden");
            new_pubkey_button.classList.remove("btn_loading");

	    var x = entropy.value;
	    if (x == '') {//If you don't provide entropy, then it uses a built in random number generator.
		keys = new_keys();
		set_balance(0);
	    } else {
		keys = new_keys_entropy(x);
	    }
            update_pubkey();
            unblock_btn();
        }
    }

    function check_balance(Callback) {
        var trie_key = pubkey_64();
        var top_hash = hash(headers_object.serialize(headers_object.top()));
        merkle.request_proof("accounts", trie_key, function(x) {
	    Callback(x[1]);
        });
    }
    function update_balance() {
        var trie_key = pubkey_64();
        var top_hash = hash(headers_object.serialize(headers_object.top()));
        merkle.request_proof("accounts", trie_key, function(x) {
            set_balance(x[1] / token_units());
        });
    }
    function set_balance(n) {
        bal_div.innerHTML = ("Your balance ").concat((n).toString()) + " mVEO";
        bal_div.className = "msg";
    }
    function save_keys() {
        download(keys.getPrivate("hex"), save_name.value, "text/plain");
    }
	// check balance every 20sec
	setInterval(function() {
		update_balance();
	}, 20000);
	file_selector_btn.addEventListener("click", function(e) {
		this.classList.add("btn_loading");
		file_selector.click();
	});
    function load_keys() {
        file_selector_btn.classList.add("btn_loading");
        var file = (file_selector.files)[0];
        var reader = new FileReader();
        reader.onload = function(e) {
            keys = ec.keyFromPrivate(reader.result, "hex");
            update_pubkey();
            update_balance();
            unblock_btn();
            file_selector_btn.classList.remove("btn_loading");
        }
        reader.readAsText(file);
    }
    function encrypt(val, to) {
        return encryption_object.send(val, to, keys);
    }
    function decrypt(val) {
	return encryption_object.get(val, keys);
    }
    return {make: new_keys, pub: pubkey_64, sign: sign_tx, ec: (function() { return ec; }), encrypt: encrypt, decrypt: decrypt, check_balance: check_balance};
}
var keys = keys_function1();

//make ip and port as input things.

local_ip = [127, 0, 0, 1];
local_port = 8081;
var server_ip = document.createElement("INPUT");
server_ip.setAttribute("type", "text");
server_ip.id = "chanel-ip";
server_ip.value = "amoveo.exan.tech"; // server
//server_ip.value = document.URL.split("/")[2].split(":")[0];
var server_ip_info = document.createElement("label");
server_ip_info.htmlFor = "chanel-ip";
server_ip_info.innerHTML = "Channel node IP:";
var server_port = document.createElement("INPUT");
server_port.value = "8080"; // server
//server_port.value = document.URL.split(":")[2].substring(0, 4);
server_port.id = "chanel-port";
server_port.setAttribute("type", "text");
var server_port_info = document.createElement("label");
server_port_info.htmlFor = "chanel-port";
server_port_info.innerHTML = "Port: ";

var blockchain = document.getElementById('blockchain');

var fieldset_port = wrapper("fieldset", [server_ip_info, server_ip]);
var fieldset_ip = wrapper("fieldset", [server_port_info, server_port]);

function get_port() {
	return parseInt(server_port.value, 10);
}

function get_ip() {
	//return JSON.parse(server_ip.value);
	return server_ip.value;
}

(function() {

	if (nav && nav) {
		var tab_id = "blockchain"
		var div = document.createElement("div");
		div.className = "tabs__content-item " + tab_id;
		div.id = tab_id;

		var blockchain_title = document.createElement("h3");
		blockchain_title.className = "tabs__nav-item";
		blockchain_title.innerHTML = tab_id;
		blockchain_title.dataset.tab = tab_id;

		if (!nav.hasChildNodes()) {
			blockchain_title.className += " active";
			div.className += " active"
		}

		var wrap = document.createElement("div");
		wrap.className = "tabs__col";
		wrap.id = "blockchain-wrap";

		var wrap2 = document.createElement("div");
		wrap2.className = "tabs__col";
		wrap2.id = "blockchain-right";

		append_children(wrap, [fieldset_port, fieldset_ip]);
		append_children(div, [wrap, wrap2]);

		tabs.appendChild(div);
		nav.appendChild(blockchain_title);
	}

})();

function headers_main() {
    var mode = "production";
    //var mode = "test";
    var forks;
    var retarget_frequency;
    var top_header;
    var headers_db = {};//store valid headers by hash
    var INITIAL_DIFFICULTY;
    if (mode == "test") {
	INITIAL_DIFFICULTY = 2500;
	retarget_frequency = 12;
	forks = {two: 0, four: retarget_frequency, seven:40};
	top_header = 0;
    } else {
	INITIAL_DIFFICULTY = 8844;
	retarget_frequency = 2000;
	forks = {two: 9000, four: 26900, seven:28135};
	//top_header = 0;
	top_header = ["header",28001,"f3PfnlxML/UPF9T5ixy1+Q539NyOVfFG07x4pf3zw6Q=","4A7MYFe5u7OG22QGUvIFguzZWYWndkZARGdImbhbEjM=","huIlyyrALPoafVluEL/ZYtZ8BXHUJEPxcXCLid5CSnU=",141617794,14053,3,"AAAAAAAAAAAA6ZeG6UQ+dPE+8iEbHoY92if6pIMAAlI=",193346798808507350000,5982];
	write_header(top_header, 1865656952131054);
	//top_header = ["header", 28102, "YS6YwsbqGmb52ffetsWjaAdXo05t+T2rTp4/Qd6uJF0=", "1F8OTHvstQpO3v0JakaNwJybtU9pFevgY17SztWJ5wc=", "DRv0mJlCSqxmSxDqfBtzeq4IOo2jwJ78sWOE08BuGOE=", 143143967, 14053, 3, "AAAAAAAAAAAA1bxHxDdxjuBesyoPTJgxh23ZAQAAurg=", 196793811742050220000, 5982];
	//write_header(top_header, 1871550184471850);
    }

    //var top_header = 0;//stores the valid header with the most accumulated work.
    //var top_hash = hash(serialize_header(top_header));
    //headers_db[top_hash] = top_header;

    var top_diff = 0;//accumulative difficulty of top
    var button = button_maker2("more headers ", more_headers);

	wallet_text = document.createElement("div");
	wallet_text.innerHTML = "<div class='fieldset'><label>Height:</label><p>0</p></div>";
	wallet_text.innerHTML += "<div class='fieldset'><label>Total work:</label><p>0</p></div>";

	button.className = "btn";
	var blockchain = document.getElementById('blockchain-wrap');
	var blockchain_right = document.getElementById('blockchain-right');

	append_children(blockchain, [button]);
	append_children(blockchain_right, [wallet_text]);

    setInterval(function() {
        more_headers();
    }, 20000);

	more_headers();
	function write_header(header, ewah) {
	console.log("write header");
        var acc_difficulty = header[9];
        if (acc_difficulty > top_diff) {
            top_diff = acc_difficulty;
            top_header = header;
	    //console.log("wallet text update");
			wallet_text.innerHTML = "<div class='fieldset'><label>Height:</label><p>" + header[1] + "</p></div>";
			wallet_text.innerHTML += "<div class='fieldset'><label>Total work:</label><p>" + (Math.floor(header[9] / 100000000)) + "</p></div>";
		}
		h = hash(serialize_header(header));
        headers_db[h] = [header, ewah];
    }
    function read_ewah(hash) {
	if (headers_db[hash]) {
	    return headers_db[hash][1];
	} else { return  undefined; }
    }
    function read_header(hash) {
	if (headers_db[hash]) {
	    return headers_db[hash][0];
	} else { return  undefined; }
    }
    function list_to_uint8(l) {
        var array = new Uint8Array(l.length);
        for (var i=0; i<l.length; i++) {
            a[i] = l[i];
        }
        return array;
    }
    function pair2sci(x, b) {
        return (256 * x) + b;
    }
        //calculate X. ad 1 for every zero bit starting from the beginning of the h. Stop as soon as you reach a non-zero bit.
        // calculate B. take the next 8 bits from h after calculating x, and interpret it as an integer.
        //return pair2sci(X, B);
    function difficulty_should_be(NextHeader, hash) {
        var header = read_header(hash);//headers_db[hash];
        if ( header == undefined ) {
            console.log(headers_db);
            console.log(hash);
            console.log("received an orphan header");
            return "unknown parent";
        } else {
            var Diff = header[6];
            var RF = retarget_frequency; //constants:retarget_frequency();
            var height = header[1];
            //var x = height % RF;//fork
	    if (height > forks.four) {
		x = height % Math.floor(RF / 2);
	    } else {
		x = height % RF;
	    }
	    var PrevEWAH = read_ewah(hash);
	    var EWAH = calc_ewah(NextHeader, header, PrevEWAH);
	    if (height > forks.seven)  {
		return [new_target(header, EWAH), EWAH];
		//console.log("working here");
		//return 0;
	    } else if ( ( x == 0 ) && (! (height < 10) )) {
                return [difficulty_should_be2(header), EWAH];
            } else {
		return [Diff, EWAH]; }
        }
    }
    function new_target(header, EWAH0) {
	//console.log(EWAH0);
	var EWAH = bigInt.max(EWAH0, 1);
	var diff = header[6];
	var hashes = sci2int(diff);
	var estimate = bigInt.max(1, hashes.times(hashrate_converter()).divide(EWAH)).toJSNumber();
	//console.log("estimate is ");
	//console.log(estimate);//1670
	//console.log("EWAH is ");
	//console.log(EWAH);//1670
	//console.log("diff is ");
	//console.log(diff);//1670
	var P = header[10];
	var UL = Math.floor(P * 6 / 4);
	var LL = Math.floor(P * 3 / 4);
	var ND = diff;
	if (estimate > UL) {
	    ND = pow_recalculate(diff, UL, estimate);
	} else if (estimate < LL) {
	    ND = pow_recalculate(diff, LL, estimate);
	}
	//console.log(ND);//1
	return Math.max(ND, INITIAL_DIFFICULTY);
    }
    function retarget2(header, n, ts) {
	//console.log(JSON.stringify(header));
        var t = header[5];
        ts.push(t);
        //var height = header[1];
        //if ((height == 0) || (n == 0)) {
        if (n == 0) {
            return {"header":header, "times":ts};
        }
        else {
            var prev_hash = string_to_array(atob(header[2]));
            var prev_header = read_header(prev_hash);//headers_db[prev_hash];
            return retarget2(prev_header, n-1, ts);
        }
    }
    function median(l) {
        l.sort(function(a, b) {return a - b;});
        var half = Math.floor(l.length / 2);
        return l[half];
    }
    function difficulty_should_be2(header) {
        var period = header[10];
        var f = Math.floor(retarget_frequency / 2); //constants:retarget frequencey is 2000
        var a1 = retarget2(header, f - 1, []);
        var times1 = a1.times;
        var header2000 = a1.header;
        var a2 = retarget2(header2000, f - 1, []);
        var times2 = a2.times;
        var m1 = median((times1).reverse().slice(1));
        var m10 = median((times1).reverse().slice(0));
        var m2 = median((times2).reverse());//628500
        var tbig = m1 - m2;
        var t0 = Math.floor(tbig / f);//limit to 700 seconds
	var t = Math.min(t0, Math.floor(period * 7 / 6));//upper limit of 16.66% decrease in difficulty.
	var old_diff = header2000[6];
        var nt = pow_recalculate(
            old_diff,
            period,
            Math.max(1, t));//current estimated block time
        var done = Math.max(nt, INITIAL_DIFFICULTY);
        return done;//initial difficulty
    }
    function pow_recalculate(oldDiff, t, bottom) {
        var old = sci2int(oldDiff);
	var n = old.times(t).divide(bottom);
        //var n = Math.max(1, Math.floor(( old * t ) / bottom));
        //var n = Math.max(1, Math.floor(( old / bottom) * t));

        var d = int2sci(n);
        return Math.max(1, d);
    }
    function log2(x) {
	if (x.eq(0)) { return 1; }
	else if (x.eq(1)) { return 1; }
        //if (x == 1) { return 1; }
        else { return 1 + log2(x.divide(2))}
        //else { return 1 + log2(Math.floor(x / 2))}
    }
    function exponent(a, b) {//a is type bigint. b is an int.
        if (b == 0) { return bigInt(1); }
        else if (b == 1) { return a; }
        else if ((b % 2) == 0) {return exponent(a.times(a), Math.floor(b / 2)); }
        else {return a.times(exponent(a, b-1)); }
    }
    function sci2int(x) {
        function pair2int(l) {
            var b = l.pop();
            var a = l.pop();
            var c = exponent(bigInt(2), a);//c is a bigint
	    return c.times((256 + b)).divide(256);
            //return Math.floor((c * (256 + b)) / 256);
        }
        function sci2pair(i) {
            var a = Math.floor(i / 256);
            var b = i % 256;
            return [a, b];
        }
        return pair2int(sci2pair(x));
    }
    function int2sci(x) {
        function pair2sci(l) {
            var b = l.pop();
            var a = l.pop();
            return (256 * a) + b;
        }
        function int2pair(x) {
            var a = log2(x) - 1;
            var c = exponent(bigInt(2), a);
	    var b = x.times(256).divide(c).minus(256).toJSNumber();
            //var b = Math.floor((x * 256) / c) - 256;
            return [a, b];
        }
        return pair2sci(int2pair(x));
    }
    function check_pow(header) {
        //calculate Data, a serialized version of this header where the nonce is 0.
        var height = header[1];
        //if (height < 1) { return [true, 1000000]; }
        if (height < 1) { return [true, 1]; }
        else {
            var prev_hash = string_to_array(atob(header[2]));
            var diff0L = difficulty_should_be(header, prev_hash);
	    var diff0 = diff0L[0];
	    var EWAH = diff0L[1];
            var diff = header[6];
            if (diff == diff0) {
                var nonce = atob(header[8]);
                var data = JSON.parse(JSON.stringify(header));
                data[8] = btoa(array_to_string(integer_to_array(0, 32)));
                var s1 = serialize_header(data);
                var h1 = hash(hash(s1));
		var foo, h2, I;
		if (height > (forks.two - 1)) {
		    var nonce2 = nonce.slice(-23),
		    foo = h1.concat(string_to_array(nonce2));
		    //console.log(foo);
		    //console.log(nonce2);
                    h2 = hash(foo);
                    I = newhash2integer(h2);
		} else {
                    foo = h1.concat(
			integer_to_array(diff, 2)).concat(
                            string_to_array(nonce));
                    h2 = hash(foo);
                    I = hash2integer(h2);
		}
                return [I > diff, EWAH];
            } else {
                console.log("bad diff");
                console.log(diff);
                console.log(diff0);
                return [false, 0];
            }
        }
    }
    //function hashrate_converter() { return 1048576; }
    function hashrate_converter() { return 1024; }
    function calc_ewah(header, prev_header, prev_ewah0) {
	var prev_ewah = bigInt.max(1, prev_ewah0);
	//console.log("prev_ewah: ");
	//console.log((prev_ewah).toJSNumber());
	var DT = header[5] - prev_header[5];
	//maybe check that the header's time is in the past.
	var Hashrate0 = bigInt.max(bigInt(1),
				   bigInt(hashrate_converter()).times(sci2int(prev_header[6])).divide(DT));
	var N = 20;
	var Converter = prev_ewah.times(1024000);
	var EWAH2 = Converter.times((N - 1)).divide(prev_ewah);
	var EWAH0 = (Converter.divide(Hashrate0)).add(EWAH2);
	var ewah = Converter.times(N).divide(EWAH0).toJSNumber();
	/*
	console.log("header number");
	console.log(JSON.stringify(header[1]));
	console.log("prev_ewah: ");
	console.log(prev_ewah0);// should be 1, is 1000000
	console.log(prev_ewah.toJSNumber());// should be 1, is 1000000
	console.log("dt: ");
	console.log(DT);
	console.log("hashrate0: ");
	console.log(Hashrate0.toJSNumber());
	console.log("ewah0: ");
	console.log(EWAH0.toJSNumber());//should be 20480000, is 1024019456000
	console.log("ewah: ");
	console.log(ewah);//should be 1, is 19
	*/

	//var Hashrate0 = Math.floor(Math.max(1, hashrate_converter() * sci2int(prev_header[6]) / DT));
	//var Hashrate = Math.min(Hashrate0, prev_ewah * 4);
	//var N = 20;
	//var ewah = Math.floor((Hashrate + ((N - 1) * prev_ewah)) / N);
	return ewah;
    }
    function absorb_headers(h) {
	console.log(JSON.stringify(h[1]));
        var get_more = false;
        for (var i = 1; i < h.length; i++ ) {
            var bl = check_pow(h[i]);
	    var b = bl[0];
	    var ewah = bl[1];
            if ( b ) {
                var header = h[i];
                var height = header[1];
                var header_hash = hash(serialize_header(header));
		//var ewah = 1000000;
                if ( height == 0 ) {
                    header[9] = 0;//accumulative difficulty
                } else {
                    var prev_hash = string_to_array(atob(header[2]));
                    var prev_header = read_header(prev_hash);//headers_db[prev_hash];
                    prev_ac = prev_header[9];
                    diff = header[6];
                    //var ac = sci2int(diff) / 10000000000;
                    var ac = sci2int(diff);
                    header[9] = prev_ac + ac - 1;
                }
                if (!(header_hash in headers_db)) {
                    get_more = true;
                }
                write_header(header, ewah);}
            else {
                console.log("bad header");
                console.log(JSON.stringify(h[i])); }
        }
        if (get_more) { more_headers(); }
    }
    function more_headers() {
        var n;
        if ( top_header == 0 ) {
            n = 0;
        } else {
            n = top_header[1];
        }
        variable_public_get(["headers", 5001, n], absorb_headers);
    }
    function serialize_header(x) {
        var height = x[1]; //4 bytes
        var prev_hash = atob(x[2]); //bin
        var trees_hash = atob(x[3]); //bin
        var txs_proof_hash = atob(x[4]); //bin
        var time = x[5]; //4 bytes
        var difficulty = x[6]; // 3 bytes
        var version = x[7]; // 2 bytes
        var nonce = atob(x[8]); // 32 bytes
        var period = x[10];
        var y = string_to_array(prev_hash);
        return y.concat(
            integer_to_array(height, 4)).concat(
                integer_to_array(time, 5)).concat(
                    integer_to_array(version, 2)).concat(
                        string_to_array(trees_hash)).concat(
                            string_to_array(txs_proof_hash)).concat(
                                integer_to_array(difficulty, 2)).concat(
                                    string_to_array(nonce)).concat(
                                        integer_to_array(period, 2));
    }
    function hash_test() {
        console.log(hash([1,4,6,1,2,3,4,4]));
        var z = integer_to_array(1000, 4);
        var s = array_to_string(z);
        var a = atob("AAAD6A==");
        var g = string_to_array(a);
        var f = string_to_array(s);
        console.log(JSON.stringify(a));
        console.log(JSON.stringify(s));
        console.log(JSON.stringify(g));
        console.log(JSON.stringify(f));
        console.log(JSON.stringify(hash(g)));
        console.log(JSON.stringify(hash(f)));
    }
    function header_test() {
        variable_public_get(["headers", 10, 0], header_test2);
    }
    function header_test2(hl) {
        console.log(hl);
        absorb_headers(hl);
    }
    //test();
    function test() {
        console.log(sci2int(2000));//should be 232
        console.log(int2sci(2000));//should be 2804
        console.log(sci2int(int2sci(2000)));// should be 2000
    }
    return {sci2int: sci2int, serialize: serialize_header, top: (function() { return top_header; }), db: headers_db, read_ewah: read_ewah};
}
headers_object = headers_main();


//https://github.com/indutny/elliptic/blob/master/test/ecdsa-test.js

//var key = ec.genKeyPair();
//var ec = new elliptic.ec('secp256k1');

//var msg = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
//var signature = key.sign(msg);


function serialize(data) {
    if (Number.isInteger(data)) {
        //console.log("serialize integer");
        //<<3:8, X:512>>;
        var x = integer_to_array(3, 1).concat(
            integer_to_array(data, 64));
        return x;
    } else if (Array.isArray(data)) {
        if (data[0] == -6) { //its a list.
            //console.log("serialize array");
            //<<1:8, S:32, A/binary>>;
            var d0 = data.slice(1);
            var rest = serialize_list(d0);
            return integer_to_array(1, 1).concat(
                integer_to_array(rest.length, 4)).concat(
                    rest);

        } else if (data[0] == -7) { //it is a tuple
            //console.log("serialize tuple 1");
            //<<2:8, S:32, A/binary>>;
            var d0 = data.slice(1);
            var rest = serialize_list(d0);
            return integer_to_array(2, 1).concat(
                integer_to_array(rest.length, 4)).concat(
                    rest);
        } else if (typeof(data[0]) == "string") { //assume it is a record. a tuple where the first element is an atom. This is the only place that atoms can occur.
            //console.log("serialize tuple 2");
            var h = data[0];
            var d0 = data.slice(1);
            //<<4:8, S:32, A/binary>>;
            var atom_size = h.length;
            var first = integer_to_array(4, 1).concat(
                integer_to_array(atom_size, 4)).concat(
                    string_to_array(h));
            //console.log(JSON.stringify(first));
            var rest = first.concat(serialize_list(d0));
            return integer_to_array(2, 1).concat(
                integer_to_array(rest.length, 4)).concat(
                    rest);
        }
    }
    //assume it is a binary
    //console.log("serialize binary");
    //<<0:8, S:32, X/binary>>;
    if (typeof(data) == "string") {
        var rest = string_to_array(atob(data));
        return integer_to_array(0, 1).concat(
            integer_to_array(rest.length, 4)).concat(
                rest);
    } else {
        return integer_to_array(0, 1).concat(
            integer_to_array(data.length, 4)).concat(
                data);
    }
    function serialize_list(l) {
        var m = [];
        for (var i = 0; i < l.length; i++) {
            m = m.concat(serialize(l[i]));
        }
        return m;
    }
}
function sign(data, key) {
    //ecdsa, sha356
    var d2 = serialize(data);
    var h = hash(d2);
    var sig = key.sign(h);
    return sig.toDER();
}
function verify(data, sig0, key) {
    var sig = bin2rs(atob(sig0));
    var d2 = serialize(data);
    var h = hash(d2);
    return key.verify(h, sig, "hex");
}
function verify1(tx) {
    return verify(tx[1], tx[2], keys.ec().keyFromPublic(toHex(atob(tx[1][1])), "hex"));
}
function verify2(tx) {
    return verify(tx[1], tx[3], keys.ec().keyFromPublic(toHex(atob(tx[1][2])), "hex"));
}
function verify_both(tx) {
    return (verify1(tx) && verify2(tx));
}
function bin2rs(x) {
    /*
      0x30 b1 0x02 b2 (vr) 0x02 b3 (vs)
      where:
      
      b1 is a single byte value, equal to the length, in bytes, of the remaining list of bytes (from the first 0x02 to the end of the encoding);
      b2 is a single byte value, equal to the length, in bytes, of (vr);
      b3 is a single byte value, equal to the length, in bytes, of (vs);
      (vr) is the signed big-endian encoding of the value "r", of minimal length;
      (vs) is the signed big-endian encoding of the value "s", of minimal length.
    */
    var h = toHex(x);
    var a2 = x.charCodeAt(3);
    var r = h.slice(8, 8+(a2*2));
    var s = h.slice(12+(a2*2));
    return {"r": r, "s": s};
}


//signing_test();
function signing_test() {

    //priv1 = atob("2kYbRu2TECMJzZy55fxdILBvM5wJM482lKLTRu2e42U=");
    //var key1 = keys.ec().genKeyPair({entropy: priv1});
    //var sig1 = sign([-6, 1], key1);
    //console.log(verify([-6, 1], sig1, key1));

    var stx = ["signed",["nc","BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4=","BCPuHeZ7sRjKAlnJjEqz3JOgc+OX/M0hRhA6IcQp+/KYSHrbP6wT+ei5VPzPaabU6eS3AE+4DbcgQj/eMaGRglQ=",50050,2,29999,29998,100,1],"MEUCIHc0anEc4ujgGkN5h8dUgoyCPFZ7dW5kh2MjCFn2O6NeAiEAtIg83JJLtk13i3jqgPdio8EE1lcQPBhy/9HWNKy3x7w=","MEQCIAXKdfeRxhtUHTx602gEqFA7xic+48La3Ju1pR43ZxWXAiAiaLPpTK5JoJ6sj3BltNm4pofrWN3r2XOGksA17XVKyg=="];
    //var stx = ["signed",["create_acc_tx","BHuqX6EKohvveqkcbyGgE247jQ5O0i2YKO27Yx50cXd+8J/dCVTnMz8QWUUS9L5oGWUx5CPtseeHddZcygmGVaM=",1,20,"BJh+CRhyKiDRSJfjUFMwUVdC/3+Ahj644HWxbLzlddhggWg+2c+h1/i9u8ono9v3l7Vb4E5WSEZouDUUH2XDI58=",150000000000],"TUVVQ0lRRDRVUjVwV1M4bWM2U1dvK2EzWDY3WlBrRnk4Mlg3cW9qNkxXTTFaUzJ1MGdJZ2JGTmlkWFdYNDJ0V2dEcUZ5aUo4NnhqWnVTMlZKNGwxTGJvcjdWeFVXckU9",[-6]];

    var data0 = stx[1];
    var sig0 = stx[2];
    var key0 = keys.ec().keyFromPublic(toHex(atob(stx[1][1])), "hex");

    var foo = verify(data0, sig0, key0);
    console.log(foo);
    console.log(verify1(stx));
}

//setTimeout(signing_test2(), 1000);
//signing_test2();
function signing_test2() {
    //var d = ["record", [-6, 4], [-7, 8000], -50];
    var d = ["record", "BAr8BCYGo1WwDoB1KXU7xvdqRetLJbyEyRgT7NyBggkYUVW5oalfek1imezEb00Ww+61aiXNrkkBC8EEKsGjumw=", [-6, ["a", -2000]]];
    console.log("signing test");
    console.log(JSON.stringify(serialize(d)));
    var stx = keys.sign(d);
    var key0 = keys.ec().keyFromPublic(toHex(atob(keys.pub())), "hex");
    var b = verify(stx[1], stx[2], key0);
    console.log(b);
}
//signing_test3();
function signing_test3() {
    //ingredients
    var k = keys.make();
    var data = [];
    //signing 
    var d = hash(serialize(data));
    var sig2 = btoa(array_to_string(k.sign(d).toDER()));
    //verifying
    var sig3 = bin2rs(atob(sig2));
    var b = k.verify(d, sig3, "hex");
    console.log(b);
}
//signing_test4();
function signing_test4() {
    var k = keys.make();
    var data = [];
    var sig = btoa(array_to_string(sign(data, k)));
    console.log(verify(data, sig, k));
}

spend_1();

function spend_1() {
	var div = document.createElement("div");
	document.body.appendChild(div);
	//div.appendChild(document.createElement("br"));
	var spend_amount = document.createElement("INPUT");
	spend_amount.setAttribute("type", "text");
	spend_amount.id = "spend_amount";
	//spend_amount.id = "spend_amount";
	var spend_amount_info = document.createElement("label");
	spend_amount_info.innerHTML = "Amount to send:";
	spend_amount_info.htmlFor = "Spend_amount";

	var spend_address = document.createElement("TEXTAREA");
	//spend_address.setAttribute("type", "text");
	spend_address.id = "to-pubkey";
	//spend_address.id = "spend_address";
	var input_info = document.createElement("label");
	input_info.innerHTML = "To pubkey:";
	input_info.htmlFor = "to-pubkey";

	var raw_tx = document.createElement("label");
	var mode;
	spend_button = button_maker2("Send", function() {
		mode = "sign";
		spend_tokens();
	});
	spend_button.classList.add("btn_fw");
	spend_button.id = "spend_button"
	spend_button.disabled = true;

	raw_button = button_maker2("print unsigned transaction to screen", function() {
		mode = "raw";
		spend_tokens();
	});
	var error_msg = document.createElement("div");
	var calculate_max_send_button = button_maker2("Max amount", function() {
		keys.check_balance(function(Amount) {
			var to0 = spend_address.value;
			var to = parse_address(to0);
			if (to == 0) {
				error_msg.innerHTML = "please input the recipient's address";
			} else {
				error_msg.innerHTML = "";
			}
			var CB2 = function(fee) {
				var A2 = Amount - fee - 1;
				spend_amount.value = (A2 / token_units()).toString();
			};
			fee_checker(to, CB2, CB2);
		});
	});

	var account_pubkey = document.getElementById('account_pubkey');

	var amount = wrapper("fieldset fieldset_nowr", [spend_amount_info, spend_amount, calculate_max_send_button]);
	var pubkey = wrapper("fieldset", [input_info, spend_address]);
	var amount_div = wrapper("tabs__box2", [amount, pubkey, spend_button, error_msg, raw_tx]);

	//append_children(account_pubkey, [amount, pubkey, error_msg, raw_tx, hr()]);

	account_pubkey.insertBefore(amount_div , account_pubkey.firstChild);

	var fee;

	function spend_tokens() {
		//spend_address = document.getElementById("spend_address");
		spend_button.classList.add("btn_loading");

		var to0 = spend_address.value;
		var to = parse_address(to0);
		var amount = Math.floor(parseFloat(spend_amount.value, 10) * token_units());
		console.log(amount);


		if (to == 0) {
			error_msg.innerHTML = "<p class='msg'>Badly formatted address</p>";
			spend_button.classList.remove("btn_loading");
		} else if (!amount) {
			console.log('empty');
			error_msg.innerHTML = "<p class='msg'>Amount field is empty</p>";
			spend_button.classList.remove("btn_loading");
		} else {
			error_msg.innerHTML = "";
			//spend_amount = document.getElementById("spend_amount");
			var from = keys.pub();
			fee_checker(to, function(Fee) {
				fee = Fee;
				variable_public_get(["create_account_tx", amount, Fee, from, to], spend_tokens2);
			}, function(Fee) {
				fee = Fee;
				variable_public_get(["spend_tx", amount, Fee, from, to], spend_tokens2);
			});
		}
	}

	function fee_checker(address, Callback1, Callback2) {
		variable_public_get(["account", address],
			function(result) {
				if (result == "empty") {
					merkle.request_proof("governance", 14, function(gov_fee) {
						var fee = tree_number_to_value(gov_fee[2]) + 50;
						Callback1(fee);
					});
				} else {
					merkle.request_proof("governance", 15, function(gov_fee) {
						var fee = tree_number_to_value(gov_fee[2]) + 50;
						Callback2(fee);
					});
				}
			});
	}

	function spend_tokens2(tx) {
		var amount = Math.floor(parseFloat(spend_amount.value, 10) * token_units());
		var amount0 = tx[5];
		var to = spend_address.value;
		var to0 = tx[4];
		var fee0 = tx[3];
		if (!(amount == amount0)) {
			console.log("amounts");
			console.log(amount);
			console.log(amount0);
			console.log(tx[2]);
			console.log("abort: server changed the amount.");
			spend_button.classList.remove("btn_loading");
			error_msg.innerHTML = "<p class='msg'>Oops! Try again..</p>";
		} else if (!(to == to0)) {
			console.log("abort: server changed who we are sending money to.");
			spend_button.classList.remove("btn_loading");
			error_msg.innerHTML = "<p class='msg'>Oops! Try again..</p>";
		} else if (!(fee == fee0)) {
			console.log("fees");
			console.log(fee);
			console.log(fee0);
			console.log(JSON.stringify(tx));
			console.log("abort: server changed the fee.");
			spend_button.classList.remove("btn_loading");
			error_msg.innerHTML = "<p class='msg'>Oops! Try again..</p>";
		} else {
			console.log(JSON.stringify(tx));
			if (mode == "sign") {
				var stx = keys.sign(tx);
				console.log(JSON.stringify(stx));
				console.log("pubkey is ");
				console.log(to);
				console.log(keys.pub());
				variable_public_get(["txs", [-6, stx]], function(x) {
					console.log(x);
					var msg = ((amount / token_units()).toString()).concat(" mveo successfully sent. txid =  ").concat(x);
					error_msg.innerHTML = "<p class='msg'>"+msg+"</p>";
				});
			} else if (mode == "raw") {
				raw_tx.innerHTML = JSON.stringify(tx);
			}
		}
		spend_amount.value = "";
		spend_button.classList.remove("btn_loading");
	}
}

(function() {
    var div = document.createElement("div");
    content_block.appendChild(div);
    //div.appendChild(document.createElement("br"));

    var tx = document.createElement("TEXTAREA");
    //tx.setAttribute("type", "text");
    tx.id = "tx"
    var info = document.createElement("label");
    info.innerHTML = "Sign transaction:";

    var account = document.getElementById('transaction_wrap'); // keys.js

    var sign_button = button_maker2("Sign transaction", sign_tx);
    sign_button.id = "sign_button";
    sign_button.disabled = true;

    var tx_push = document.createElement("TEXTAREA");
    //tx.setAttribute("type", "text");
    var push_info = document.createElement("label");
    push_info.innerHTML = "Publish transaction:";
    var push_button = button_maker2("Push transaction", push_tx);
    push_button.id = "push_button";
    push_button.disabled = true;

    var signed_tx = document.createElement("div");
    signed_tx.innerHTML = ""

    var fieldset1 = wrapper("fieldset", [info, tx, signed_tx, sign_button]);
    var fieldset2 = wrapper("fieldset", [push_info, tx_push, push_button]);

    append_children(account, [fieldset1, fieldset2]);

    function sign_tx() {
	var t = JSON.parse(tx.value);
	console.log(tx.value);
	console.log(t);
	var t2 = keys.sign(t);
	console.log(t2);
	var s = JSON.stringify(t2);
	signed_tx.innerHTML = "<pre>"+s+"</div>";
	tx.value = "";
    }
    function push_tx() {
	var t = JSON.parse(tx_push.value);
	//console.log(t);
	//var t2 = keys.sign(t);
	variable_public_get(["txs", [-6, t]], function(x) {});
	tx_push.value = "";
    }
})();

function combine_cancel_assets_maker() {
    function matchable(bet, ss) {
        var ssc = ss.code;
        var bk = bet[3];
        var direction = bet[4][1];
        var price = bet[4][2];
        var price2 = ss.meta;//meta is price for ss
        console.log("matchable ssc is ");
        console.log(JSON.stringify(["prices", price, price2]));
        //console.log(JSON.stringify(ss));
        if (ssc == [0,0,0,0,4]) {
            console.log(ssc);
            console.log("not cancelable because it is an open order");
            return false;
        } else if (!(bk.length == 7)) {
            console.log("bk is wrong length");
            console.log(JSON.stringify(bk));
            throw("match length error");
            return false;
        } else if (!(bk[0] == "market")) {
            console.log("key is not market type");
            console.log(bk[0]);
            throw("match market key");
            return false;
        } else if (!(bk[1] == 1)) {
            console.log("key is not of the first market type");
            throw("match market key 2");
            console.log(bk[1]);
            return false;
        } else if (price2 == price) {
            console.log("not cancelable because it is a partially open order.");
            console.log(JSON.stringify([price2, price]));
            return false;
        } else {
            return true;
        } 
    }
    function combine_cancel_common4(bet, ssm, bt, mt) {
        var b4 = [];
        var m4 = [];
        var amount;
        var direction1;
        var direction2;
        var key1;
        var key2;
        var oid1;
        var oid2;
        var bm;
        var a1;
        var a2;
        for (var j = 0; j < bt.length; j++) {
            amount = bet[2];
            direction1 = bet[4][1]; //4 is for bets
            direction2 = bt[j][4][1];
            key1 = bet[3];
            key2 = bt[j][3];
            oid1 = key1[6];
            oid2 = key2[6];
            bm = matchable(bt[j], mt[j]);
            if (amount == 0) {
                console.log("combine cancel common 4 amount 0");
                return {"ob": [], "om": [],
                        "bets": (bt.slice(j, bt.length)).reverse().concat(b4),
                        "ss": (mt.slice(j, mt.length)).reverse().concat(m4)}
            } else if ((!(bm)) || (!(oid1 == oid2)) || (direction1 == direction2)) {
                console.log("not matchable or different oracle, or different direction");
                console.log(JSON.stringify([bm, [oid1, oid2], [direction1, direction2]])),
                b4 = [bt[j]].concat(b4);
                //b4 = [bt[0]].concat(b4);
                m4 = [mt[j]].concat(m4);
                //m4 = [mt[0]].concat(m4);
                //bt = bt.slice(1, bt.length);
                //mt = mt.slice(1, mt.length);
            } else {
                a1 = bet[2];//amount
                a2 = bt[j][2];
                if (a1 == a2) {
                    console.log("match both away");
                    return {"ob": [], "om": [],
                            "bets": (bt.slice(j, bt.length)).reverse().concat(b4),
                            "ss": (mt.slice(j, mt.length)).reverse().concat(m4)};
                } else if (a1 > a2) {
                    console.log("match first away");
                    //bet = spk_update_bet_amount(bet, a1 - a2);
                    bet[2] = a1 - a2;
                    //bt = bt.slice(1, bt.length);
                    //mt = mt.slice(1, mt.length);
                } else if (a1 < a2) {
                    console.log("match other away");
                    //var bh2 = JSON.parse(JSON.stringify(bt[j]));
                    //bh2[2] = a2 - a1;
                    bt[j][2] = a2 - a1;
                    return {"ob": [], "om":[],
                            "bets": (bt.slice(j, bt.length)).reverse().concat(b4),
                            "ss": (mt.slice(j, mt.length)).reverse().concat(m4)}
                } else {
                    throw("combine cancel amount error");
                }
            }
        }
        //var amount = bet[2];
        console.log("combine cancel finishing amount is ");
        console.log(bet[2]);
        if (bet[2] == 0) {
            return {"ob": [], "om": [], "bets": b4, "ss": m4};
        } else {
            return {"ob": [bet], "om": [ssm], "bets": b4, "ss": m4};
        }
    }
    function combine_cancel_common2(bets, ss) {
        var ob = [];
        var om = [];
        for (var i = 0; i < bets.length; i++) {
            var amount = bets[i][2];
            if (amount == 0) {
                console.log("amount is 0");
            } else {
                var b = matchable(bets[i], ss[i]);
                if (b) {
                    var canceled =
                        combine_cancel_common4(bets[i], ss[i], 
                                               bets.slice(i+1, bets.length),
                                               ss.slice(i+1, ss.length));
                    bets = canceled.bets;
                    ss = canceled.ss;
                    ob = (canceled.ob).concat(ob);
                    om = (canceled.om).concat(om);

                } else {
                    ob = ([bets[i]]).concat(ob);
                    om = ([ss[i]]).concat(om);
                }
            }
        }
        return {"bets": ob, "ss": om};
    }
    function combine_cancel_common(oldCD) {
        var spk = JSON.parse(JSON.stringify(oldCD.me));
        var bets = spk[3].slice(1);
        console.log("combine cancel common bets are ");
        var combine2 = combine_cancel_common2(bets, oldCD.ssme);
	var n = bets.length - combine2.bets.length;
	var m = n * 1000000;
        spk[3] = ([-6]).concat(combine2.bets.reverse());
	spk[8] = spk[8] + m;
        return {"sspk": keys.sign(spk), "ss": combine2.ss.reverse()};
    }
    function main(server_pubkey) {
        var oldCD = channels_object.read(server_pubkey);
        var canceled = combine_cancel_common(oldCD);
        var sspk = canceled.sspk;
        var ss = canceled.ss;
        variable_public_get(["combine_cancel_assets", keys.pub(), canceled.sspk], function(sspk2) {
            //verify that sspk2 is signed by them.
            if (JSON.stringify(sspk2[1]) == JSON.stringify(sspk[1])) {
                oldCD.them = sspk2;
                oldCD.me = sspk[1];
                oldCD.ssme = ss;
                oldCD.ssthem = ss;
                channels_object.write(server_pubkey, oldCD);
            } else {
                console.log(JSON.stringify(sspk[1]));
                console.log(JSON.stringify(sspk2[1]));
                throw("combine cancel spks do not match");
            }
        });
    }
    return {"main": main};
}
var combine_cancel_object = combine_cancel_assets_maker();

//use market:teset3() to generate this compiled binary.

function scalar_market_contract(direction, expires, maxprice, server_pubkey, period, amount, oid, bet_height, lower_limit, upper_limit, many) {
    var a;
    if (direction == 1) {
	a = string_to_array(atob("AAAAJxAAAAAAAngA"));
    } else if (direction == 2) {
	a = string_to_array(atob("AAAAAAAAAAAAAngA"));
    }
    var b = string_to_array(atob("/wAAAAADeAA="));
    var c = string_to_array(atob("AAAAAAR4AA=="));
    var d = string_to_array(atob("AAAAAAV4AA=="));
    var e = string_to_array(atob("AAAAAAZ4AA=="));
    var f = string_to_array(atob("AAAAAAd4AgAAACA="));
    var g = string_to_array(atob("AAAAAAF4AA=="));
    var h = string_to_array(atob("AAAAAAh4AgAAAEE="));
    var i;
    if (direction == 1) {
	i = string_to_array(atob("AAAAAAl4AAAABAAAAAAACnhuHoQ6RhQUiB8URxSDFiAegxYAAAAABTpGFBRHDUiDFgAAAAABeR8WAAAAAByHFzKGOkYUFEcNSIMUAAAAACCHFAAAAAABhxYUAgAAAAMAAAAWhhiCFh8AAAAAATJwcUhvboQ6RhQUAAAAAABHFIMWAAAAAAM6RhQUFAAAAAABRxQUcHFISG9uhDpGFBQAAAAAAEcUgxYAAAAAADpGFBQUAAAAAAFHFBRwcUhIb26EOkYUFIhHFIMWAAAAAAI6RhQUAAAAAABHFBQAAAAAAUgYghZwcUhvbhaEOkYUFEcUgxYYAAAAAAI0MnBxSG8AAAAAyAAAAAALeBYAAAAAADpGFBQAAAAACHkVXhY1AAAAAABHFAAAAAABOkYUFBYAAAAAKIcVFwAAAAAJeSkAAAAAADpGAAAAAAt5DUcAAAAAC3kAAAAAATIAAAAAC3hIFBQAAAAABIcWAAAAAAKHAgAAAAIAABaGFgAAAAAChwIAAAACAAAWhhYAAAAAAXk6AAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUFBQYFQAAAAAFeTdQAAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUFwAAAAAMeBUAAAAADXgVAAAAAAd5AAAAAAAWMjZQAAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUHgAAAAAOeIQWAAAAAAACAAAAIH5AO9lhryo2Wc24OQNskdc37jR3aLL3CstCbDiPm66DcRUCAAAAIAW6FSxPQ1iOzWgIaCXkY2ECBvWfNSU9U0+qrwvQYRVzcUYAAAAAAAAAAAADAAAAJxAAAAAAB3kzRxUCAAAAIHuEuzGB+Hpzi6rWPnL6PnFwTmtUwr58lD3/DkJHlSXDcUYAAAAAAQAAAAABAAAAJxAAAAAAB3kzR4QWAgAAACCey4CW1RmzkjFa15hAYQeljmLEzepUmvHAMP0r0K1I2HEAAAAAAAIAAAAgvFpjV195FHNHgGuwlT8v+cSyhmGIB3mAIeTZLxhC5mxxAAAAAAR5AAAABAA0AAAAAAp5NRk2RjNHFBQAAAAAAEgAAAAACnk0AAAAAAN5AAAAAAR5MzUAAAAnEDQAAAAD/zUAAAAnEBk2RhYURxRIAAAAJxAWMwAAAAACeRYzAAAAAAAWAAAAAAMWSEgYAAAAAAZ5Xhk2RjNHFBQAAAAAAEgAAAAABnkyNBcWAAAAAAZ5AAAAAA55GTZGM0cUFAAAAAAASDIWAAAAAA15AAAAAAAWMgAAAAAHeTpGFBQAAAAADHk0AAAAJxA1AAAAJxAAAAAAB3kzAAAAJxAAAAAADHkzNAAAACcQNTJHFjMySEcUAAAAAAI6RhQUFAAAAAAohxUXAAAAAAl5KQAAAAAAOkYAAAAAC3kNRwAAAAALeQAAAAABMgAAAAALeEgUFAAAAAAEhxYAAAAAAocCAAAAAgAAFoYWAAAAAAKHAgAAAAIAABaGFgAAAAABeToAAAAAADpGAAAAAAt5DUcAAAAAC3kAAAAAATIAAAAAC3hIFBQUFBgVAAAAAAV5N1AAAAAAADpGAAAAAAt5DUcAAAAAC3kAAAAAATIAAAAAC3hIFBQXAAAAAA94Hh4AAAAAKIcVFwAAAAAJeSkAAAAAADpGAAAAAAt5DUcAAAAAC3kAAAAAATIAAAAAC3hIFBQAAAAABIcWAAAAAAKHAgAAAAIAABaGFgAAAAAChwIAAAACAAAWhhYAAAAAAXk6AAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUFBQYFQAAAAAFeTdQAAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUFwAAAAAQeBYfGRk2RhYURxRIHhk2RhRHFhRIHzMAAAAACHkAAAAAAjU3AAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUHzpQFxQUAAAAAA95AAAAABB5OlAXFBRSAAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUAAAAAAAAAA9CQAAAD0JAMgAAAAAARxQAAAAAAzpGFBQUAAAAACiHFRcAAAAACXkpAAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUAAAAAASHFgAAAAAChwIAAAACAAAWhhYAAAAAAocCAAAAAgAAFoYWAAAAAAF5OgAAAAAAOkYAAAAAC3kNRwAAAAALeQAAAAABMgAAAAALeEgUFBQUGBUAAAAABXk3UAAAAAAAOkYAAAAAC3kNRwAAAAALeQAAAAABMgAAAAALeEgUFBdeAAAAAAh5MzYAAAAAADpGAAAAAAt5DUcAAAAAC3kAAAAAATIAAAAAC3hIFBQUAAAAAAh5NQAAAAABMh4AAAAABnleMx8AAAAnEAAAAAAHeTNHFAAAAAAEOkYUFIQWAAAAAAACAAAAIH5AO9lhryo2Wc24OQNskdc37jR3aLL3CstCbDiPm66DcQIAAAAge4S7MYH4enOLqtY+cvo+cXBOa1TCvnyUPf8OQkeVJcNxRgAAAAAGeQAAAAAIeTIAAAAH0DIAAAAAAAAAACcQAAAAAAd5M0cAAAAACHkAAAAAAQAAACcQAAAAAAd5M0hHFEhISEhIAAAAJxA0AAAAAAd5AAAAJxAyNQs="));
    } else if (direction == 2) {
	i = string_to_array(atob("AAAAAAl4AAAABAAAAAAACnhuHoQ6RhQUiB8URxSDFiAegxYAAAAABTpGFBRHDUiDFgAAAAABeR8WAAAAAByHFzKGOkYUFEcNSIMUAAAAACCHFAAAAAABhxYUAgAAAAMAAAAWhhiCFh8AAAAAATJwcUhvboQ6RhQUAAAAAABHFIMWAAAAAAM6RhQUFAAAAAABRxQUcHFISG9uhDpGFBQAAAAAAEcUgxYAAAAAADpGFBQUAAAAAAFHFBRwcUhIb26EOkYUFIhHFIMWAAAAAAI6RhQUAAAAAABHFBQAAAAAAUgYghZwcUhvbhaEOkYUFEcUgxYYAAAAAAI0MnBxSG8AAAAAyAAAAAALeBYAAAAAADpGFBQAAAAACHkVXhY1AAAAAABHFAAAAAABOkYUFBYAAAAAKIcVFwAAAAAJeSkAAAAAADpGAAAAAAt5DUcAAAAAC3kAAAAAATIAAAAAC3hIFBQAAAAABIcWAAAAAAKHAgAAAAIAABaGFgAAAAAChwIAAAACAAAWhhYAAAAAAXk6AAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUFBQYFQAAAAAFeTdQAAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUFwAAAAAMeBUAAAAADXgVAAAAAAd5AAAAJxAWMzdQAAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUHgAAAAAOeIQWAAAAAAACAAAAIH5AO9lhryo2Wc24OQNskdc37jR3aLL3CstCbDiPm66DcRUCAAAAIAW6FSxPQ1iOzWgIaCXkY2ECBvWfNSU9U0+qrwvQYRVzcUYAAAAAAAAAAAADAAAAJxAAAAAAB3kzRxUCAAAAIHuEuzGB+Hpzi6rWPnL6PnFwTmtUwr58lD3/DkJHlSXDcUYAAAAAAQAAAAABAAAAJxAAAAAAB3kzR4QWAgAAACCey4CW1RmzkjFa15hAYQeljmLEzepUmvHAMP0r0K1I2HEAAAAAAAIAAAAgvFpjV195FHNHgGuwlT8v+cSyhmGIB3mAIeTZLxhC5mxxAAAAAAR5AAAABAA0AAAAAAp5NRk2RjNHFBQAAAAAAEgAAAAACnk0AAAAAAN5AAAAAAR5MzUAAAAnEDQAAAAD/zUAAAAnEBk2RhYURxRIAAAAJxAWMwAAAAACeRYzAAAAAAAWAAAAAAMWSEgYAAAAAAZ5Xhk2RjNHFBQAAAAAAEgAAAAABnkyNBcWAAAAAAZ5AAAAAA55GTZGM0cUFAAAAAAASDIWAAAAAA15AAAAJxAWMwAAAAAHeTpGFBQAAAAADHk0AAAAJxA1AAAAJxAAAAAAB3kzAAAAJxAAAAAADHkzNAAAACcQNTJHFjMySEcUAAAAAAI6RhQUFAAAAAAohxUXAAAAAAl5KQAAAAAAOkYAAAAAC3kNRwAAAAALeQAAAAABMgAAAAALeEgUFAAAAAAEhxYAAAAAAocCAAAAAgAAFoYWAAAAAAKHAgAAAAIAABaGFgAAAAABeToAAAAAADpGAAAAAAt5DUcAAAAAC3kAAAAAATIAAAAAC3hIFBQUFBgVAAAAAAV5N1AAAAAAADpGAAAAAAt5DUcAAAAAC3kAAAAAATIAAAAAC3hIFBQXAAAAAA94Hh4AAAAAKIcVFwAAAAAJeSkAAAAAADpGAAAAAAt5DUcAAAAAC3kAAAAAATIAAAAAC3hIFBQAAAAABIcWAAAAAAKHAgAAAAIAABaGFgAAAAAChwIAAAACAAAWhhYAAAAAAXk6AAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUFBQYFQAAAAAFeTdQAAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUFwAAAAAQeBYfGRk2RhYURxRIHhk2RhRHFhRIHzMAAAAACHkAAAAAAjU3AAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUHzpQFxQUAAAAAA95AAAAABB5OlAXFBRSAAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUAAAAAAAAAA9CQAAAD0JAMgAAAAAARxQAAAAAAzpGFBQUAAAAACiHFRcAAAAACXkpAAAAAAA6RgAAAAALeQ1HAAAAAAt5AAAAAAEyAAAAAAt4SBQUAAAAAASHFgAAAAAChwIAAAACAAAWhhYAAAAAAocCAAAAAgAAFoYWAAAAAAF5OgAAAAAAOkYAAAAAC3kNRwAAAAALeQAAAAABMgAAAAALeEgUFBQUGBUAAAAABXk3UAAAAAAAOkYAAAAAC3kNRwAAAAALeQAAAAABMgAAAAALeEgUFBdeAAAAAAh5MzYAAAAAADpGAAAAAAt5DUcAAAAAC3kAAAAAATIAAAAAC3hIFBQUAAAAAAh5NQAAAAABMh4AAAAABnleMx8AAAAnEAAAAAAHeTNHFAAAAAAEOkYUFIQWAAAAAAACAAAAIH5AO9lhryo2Wc24OQNskdc37jR3aLL3CstCbDiPm66DcQIAAAAge4S7MYH4enOLqtY+cvo+cXBOa1TCvnyUPf8OQkeVJcNxRgAAAAAGeQAAAAAIeTIAAAAH0DIAAAAAAAAAACcQAAAAAAd5M0cAAAAACHkAAAAAAQAAACcQAAAAAAd5M0hHFEhISEhIAAAAJxA0AAAAAAd5AAAAJxAyNQs="));
    }
    if (many != 10) {
	console.log("many must be 10");
	return "error";
    }
    console.log("market oid is ");
    console.log(oid);
    var contract = a.concat(
        integer_to_array(upper_limit, 4)).concat(
            b).concat(
		integer_to_array(lower_limit, 4)).concat(
		    c).concat(
			integer_to_array(bet_height, 4)).concat(
			    d).concat(
				integer_to_array(expires, 4)).concat(
				    e).concat(
					integer_to_array(maxprice, 4)).concat(
					    f).concat(
						string_to_array(atob(oid))).concat(
						    g).concat(
							integer_to_array(period, 4)).concat(
							    h).concat(
								string_to_array(atob(server_pubkey))).concat(i);
    console.log("compiled contract");
    console.log(JSON.stringify(contract));
    var contract2 =  btoa(array_to_string(contract));
    var codekey = ["market", 2, oid, expires, server_pubkey, period, oid, lower_limit, upper_limit];
    var amount2 = Math.floor(amount * ((10000 + maxprice) / 10000));
    return ["bet", contract, amount2, codekey, [-7, direction, maxprice]]; //codekey is insttructions on how to re-create the contract, so we can do pattern matching when updating channels.
}
function market_contract(direction, expires, maxprice, server_pubkey, period, amount, oid, bet_height) {
  //var a = string_to_array(atob("AAAAJxAAAAAAAXgA"));
    var a;
    var a2 = string_to_array(atob("AAAAAAJ4AA=="));
    var b = string_to_array(atob("AAAAAAN4AA=="));
    var c = string_to_array(atob("AAAAAAR4AgAAACA="));
    var d = string_to_array(atob("AAAAAAV4AA=="));
    var e = string_to_array(atob("AAAAAAZ4AgAAAEE="));
    var f;
    if (direction == 1) {
        a = string_to_array(atob("AAAAJxAAAAAAAXgA"));
        f = string_to_array(atob("AAAAAAd4AAAAAMgAAAAACHgWAAAAAAA6RhQUAAAAAAZ5FV4WNQAAAAAARxQAAAAAATpGFBQWAAAAACiHFRcAAAAAB3kpAAAAAAA6RgAAAAAIeQ1HAAAAAAh5AAAAAAEyAAAAAAh4SBQUAAAAAASHFgAAAAAChwIAAAACAAAWhhYAAAAAAocCAAAAAgAAFoYWAAAAAAV5OgAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFBQUGBUAAAAAAnk3UAAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFBcAAAAACXgVAAAAAAp4FQAAAAAEeQAAAAAAFjI2UAAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFB4AAAAAC3iDFIMWAAAAAAU6RhQURw1IgxYAAAAABXk6RhQURw1IgxQAAAAAIIcUAAAAAAGHFhQCAAAAAwAAABaGAAAAAAE6RhQUAAAAAAAAAAAAAwAAAAABeUcUAAAAAAI6RhQUAAAAAAAAAAAAAwAAACcQAAAAAAF5M0cUAAAAAAM6RhQUAAAAAAAAAAAAAwAAACcQAAAAAAR5M0cUAAAAAAA6RhQUAAAAAAEAAAAAAQAAACcQAAAAAAR5M0dISEhIGAAAAAADeV4ZNkYzRxQUAAAAAABIAAAAAAN5MjQXFgAAAAADeQAAAAALeRk2RjNHFBQAAAAAAEgyFgAAAAAKeQAAAAAAFjIAAAAABHk6RhQUAAAAAAl5NAAAACcQNQAAACcQAAAAAAR5MwAAACcQAAAAAAl5MzQAAAAnEDUyRxYzMkhHFAAAAAACOkYUFBQAAAAAKIcVFwAAAAAHeSkAAAAAADpGAAAAAAh5DUcAAAAACHkAAAAAATIAAAAACHhIFBQAAAAABIcWAAAAAAKHAgAAAAIAABaGFgAAAAAChwIAAAACAAAWhhYAAAAABXk6AAAAAAA6RgAAAAAIeQ1HAAAAAAh5AAAAAAEyAAAAAAh4SBQUFBQYFQAAAAACeTdQAAAAAAA6RgAAAAAIeQ1HAAAAAAh5AAAAAAEyAAAAAAh4SBQUFwAAAAAMeB4eAAAAACiHFRcAAAAAB3kpAAAAAAA6RgAAAAAIeQ1HAAAAAAh5AAAAAAEyAAAAAAh4SBQUAAAAAASHFgAAAAAChwIAAAACAAAWhhYAAAAAAocCAAAAAgAAFoYWAAAAAAV5OgAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFBQUGBUAAAAAAnk3UAAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFBcAAAAADXgWHxkZNkYWFEcUSB4ZNkYURxYUSB8zAAAAAAZ5AAAAAAI1NwAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFB86UBcUFAAAAAAMeQAAAAANeTpQFxQUUgAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFAAAAAAAAAAPQkAAAA9CQDIAAAAAAEcUAAAAAAM6RhQUFAAAAAAohxUXAAAAAAd5KQAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFAAAAAAEhxYAAAAAAocCAAAAAgAAFoYWAAAAAAKHAgAAAAIAABaGFgAAAAAFeToAAAAAADpGAAAAAAh5DUcAAAAACHkAAAAAATIAAAAACHhIFBQUFBgVAAAAAAJ5N1AAAAAAADpGAAAAAAh5DUcAAAAACHkAAAAAATIAAAAACHhIFBQXXgAAAAAGeTM2AAAAAAA6RgAAAAAIeQ1HAAAAAAh5AAAAAAEyAAAAAAh4SBQUFAAAAAAGeTUAAAAAATIeAAAAAAN5XjMfAAAAJxAAAAAABHkzRxQAAAAABDpGFBSDFIMWAAAAAAU6RhQURw1IgxYAAAAABXk6RhQURw1IgxQAAAAAIIcUAAAAAAGHFhQCAAAAAwAAABaGAAAAAAA6RgAAAAADeQAAAAAGeTIAAAAH0DIAAAAAAAAAACcQAAAAAAR5M0cAAAAABnkAAAAAAQAAACcQAAAAAAR5M0hHFEhISEhIAAAAJxA0AAAAAAR5AAAAJxAyNQs="));
    } else if (direction == 2) {
        a = string_to_array(atob("AAAAAAAAAAAAAXgA"));
        f = string_to_array(atob("AAAAAAd4AAAAAMgAAAAACHgWAAAAAAA6RhQUAAAAAAZ5FV4WNQAAAAAARxQAAAAAATpGFBQWAAAAACiHFRcAAAAAB3kpAAAAAAA6RgAAAAAIeQ1HAAAAAAh5AAAAAAEyAAAAAAh4SBQUAAAAAASHFgAAAAAChwIAAAACAAAWhhYAAAAAAocCAAAAAgAAFoYWAAAAAAV5OgAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFBQUGBUAAAAAAnk3UAAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFBcAAAAACXgVAAAAAAp4FQAAAAAEeQAAACcQFjM3UAAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFB4AAAAAC3iDFIMWAAAAAAU6RhQURw1IgxYAAAAABXk6RhQURw1IgxQAAAAAIIcUAAAAAAGHFhQCAAAAAwAAABaGAAAAAAE6RhQUAAAAAAAAAAAAAwAAAAABeUcUAAAAAAI6RhQUAAAAAAAAAAAAAwAAACcQAAAAAAF5M0cUAAAAAAM6RhQUAAAAAAAAAAAAAwAAACcQAAAAAAR5M0cUAAAAAAA6RhQUAAAAAAEAAAAAAQAAACcQAAAAAAR5M0dISEhIGAAAAAADeV4ZNkYzRxQUAAAAAABIAAAAAAN5MjQXFgAAAAADeQAAAAALeRk2RjNHFBQAAAAAAEgyFgAAAAAKeQAAACcQFjMAAAAABHk6RhQUAAAAAAl5NAAAACcQNQAAACcQAAAAAAR5MwAAACcQAAAAAAl5MzQAAAAnEDUyRxYzMkhHFAAAAAACOkYUFBQAAAAAKIcVFwAAAAAHeSkAAAAAADpGAAAAAAh5DUcAAAAACHkAAAAAATIAAAAACHhIFBQAAAAABIcWAAAAAAKHAgAAAAIAABaGFgAAAAAChwIAAAACAAAWhhYAAAAABXk6AAAAAAA6RgAAAAAIeQ1HAAAAAAh5AAAAAAEyAAAAAAh4SBQUFBQYFQAAAAACeTdQAAAAAAA6RgAAAAAIeQ1HAAAAAAh5AAAAAAEyAAAAAAh4SBQUFwAAAAAMeB4eAAAAACiHFRcAAAAAB3kpAAAAAAA6RgAAAAAIeQ1HAAAAAAh5AAAAAAEyAAAAAAh4SBQUAAAAAASHFgAAAAAChwIAAAACAAAWhhYAAAAAAocCAAAAAgAAFoYWAAAAAAV5OgAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFBQUGBUAAAAAAnk3UAAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFBcAAAAADXgWHxkZNkYWFEcUSB4ZNkYURxYUSB8zAAAAAAZ5AAAAAAI1NwAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFB86UBcUFAAAAAAMeQAAAAANeTpQFxQUUgAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFAAAAAAAAAAPQkAAAA9CQDIAAAAAAEcUAAAAAAM6RhQUFAAAAAAohxUXAAAAAAd5KQAAAAAAOkYAAAAACHkNRwAAAAAIeQAAAAABMgAAAAAIeEgUFAAAAAAEhxYAAAAAAocCAAAAAgAAFoYWAAAAAAKHAgAAAAIAABaGFgAAAAAFeToAAAAAADpGAAAAAAh5DUcAAAAACHkAAAAAATIAAAAACHhIFBQUFBgVAAAAAAJ5N1AAAAAAADpGAAAAAAh5DUcAAAAACHkAAAAAATIAAAAACHhIFBQXXgAAAAAGeTM2AAAAAAA6RgAAAAAIeQ1HAAAAAAh5AAAAAAEyAAAAAAh4SBQUFAAAAAAGeTUAAAAAATIeAAAAAAN5XjMfAAAAJxAAAAAABHkzRxQAAAAABDpGFBSDFIMWAAAAAAU6RhQURw1IgxYAAAAABXk6RhQURw1IgxQAAAAAIIcUAAAAAAGHFhQCAAAAAwAAABaGAAAAAAA6RgAAAAADeQAAAAAGeTIAAAAH0DIAAAAAAAAAACcQAAAAAAR5M0cAAAAABnkAAAAAAQAAACcQAAAAAAR5M0hHFEhISEhIAAAAJxA0AAAAAAR5AAAAJxAyNQs="));
    } else {
        console.log("that is an invalid direction");
        console.log(direction);
        return("invalid direction to bet");
    }
    console.log("market oid is ");
    console.log(oid);
    var g = a.concat(
        integer_to_array(bet_height, 4)).concat(
            a2).concat(
                integer_to_array(expires, 4)).concat(
                    b).concat(
                        integer_to_array(maxprice, 4)).concat(
                            c).concat(
                                string_to_array(atob(oid))).concat(
                                    d).concat(
                                        integer_to_array(period, 4)).concat(
                                            e).concat(
                                                string_to_array(atob(server_pubkey))).concat(
                                                    f);
    console.log("compiled contract");
    console.log(JSON.stringify(g));
    var contract =  btoa(array_to_string(g));
    var codekey = ["market", 1, oid, expires, server_pubkey, period, oid];
    var amount2 = Math.floor(amount * ((10000 + maxprice) / 10000));
    return ["bet", contract, amount2, codekey, [-7, direction, maxprice]]; //codekey is insttructions on how to re-create the contract, so we can do pattern matching when updating channels.
}

function market_trade(cd, amount, price, bet, oid) { //oid unused
    var market_spk = cd.me;
    console.log("market trade spk before ");
    console.log(JSON.stringify(market_spk));
    var cid = market_spk[6];
    var time_limit = 10000;//actually constants:time_limit div 10
    var space_limit = 100000;
    var cGran = 10000;
    var a = Math.floor((amount * price) / cGran);
    market_spk[3][0] = bet;
    market_spk[3] = ([-6]).concat(market_spk[3]);//add new bet to front
    market_spk[8] = market_spk[8] + 1; //nonce
    market_spk[5] = market_spk[5] + time_limit;// time_gas/10
    market_spk[4] = Math.max(market_spk[4], space_limit); //space_gas
    market_spk[7] = market_spk[7] - a; //amount
    console.log("market trade spk after ");
    console.log(JSON.stringify(market_spk));
    return market_spk;
}

function chalang_main() {
    const word_size = 4294967296,
          hash_size = 12;
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
        return h.slice(0, 12);
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
        definition = d.funs[code_hash];
        var s = definition.length;
        d.stack = d.stack.slice(1);
        d = run2(definition, d);
        return {i: i, d: d, g: (s + 10), s: "slow call op", r: (s - 1)};
    }
    op_code[ops.define] = function(i, code, d) {
        var skipped_size = count_till(code, i, ops.fun_end);
        var definition = code.slice(i+1, i+skipped_size);
        i += skipped_size;
        var hash_array = small_hash(definition);
        var b = btoa(array_to_string(hash_array));
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
        underflow_check(d, 2, "append");
        var a;
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
        var x = run5(variable_contract, d);
        //var x = run5(function_contract, d);
        //var x = run5(map_contract, d);
        console.log(JSON.stringify(x.stack));
        return x.stack;
    }
    function new_state(height, slash) {
        return{"name": "state", "height": height, "slash": slash};
    }
    function data_maker(op_gas, ram_gas, many_vs, many_funs, script_sig, code, state) {
        var arr = [];
        arr.length = many_vs;
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


function spk_main() {
    const ops = chalang_object.ops();
    function prove_facts(facts, callback) {
        if (JSON.stringify(facts) == JSON.stringify([])) {
            return callback([ops.empty_list]);
        }
        return prove_facts2(facts, 1, [ops.empty_list], callback); // [
    }
    var tree2id = {accounts: 1, channels: 2, existence: 3, burn: 4, oracles: 5, governance: 6};
    function prove_facts2(facts, i, r, callback) {
	var ops = chalang_object.ops();
        if (i == facts.length) {
            r.concat([ops.reverse]); // converts a , to a ]
            return callback(r);
        }
        //console.log("prove facts 2");
        //console.log(JSON.stringify(facts));
        var tree = facts[i][0];
        var key = facts[i][1];
	//var key = hash(string_to_array(atob(facts[i][1])));
        merkle.request_proof(tree, key, function(value) {
            //var value = merkle.verify(key, proof);
            //we are making chalang like this:
            //[ int id, key, binary size serialized_data ]
            // '[', ']', and ',' are macros for making a list.
            var id = tree2id[tree];
            r = r.concat([ops.empty_list]); // [
            r = r.concat([0]).concat(integer_to_array(id, 4));
            r = r.concat([ops.swap, ops.cons]); // ,
            if (Number.isInteger(key)) {
                r = r.concat([0]);
                r = r.concat(integer_to_array(key, 4));
            } else {
		key = string_to_array(atob(key));
                r = r.concat([2]);
                r = r.concat(integer_to_array(key.length, 4));
                r = r.concat(key);
            }
            r = r.concat([ops.swap, ops.cons]); // ,
            var serialized_data = merkle.serialize(value, key);//this is the serialized version of the thing who's existence we are proving. make it from value.
            var s = serialized_data.length;
            r = r.concat([2]).concat(integer_to_array(s, 4));
            r = r.concat(serialized_data);
            r = r.concat([ops.swap, ops.cons, ops.reverse]); // ]
            r = r.concat([ops.swap, ops.cons]); // ,
            return prove_facts2(facts, i+1, r, callback);
        });
        //return r.concat([ops.reverse]); // converts a , to a ]
    }
    function spk_run(mode, ss0, spk0, height, slash, fun_limit, var_limit, callback) {//mode unused
        var spk = JSON.parse(JSON.stringify(spk0));
        var ss = JSON.parse(JSON.stringify(ss0));
        var state = chalang_object.new_state(height, slash);
        //var key1 = "fun_limit";
        var ret;
        if (!(ss.length == (spk[3].length - 1))) {//spk[3] == bets is formated with a -6 in front for packer.erl
            console.log(JSON.stringify(ss));
            console.log(JSON.stringify(spk));
            throw("ss and bets need to be the same length");
        }
        spk_run2(ss, spk[3], spk[5], spk[4], fun_limit, var_limit, state, spk[9], spk[8], 0, 1, function(ret) {
            return callback(ret);
        });
    }
    function spk_run2(ss, bets, opgas, ramgas, funs, vars, state, delay, nonce, amount, i, callback) {
        if (i > (ss.length)) {
            return callback({"amount": amount, "nonce": nonce, "delay": delay});//, "opgas": opgas});
        }
        spk_run3(ss[i-1], bets[i], opgas, ramgas, funs, vars, state, function(run_object) {
            if (!(Number.isInteger(run_object.nonce))) {
                console.log(JSON.stringify(run_object.nonce));
                throw("nonce should be an integer");
            }
            return spk_run2(ss, bets, opgas, ramgas, funs, vars, state,
                            Math.max(delay, run_object.delay),
                            nonce + run_object.nonce,
                            amount + run_object.amount,
                            i+1, callback);
        });
    }
    function spk_run3(ss, bet, opgas, ramgas, funs, vars, state, callback) {
	console.log("spk run 3 ss is ");
	console.log(JSON.stringify(ss));
	//{"code":[2,0,0,0,32,175,20,235,211,57,38,228,113,95,134,170,11,54,51,95,61,134,20,89,119,227,76,113,166,247,85,51,203,81,88,170,5],"prove":[-6,-6],"meta":[-6,-6]} //prove should only have one -6
        var script_sig = ss.code;
        if (!(chalang_none_of(script_sig))) {
            throw("error: return op in the script sig");
        }
	console.log("spk run3");
	console.log(JSON.stringify(ss.prove));
        prove_facts(ss.prove, function(f) {
            var c = string_to_array(atob(bet[1]));
            //var c = bet.code;
	    //console.log("spk run3 f is ");
	    //console.log(f);
	    //console.log("and c is ");
	    //console.log(c);
            var code = f.concat(c);
            var data = chalang_object.data_maker(opgas, ramgas, vars, funs, script_sig, code, state);
            var data2 = chalang_object.run5(script_sig, data);
            var data3 = chalang_object.run5(code, data2);
            //console.log("just ran contract, stack returned as ");
            //console.log(JSON.stringify(data3.stack));
            //console.log("bet was ");
            //console.log(JSON.stringify(bet));
            var amount = data3.stack[0] | 0;//This should be a signed integer, but for some reason the integer is being stuck into a 32 byte unsigned value, so -2000 becomes 4294965296
            var nonce = data3.stack[1];
            var delay = data3.stack[2];
            var cgran = 10000; //constants.erl
	    console.log(amount);
            if ((amount > cgran) || (amount < -cgran)) {
                throw("you can't spend money you don't have in the channel.");
            }
            //var a3 = Math.floor(amount * bet.amount / cgran);
            var a3 = Math.floor(amount * bet[2] / cgran);
            return callback({"amount": a3, "nonce": nonce, "delay": delay, "opgas": data3.opgas});
        });
    }
    function spk_force_update(spk0, ssold0, ssnew0, fun_limit, var_limit, callback) {
        var spk = JSON.parse(JSON.stringify(spk0));
        var ssold = JSON.parse(JSON.stringify(ssold0));
	if (ssold[0] == -6) {
	    ssold = ssold.slice(1);
	}
        var ssnew = JSON.parse(JSON.stringify(ssnew0));
        console.log("force update");
	console.log(JSON.stringify(ssold));
	console.log(JSON.stringify(ssnew));//double -6
        var height = headers_object.top()[1];
        var ret;
        spk_run("fast", ssold, spk, height, 0, fun_limit, var_limit, function(ran1) {
            var nonceOld = ran1.nonce;
            spk_run("fast", ssnew, spk, height, 0, fun_limit, var_limit, function(ran2) {
                var nonceNew = ran2.nonce;
                if (!(nonceNew < nonceOld)) {
                    spk_force_update2(spk[3], ssnew, height, function(updated) {
                        spk[3] = updated.new_bets;
                        spk[7] += updated.amount;
                        spk[8] += updated.nonce;
                        console.log("force udpate final ss is ");
                        console.log(JSON.stringify(updated.newss));//failing to remove the ss.
                        console.log("force udpate final spk is ");
                        console.log(JSON.stringify(spk));//succeeds to remove the bet.
			console.log("updated is ");
			console.log(JSON.stringify(updated));
                        return callback({"spk":spk, "ss":updated.newss});
                    });
                } else {
		    console.log(JSON.stringify([nonceNew, nonceOld]));
		    console.log(JSON.stringify([ssnew, ssold]));
                    console.log("spk force update had nothing to do.");
                    return callback(false);
                }
            });
        });
    }
    function chalang_none_of(c) {
        console.log("none of");
        var n;
        for (var i = 0; i < c.length; i++) {
            if ( c[i] == ops.finish ) {
                return false;
            } else if ( c[i] == ops.int_op ) {
                i += 4
            } else if ( c[i] == ops.binary_op ) {
                n = array_to_int(c.slice(i+1, i+5));
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
        var bet_gas_limit = 100000;//same as bet_unlock2
        var cgran = 10000; //constants.erl
	console.log("spk force update 2 compare bets and ss");
	console.log(JSON.stringify(ss));//no -6 to start
	console.log(JSON.stringify(bets));//starts with -6
        spk_force_update22(bets, ss, height, amount, nonce, new_bets, newss, fun_limit, var_limit, bet_gas_limit, bets.length-1, callback);
    }
    function spk_force_update22(bets, ss, height, amount, nonce, new_bets, newss, fun_limit, var_limit, bet_gas_limit, i, callback) {
        //console.log("spke force update 22");
        if (i < 1) {
            return callback({"new_bets": new_bets, "newss": newss, "amount": amount, "nonce": nonce});
        }
        var b = chalang_none_of(ss[i-1].code);//ss.code
        if (!(b)) {
            throw("you can't put return op into the ss");
        }
        var state = chalang_object.new_state(height, 0);
        prove_facts(ss[i-1].prove, function(f) { //PROBLEM HERE
            //var code = f.concat(bets[i].code);
            var code = f.concat(string_to_array(atob(bets[i][1])));
	    console.log("spk force update 22. code is ");
	    console.log(JSON.stringify(code));
            var data = chalang_object.data_maker(bet_gas_limit, bet_gas_limit, var_limit, fun_limit, ss[i-1].code, code, state);
            var data2 = chalang_object.run5(ss[i-1].code, data);
            var data3 = chalang_object.run5(code, data2);
            var s = data3.stack;
            var cgran = 10000; //constants.erl
	    console.log("ran code stack is ");
	    console.log(JSON.stringify(s));
            /*
console.log(JSON.stringify([
                //"code", code,
                "ss", ss[i],
                "data2", data2.stack,
                "data3", data3.stack,
                "delay", s[2],
                "amount", s[0],
                "nonce", s[1]]));
*/
            if (!(s[2] > 0)) { //if the delay is long, then don't close the trade.
		console.log("short delay, close the trade.");
                if (s[0] > cgran) {
                    throw("you can't spend money that you don't have");
                }
		console.log("update amount");
		console.log(JSON.stringify([s[0], bets[i][2], cgran]));
                amount += Math.floor(s[0] * bets[i][2] / cgran);
                nonce += s[1];
                new_bets = new_bets.slice(0, i).concat(new_bets.slice(i+1, new_bets.length));
                newss = newss.slice(0, i-1).concat(newss.slice(i, newss.length));
            } else {
		console.log("long delay, do not close the trade.");
	    }
            return spk_force_update22(bets, ss, height, amount, nonce, new_bets, newss, fun_limit, var_limit, bet_gas_limit, i-1, callback); 
        });
    }
    function ss_to_internal(ess) {
        var ss = [];
        for (var i = 1; i < ess.length; i++) {
	    if (JSON.stringify(ess[i][2]) ==
		JSON.stringify([-6, -6])) {
		ess[i][2] = [-6];
		ess[i][3] = [-6];
		//throw("ss to internal broken");
	    }
            ss = ss.concat([channels_object.new_ss(string_to_array(atob(ess[i][1])), ess[i][2], ess[i][3])]);
	}
	console.log("ss to internal ss is ");
	console.log(JSON.stringify(ss));
        return ss;
    }
    function channel_feeder_they_simplify(from, themspk, cd, callback) {
        cd0 = channels_object.read(from);
        //true = cd0.live; //verify this is true
        //true = cd.live; //verify this is true
        var spkme = cd0.me;
        var ssme = cd0.ssme;
        //console.log("ssme is ");
        //console.log(JSON.stringify(ssme));
        //verify that they signed themspk
        var newspk = themspk[1];
        //console.log("spkme is ");
        var newspk2 = cd[1];
        if (!(JSON.stringify(newspk) == JSON.stringify(newspk2))) {
            console.log(JSON.stringify(newspk));
            console.log(JSON.stringify(newspk2));
            throw("spks they gave us do not match");
        }
        var ss = ss_to_internal(cd[3]);
        var ss4 = ss_to_internal(cd[4]);//this one looks weird
        merkle.request_proof("governance", 14, function(tree_fun_limit) {
            var fun_limit = tree_number_to_value(tree_fun_limit[2]);
            merkle.request_proof("governance", 15, function(tree_var_limit) {
                var var_limit = tree_number_to_value(tree_var_limit[2]);
                spk_force_update(spkme, ssme, ss4, fun_limit, var_limit, function(b2) {
                    var cid = cd[7];
		    var expiration = cd[7];
                    console.log("are we able to force update?");
                    console.log(JSON.stringify([b2, {"spk": newspk, "ss": ss}]));
                    if ( JSON.stringify(b2) == JSON.stringify({"spk": newspk, "ss": ss})) {
                        var ret = keys.sign(newspk);
                        var newcd = channels_object.new_cd(newspk, themspk, ss, ss, expiration, cid);
                        channels_object.write(from, newcd);
			ss4_text = document.createElement("h8");
			ss4_text.innerHTML = JSON.stringify(ss4);
			document.body.append(ss4_text);
			//append ss4 to the document somewhere.
                        return callback(ret);
                    } else {
                        is_improvement(spkme, ssme, newspk, ss, fun_limit, var_limit, function(b3) {//maybe ss should be ss4?
                            if ( b3 ) {
                                //If they give free stuff, then accept.
                                ret = keys.sign(newspk);
                                var newcd = channels_object.new_cd(newspk, themspk, ss, ss, expiration, cid);
                                channels_object.write(from, newcd);
                                return callback(ret);
                            } else {
                                //console.log("channel feeder they simplify had nothing to do");
                                //return callback(false);
                                //this part is only used for lightning.
                                channel_feeder_simplify_helper(from, ss4, function(sh) {
				    if (sh.ss == undefined) {
					throw "error, should be defined.";
				    }
                                    var ss5 = sh.ss;
                                    var ret = sh.spk;
                                    var spk = themspk[1];
                                    var spk2 = ret[1];
                                    if (!( JSON.stringify(spk) == JSON.stringify(spk2))) {
					console.log(JSON.stringify(spk));
					console.log(JSON.stringify(spk2));//still has the bet
					console.log("spks do not match");
                                    } else {
					var data = channels_object.new_cd(spk, themspk, ss5, ss5, expiration, cid);
					channels_object.write(from, data);
					return callback(ret);
                                    }
				});
                            }
                        });
                    }
                });
            });
        });
    }
    function channel_feeder_simplify_helper(from, ss, callback) {
	var cd = channels_object.read(from);
	var spk = cd.me;
	var bet_unlock_object = spk_bet_unlock(spk, ss, function(bet_unlock_object) {

	    var ret = keys.sign(bet_unlock_object.spk);
	    return callback({ss: bet_unlock_object.newss, spk: ret});
	});
    }
    function is_improvement(old_spk, old_ss, new_spk, new_ss, fun_limit, var_limit, callback) {
        //get height
        //check that space gas and time limit are below or equal to what is in the config file.
	var height = headers_object.top()[1];
        if (new_spk[4] > 100000) {//space gas
            console.log("this contract uses too much space.");
            return callback(false);
        }
        if (new_spk[5] > 100000) {//time gas
            console.log("this contract uses too much time");
            return callback(false);
        }
        spk_run("fast", new_ss, new_spk, height, 0, fun_limit, var_limit, function(run2) {
            var nonce2 = run2.nonce;
            var delay2 = run2.delay;
            spk_run("fast", old_ss, old_spk, height, 0, fun_limit, var_limit, function(run1) {
                var nonce1 = run1.nonce;
                var delay1 = run1.delay;
		if (((nonce1 == nonce2) && (delay1 == 0)) && (delay2 == 0)) {
		} else if (!(nonce2 > nonce1)) {
		    console.log(JSON.stringify([new_ss, old_ss]));
		    console.log(JSON.stringify([nonce2, nonce1]));
		    console.log(JSON.stringify([delay2, delay1]));
		    console.log("the new spk can't produce a lower nonce than the old.");
		    return callback(false);
                }
                var old_bets = old_spk[3];
                var old_amount = old_spk[7];
                old_spk[3] = new_spk[3];
                old_spk[5] = new_spk[5];//time gas tg;
                old_spk[4] = new_spk[4];//space gassg;
                old_spk[7] = new_spk[7];
                old_spk[8] = new_spk[8];
                if (!(JSON.stringify(old_spk) == JSON.stringify(new_spk))) {
                    console.log("spk was changed in unexpected ways");
		    console.log(JSON.stringify(old_spk));
		    console.log(JSON.stringify(new_spk));
                    return callback(false);
                }
                var cid = new_spk[6];
                var ret = false;
                merkle.request_proof("channels", cid, function(channel) {
                    //variable_public_get(["proof", btoa("channels"), cid, btoa(array_to_string(top_hash))], function(proof) {
                    //var channel = merkle.verify(cid, proof);
                    var acc1 = channel[2]
                    var acc2 = channel[3]
                    var profit;
                    if (keys.pub() == acc1) {
                        profit = new_spk[7] - old_amount;
                    } else {
                        profit = old_amount - new_spk[7];
                    }
                    var bets2 = new_spk[3];
                    if ((JSON.stringify(old_bets) == JSON.stringify(bets2)) && (profit > 0)) {
                        //if they give us money for no reason, then accept.
			console.log("the server sent us money.");
			return callback(true);
                    }
		    var many_new_bets = new_spk[3].length - old_bets.length;
                    if ((!(profit < 0)) && //costs nothing
                        (many_new_bets > 0)) { //increases number of bets
	                //if we have the same or greater amount of money, and they make a bet that possibly gives us more money, then accept it.
                        //var t = bets2.slice(1);
                        var t = [-6].concat(bets2.slice(1+many_new_bets));
                        if (!(JSON.stringify(t) ==
			      JSON.stringify(old_bets))) {
			    console.log("t is ");
			    console.log(JSON.stringify(t));
			    console.log("old bets");
			    console.log(JSON.stringify(old_bets));
                            console.log("update improperly formatted");
			    return callback(false);
                        }
			for (var i = 1; i < many_new_bets + 1; i++) {
                            var new_bet = bets2[i];
                            var betAmount = new_bet[2];
                            var potentialGain;
                            if (keys.pub() == acc1) {
				potentialGain = -betAmount;
                            } else if (keys.pub() == acc2) {
				potentialGain = betAmount;
                            } else {
				console.log("error, this spk isn't for your pubkey");
				return callback(false);
                            }
                            if (!(potentialGain > 0)) {
				console.log(potentialGain);
				console.log(betAmount);
				console.log(JSON.stringify(new_bet));
				console.log(JSON.stringify(bets2));
				console.log("error, this could make us lose money.");
				return callback(false);
                            }
			}
                        var obligations1 = spk_obligations(1, bets2);
                        var obligations2 = spk_obligations(2, bets2);
                        var channelbal1 = channel[4];
                        var channelbal2 = channel[5];
                        if (obligations1 > channelbal1) {
                            console.log("acc1 doesn't have enough money in the channel to make that bet");
			    return callback(false);
                        }
                        if (obligations2 > channelbal2) {
                            console.log("acc2 doesn't have enough money in the channel to make that bet");
			    return callback(false);
                        }
			console.log("successfully updated channel. They made a contract which costs nothing, and might give us money.");
			return callback(true);
                    }
		    console.log("this contract that the server offers might cost us something, so we refuse.");
		    return callback(false);
                });
            });
        });
    }
    function spk_obligations(n, bets) {
	if (n == 1) {
	    return spk_obligations1(bets);
	} else if (n == 2) {
	    return spk_obligations2(bets);
	}
    }
    function spk_obligations1(bets) {
	var c = 0;
	for (i = 1; i < bets.length; i++) {
	    var b = bets[i][2];
	    if (b > 0) { c += b; }
	}
	return c;
    }
    function spk_obligations2(bets) {
	var c = 0;
	for (i = 1; i < bets.length; i++) {
	    var b = bets[i][2];
	    if (b < 0) { c -= b; }
	}
	return c;
    }
    function api_decrypt_msgs(ms) {//list ms starts with -6
	console.log("msgs to decrypt");
	console.log(JSON.stringify(ms));
	for (var i = 1; i < ms.length; i++){
	    var emsg = ms[i];
	    console.log("about to decrypt this ");
	    console.log(JSON.stringify(emsg));
	    var dec = keys.decrypt(emsg);
	    console.log("decrypted this ");
	    console.log(JSON.stringify(dec));
	    var secret = dec[1];
	    var code = dec[2];
	    var amount = dec[3];
	    secrets_object.add(code, secret, amount);
	}
	return true;
    }
    function pull_channel_state(callback) {
        //get their pubkey
        variable_public_get(["pubkey"], function(server_pubkey) {
            variable_public_get(["spk", keys.pub()], function(spk_return) {
                var cd = spk_return[1];
                var them_spk = spk_return[2];
		//we need to verify that they signed them_spk.
                //returns cd and them_spk
                var cd0 = channels_object.read(server_pubkey);
		//console.log("javascript channels object is ");
		//console.log(JSON.stringify(cd0));
                if (cd0 == undefined) {
                    console.log("you don't have a record of a channel with this server. Did you load your channel data file?");
		    console.log("attempting to trustfully download a copy of the channel state from the server. Warning, this can be a security vulnerability!");
		    var spk = them_spk[1];
		    var ss = ss_to_internal(cd[4]);
		    var expiration = cd[7];
		    var cid = spk[6];
		    var NewCD = channels_object.new_cd(spk, them_spk, ss, ss, expiration, cid);
		    channels_object.write(server_pubkey, NewCD);
		    return callback();
                }
		console.log("cd0 is ");
		console.log(JSON.stringify(cd0));
		/*
                  if (!(cd0.live == true)) {
                    var s = "this channel has been closed";
                    console.log(s);
                    throw(s);
                }
		*/
                channel_feeder_they_simplify(server_pubkey, them_spk, cd, function(ret) {
                    if (!(ret == false)) {
                        var msg2 = ["channel_sync", keys.pub(), ret];
			setTimeout(function(){ variable_public_get(msg2, function(foo) {}); },
				   0);
			setTimeout(function(){
                            api_decrypt_msgs(cd[5]);
                            api_bet_unlock(server_pubkey, function(x) {
				var cd2 = channels_object.read(server_pubkey);
				var ret2 = keys.sign(cd2.me);
				var msg3 = ["channel_sync", keys.pub(), ret2];
				setTimeout(function(){ variable_public_get(msg3, function(foo) {}); }, 2000);
				//variable_public_get(msg3, function(foo) {});
				
				return callback();
			    });
			}, 2000);
                    } else {
			console.log("channel feeder they simplify failed.");
		    }
                });
            });
        });
    }
    function api_bet_unlock(server_pubkey, callback) {
	//The javascript version can be much simpler than the erlang version, because each secret is only for one smart contract for us. We don't have to search for other contracts that use it.

	channel_feeder_bets_unlock(server_pubkey, function(secrets_junk){
	    secrets = secrets_junk.secrets;
	    // spk = secrets_junk.spk;
	    teach_secrets(secrets, 0, function(){
		variable_public_get(["spk", keys.pub()], function(spk_data) {
		    console.log("should sart with -6");
		    console.log(JSON.stringify(spk_data));
		    var them_spk = spk_data[2];
		    var x = channel_feeder_update_to_me(them_spk, server_pubkey);
		    callback(x);
		});
	    });
	});
    }
    function channel_feeder_bets_unlock(server_id, callback) {
        var cd = channels_object.read(server_id);
        /*
	  if (!(true == cd.live)) {
	    console.log(JSON.stringify(cd));
            console.log("this channel has been closed");
            throw("this channel was closed");
        }
	*/
	console.log("channel feeder bets unlock ");
	console.log(JSON.stringify(cd));
	    
        spk_bet_unlock(cd.me, cd.ssme, function(unlock_object) {
	    console.log("spk object bets are ");
	    console.log(JSON.stringify(unlock_object.spk[3]));
            cd.me = unlock_object.spk;//should be empty like newss
            cd.ssme = unlock_object.newss;
            cd.ssthem = unlock_object.ssthem;
	    channels_object.write(server_id, cd);
            return callback({"secrets":unlock_object.secrets,//incorrectly storing -6 in prove
			     "spk":unlock_object.spk});
	});
	/*
    {ok, CD0} = channel_manager:read(ID),
    true = CD0#cd.live,
    SPKME = CD0#cd.me,
    SSOld = CD0#cd.ssme,
    {NewSS, SPK, Secrets, SSThem} = spk:bet_unlock(SPKME, SSOld),
    NewCD = CD0#cd{me = SPK, ssme = NewSS, ssthem = SSThem},
    channel_manager:write(ID, NewCD),
    Out = {Secrets, SPK},
	*/

    }
    function teach_secrets(secrets, i, callback) {
	//secrets is a dictionary code -> [secret, amount]
	// send ["secret", Secret, Key]
	//talker:talk({learn_secret, ID, Secret, Code}, IP, Port),
	if (!(i < secrets.length)) {
	    return callback();
	}
	console.log(JSON.stringify(secrets[i]));//incorrectly storing -6 in prove.
        var msg = ["learn_secret", keys.pub(), channels_object.ss_to_external(secrets[i][1]), secrets[i][2]];
	console.log(JSON.stringify(msg));
	variable_public_get(msg, function() {
	    return teach_secrets(secrets, i+1, callback);
	});
    }
/*
	console.log("teaching a secret");
        for (var i = 0; i < secrets.length; i++) {
	    console.log(JSON.stringify(secrets[i]));
            var msg = ["learn_secret", keys.pub(), channels_object.ss_to_external(secrets[i][1]), secrets[i][2]];
	    console.log(JSON.stringify(msg));
	    variable_public_get(msg, function() { return; });
        }
        return "ok";
    }
*/
    function channel_feeder_update_to_me(sspk, from) {
	var myid = keys.pub();
	var spk = sspk[1];
	var acc1 = spk[1];
	var acc2 = spk[2];
	if (!(((myid == acc1) && (from == acc2))
	      || ((myid == acc2) && (from == acc1))) ){
	    console.log(JSON.stringify(spk));
	    console.log(JSON.stringify(acc1));
	    console.log(JSON.stringify(acc2));
	    console.log(JSON.stringify(myid));
	    console.log(JSON.stringify(from));
	    console.log("channel_feeder_update_to_me has incorrect accounts in the spk.");
	    return false;
	}
	console.log("about to sign");
	console.log(JSON.stringify(sspk));
	sspk2 = keys.sign(sspk);
	console.log("signed");
	console.log(JSON.stringify(sspk2));
	var b = verify_both(sspk2);
	if (!(b)) {
	    console.log("they didn't sign the spk");
	    return false;
	}
	cd = channels_object.read(from);
	if (!(JSON.stringify(cd.me) ==
	      JSON.stringify(sspk[1]))) {
	    console.log(JSON.stringify(cd.me));//bet should start with -6.//bet should be removed.
	    console.log(JSON.stringify(sspk[1]));
	    console.log("can't update to me if they aren't the same.");
	    return false;
	}
	cd.them = sspk
	cd.ssthem = cd.ssme
	channels_object.write(from, cd);
    }
    function spk_bet_unlock(spk, ssold, callback) {
	console.log("spk bet unlock spk is ");
	console.log(JSON.stringify(spk));
	console.log("spk bet unlock ssold is ");
	console.log(JSON.stringify(ssold));//[{code:, prove:, meta:}]

	var bets = spk[3];//starts with -6
        var remaining = JSON.parse(JSON.stringify(bets));
        var amount_change = 0;
        var ssremaining = JSON.parse(JSON.stringify(ssold));
        var secrets = [];
        var dnonce = 0;
        var ssthem = [];
	var i = ssold.length;
        var key, bet, f, ss, key_junk;
	
	return bet_unlock2(callback);


	function bet_unlock3(data, ss2, callback) {
	    console.log("bet_unlock3");
	    var s = data.stack;
	    var nonce2 = s[1];
	    var delay = s[2];
	    if (delay > 0) {
		console.log("delay > 0. keep the bet");
		console.log(delay);
		return bet_unlock2(callback);
	    } else {
		var cgran = 10000; //constants.erl
		var contract_amount = s[0] | 0; // changes contract_amount format so negative number work.
		if ((contract_amount > cgran) ||
		    (contract_amount < -cgran)) {
                    throw("you can't spend money you don't have in the channel.");
		}
		var a3 = Math.floor(contract_amount * bet[2] / cgran);
		var key = bet[3];
		remaining.splice(i+1, 1);
		ssremaining.splice(i, 1);
		amount_change += a3;
		secrets = ([["secret", ss2, key]]).concat(secrets);
		dnonce += nonce2;
		ssthem = ([ss2]).concat(ssthem);
		return bet_unlock2(callback);
	    }
	}
	//throw("can't use a for loop with asynch recursion inside.");
	function bet_unlock2(callback) {
	    i--;
	    if (i < 0) {
		//spk.bets = remaining;
		spk[3] = remaining;
		//spk.amount += amount_change;
		spk[7] += amount_change;
		spk[8] += dnonce;
		console.log("bet unlock 2");
		    console.log(JSON.stringify(remaining));
		    console.log(JSON.stringify(ssremaining));
		if (!(remaining.length ==
		      ssremaining.length + 1)) {
		    throw("bet unlock 2 lengths don't match");
		}
		var x =  {"newss": ssremaining,
			  "spk": spk,
			  "secrets": secrets,
			  "ssthem": ssthem};
		return callback(x);
	    }
        //for (i = ssold.length - 1; i > -1; i--) {
	    ss = ssold[i];
	    bet = bets[i+1];
            key = bet[3];
	    key_junk = secrets_object.read(key);
	    if (key_junk == undefined) {
		console.log("secrets object");
		console.log(JSON.stringify(secrets_object));
		console.log("key");
		console.log(key);
		console.log("we don't have a secret to unlock this contract");
		//ssremaining = ([ss]).concat(ssremaining);//doing nothing preservse the info.
		ssthem = ([ss]).concat(ssthem);
		//remaining = // doing nothing means preserving the info.
		return bet_unlock2(callback);
            } else {
		var ss2 = ss_to_internal([-6, key_junk[0]])[0];
		var amount = key_junk[1];
		var height = headers_object.top()[1];
		var state = chalang_object.new_state(height, 0);
		var fun_limit = 400;
		var var_limit = 10000;
		console.log("ss2");
		console.log(JSON.stringify(key_junk));
		console.log(key_junk[0]);
		console.log(JSON.stringify(ss2));
		var script_sig = ss2.code;
		if (!(chalang_none_of(script_sig))) {
		    throw("error: return op in the script sig");
		}
		prove_facts(ss.prove, function(f) {
		    var c = string_to_array(atob(bet[1]));
		    var code = f.concat(c);
		    var opgas = 100000;//should be 100 000. made it smaller to stop polluting the console.
		    var data = chalang_object.data_maker(opgas, opgas, var_limit, fun_limit, ss2.code, code, state);
		    var data2 = chalang_object.run5(script_sig, data);
		    var data3 = chalang_object.run5(code, data2);
		    console.log("data3");
		    console.log(JSON.stringify(data3));
		    if (data3.stack == undefined) {
		//try using SS#ss.code instead of SS2#ss.code.
			throw("working here");
		/*
		    Data4 = chalang:run5(SS#ss.code, Data),
                    %io:fwrite("spk bet_unlock2 chalang run fourth\n"),
		    Y = chalang:run5(Code, Data4),
		    case Y of
			{error, E2} ->
			    io:fwrite("bet unlock2 ERROR"),
			    bet_unlock2(T, [Bet|B], A, SSIn, [SS|SSOut], Secrets, Nonce, [SS|SSThem]);
			Z -> 
			    bet_unlock3(Z, T, B, A, Bet, SSIn, SSOut, SS, Secrets, Nonce, SSThem)
		    end;
		*/
		    } else {
			bet_unlock3(data3, ss2, callback)
		    }
		});
	    }
	}
    }
    /*
Bets = SPK#spk.bets,
    {Remaining, AmountChange, SSRemaining, Secrets, Dnonce, SSThem} = bet_unlock2(Bets, [], 0, SS, [], [], 0, []),
    {lists:reverse(SSRemaining),
     SPK#spk{bets = lists:reverse(Remaining),
	     amount = SPK#spk.amount + (AmountChange),
	     nonce = SPK#spk.nonce + Dnonce},
     Secrets, SSThem}.
bet_unlock2([], B, A, [], SS, Secrets, Nonce, SSThem) ->
    {B, A, SS, Secrets, Nonce, lists:reverse(SSThem)};
bet_unlock2([Bet|T], B, A, [SS|SSIn], SSOut, Secrets, Nonce, SSThem) ->
    Key = Bet#bet.key, 
    case secrets:read(Key) of
	<<"none">> -> 
            io:fwrite("no secret known\n"),
	    bet_unlock2(T, [Bet|B], A, SSIn, [SS|SSOut], Secrets, Nonce, [SS|SSThem]);
	{SS2, Amount} -> 
	    %Just because a bet is removed doesn't mean all the money was transfered. We should calculate how much of the money was transfered.
            io:fwrite("we have a secret\n"),
            TP = tx_pool:get(),
            Trees = TP#tx_pool.block_trees,
            Height = TP#tx_pool.height,
	    State = chalang_state(Height, 0, Trees),
	    {ok, FunLimit} = application:get_env(amoveo_core, fun_limit),
	    {ok, VarLimit} = application:get_env(amoveo_core, var_limit),
	    {ok, BetGasLimit} = application:get_env(amoveo_core, bet_gas_limit),
	    true = chalang:none_of(SS2#ss.code),
	    F = prove_facts(SS#ss.prove, Trees),
	    C = Bet#bet.code,
	    Code = <<F/binary, C/binary>>,
	    Data = chalang:data_maker(BetGasLimit, BetGasLimit, VarLimit, FunLimit, SS2#ss.code, Code, State, constants:hash_size()),
	    Data2 = chalang:run5(SS2#ss.code, Data),
	    Data3 = chalang:run5(Code, Data2),
	    case Data3 of
		{error, _E} -> 
		    io:fwrite("spk bet unlock, ss doesn't work\n"),
		    io:fwrite(packer:pack(SS2)),
		    io:fwrite("\n"),
                    %io:fwrite("spk bet_unlock2 chalang run third\n"),
		    Data4 = chalang:run5(SS#ss.code, Data),
                    %io:fwrite("spk bet_unlock2 chalang run fourth\n"),
		    Y = chalang:run5(Code, Data4),
		    case Y of
			{error, E2} ->
			    io:fwrite("bet unlock2 ERROR"),
			    bet_unlock2(T, [Bet|B], A, SSIn, [SS|SSOut], Secrets, Nonce, [SS|SSThem]);
			Z -> 
			    bet_unlock3(Z, T, B, A, Bet, SSIn, SSOut, SS, Secrets, Nonce, SSThem)
		    end;
		X -> 
                    if
                        is_integer(Amount) ->
                            true = (abs(Amount) == abs(Bet#bet.amount));
                        true -> ok
                    end,
                    bet_unlock3(X, T, B, A, Bet, SSIn, SSOut, SS2, Secrets, Nonce, SSThem)
	    end
    end.
bet_unlock3(Data5, T, B, A, Bet, SSIn, SSOut, SS2, Secrets, Nonce, SSThem) ->
    io:fwrite("spk bet_unlock3\n"),
    [<<ContractAmount:32>>, <<Nonce2:32>>, <<Delay:32>>|_] = chalang:stack(Data5),
   if
        Delay > 0 ->
	   io:fwrite("delay is "),
	   io:fwrite(integer_to_list(Delay)),
	   io:fwrite("delay >0, keep the bet.\n"),
	   bet_unlock2(T, [Bet|B], A, SSIn, [SS2|SSOut], Secrets, Nonce, [SS2|SSThem]);
       true -> 
	   io:fwrite("delay <1, remove it.\n"),
	   CGran = constants:channel_granularity(),
	   true = ContractAmount =< CGran,
	   A3 = ContractAmount * Bet#bet.amount div CGran,
	   Key = Bet#bet.key, 
	   bet_unlock2(T, B, A+A3, SSIn, SSOut, [{secret, SS2, Key}|Secrets], Nonce + Nonce2, [SS2|SSThem])
   end.
    */
    return {pull_channel_state: pull_channel_state, spk_run: spk_run};
}


var spk_object = spk_main();

//bet: code, amount, key, meta
//spk: acc1, acc2, bets, space_gas, time_gas, cid, amount, nonce, delay
//cd: me, them, ssme, ssthem, emsg, live, cid
//["market",1,1,3000,"BJjOADT/mMg0BsqQkCDcEb/ylv6W85wipEKrY3qV5z3XvVrNygvVoEXsA6tncAoMuyvMB5Prepzqql3zZ1sDjjo=",40,1]
//{market, 1, MarketID, Expires, Pubkey, Period, OID}.
function bets_main() {
    var div;
    var oadiv;
    function draw() {
	var bets_div = document.getElementById("bets_div");
	div = document.createElement("div");
	bets_div.appendChild(div);
	oadiv = document.createElement("div");
	bets_div.appendChild(oadiv);
    }
    function main() {
        variable_public_get(["pubkey"], outstanding_bets3);
    }
    function outstanding_bets3(server_pubkey) {
        var x = channels_object.read(server_pubkey);
	console.log("outstanding bets channel object bets are ");
	console.log(JSON.stringify(x.me[3].length));
	console.log(JSON.stringify(x.me[3]));
        var bets = x.me[3];
        var ssme = x.ssme;
        div.innerHTML = "";
        oadiv.innerHTML = "";
        var cancel_buttons = [];
        for (var i = 1; i < bets.length; i++) {
            var bet = bets[i];
            console.log("bet is ");
            console.log(bet);
            var oid = bet[3][6];
            var amount = bet[2];
            var order = document.createElement("h8");
            var outcome = "";
            var meta = bet[4];
            if (bet[4][1] == 1) {
                outcome = "true";
            } else if (bet[4][1] == 2) {
                outcome = "false";
            }
            console.log("making cancel orders button, ssme is");
            console.log(JSON.stringify(ssme));
            if ( JSON.stringify(ssme[i-1].code) == JSON.stringify([0,0,0,0,4]) ) {
                console.log("unmatched");
                //console.log(JSON.stringify([i, oid, amount, "unmatched", bet[4]]));
                order.innerHTML = "in market ".concat(oid).concat(" you have an open order to trade this many tokens ").concat(s2c(amount)).concat(", you are trading at this price: ").concat(parseFloat(((bet[4][2])/100), 10)).concat(", you are betting on outcome: ").concat(outcome);
                div.appendChild(order);
                var cancel_button = document.createElement("input");
                cancel_button.type = 'button';
                cancel_button.value = "cancel trade";
                div.appendChild(cancel_button);
                div.appendChild(document.createElement("br"));
                cancel_buttons.push(cancel_button);
            } else {
                console.log("matched");
                order.innerHTML = ("market ").concat(oid).concat("you win if the outcome is ").concat(outcome).concat("amount ").concat(s2c(amount));
                oadiv.appendChild(order);
                oadiv.appendChild(document.createElement("br"));
            }
        }
        for (var i = 0; i < cancel_buttons.length; i++) {
            (function(k){
                cancel_buttons[i].onclick = function() { cancel_trade(k+2, server_pubkey); };
                
            })(i);
        }
    }
    function cancel_trade(n, server_pubkey) {
        //the nth bet in the channel (starting at 1) is a unmatched trade that we want to cancel.
        var oldCD = channels_object.read(server_pubkey);
        var spk = oldCD.me;
        var ss = oldCD.ssme[n-2];
        //var sscode = ss[1];
        console.log("oldCD ssme, n");
        console.log(JSON.stringify([oldCD.ssme, n]));
        console.log("cancel trade ss is");
        console.log(JSON.stringify(ss));
        if (JSON.stringify(ss.code) == JSON.stringify([0,0,0,0,4])) {//this is what an unmatched trade looks like.
            var spk2 = remove_bet(n-1, spk);
	    spk2[8] += 1000000;
            var sspk2 = keys.sign(spk2);
            var msg = ["cancel_trade", keys.pub(), n, sspk2];
            variable_public_get(msg, function(x) {
                return cancel_trade2(x, sspk2, server_pubkey, n-2);
            });
        } else {
            console.log(ss);
            console.log("this trade has already been partially or fully matched. it cannot be canceled now.");
        }
    }
    function remove_bet(n, spk0) {
        var spk = JSON.parse(JSON.stringify(spk0));
        var bets = spk[3];
        var bet = bets[n];
        var bets2 = remove_nth(n, bets);
        var bet_meta = bet[4];
        var a;
        if (bet_meta == 0) {
        a = 0;
        } else {
            var bet_amount = bet[2];
            var cgran = 10000;
            var price = bet_meta[2];
            a = Math.floor((bet_amount * price) / cgran);
        }
        spk[3] = bets2;
        spk[7] = spk[7] + a;
        return spk;
    }
    function cancel_trade2(sspk2, sspk, server_pubkey, n) {
        var cd = channels_object.read(server_pubkey);
	console.log("cancel trade2, fail to verify this: ");
	console.log(JSON.stringify(sspk2));
	var bool = verify_both(sspk2);
	if (!(bool)) {
	    throw("cancel trade badly signed");
	}
        var spk = sspk[1];
        var spk2 = sspk2[1];
        if (!(JSON.stringify(spk) ==
              JSON.stringify(spk2))) {
            console.log("the server didn't calculate the same update as us");
            console.log(spk);
            console.log(spk2);
	    throw("cancel trade spk does not match");
	}
        cd.them = sspk2;
        cd.me = spk;
        cd.ssme = remove_nth(n, cd.ssme);
        cd.ssthem = remove_nth(n, cd.ssthem);
        channels_object.write(server_pubkey, cd);
        main();
    }
    function remove_nth(n, a) {
        var b = a.slice(0, n);
        var c = a.slice(n+1, a.length);
        return b.concat(c);
    }
    return {main: main, draw: draw};
}
bets_object = bets_main(); 

function lightning_main() {
    var secrets = {};
    function read(code) {
        return secrets[code];
    }
    function add(code, ss, amount) {
        //code should be base64 encoded?
        secrets[code] = [ss, amount];
    }

    function make(amount) {
        var s = Array.prototype.slice.call(elliptic.rand(32));
        var sh = hash(s);
        var ss_code = ([2, 0,0,0,32]).concat(s);
        var ss = channels_object.new_ss(ss_code, [], []);
        var code = [20,90,0,0,0,0,0,58,
                    70,
                      0,0,0,0,100,0,0,0,0,1,0,0,0,0,0,11,
                    71,72,20,20,
                    40,31,
                    58,22,20,22,20,
                    70,0,0,0,0,0,0,0,0,0,2,0,0,0,39,16,
                    71,0,0,0,0,100,0,0,0,0,1,0,0,0,0,0,
                    72,11];
        var a = ([2, 0,0,0,32]).concat(
            sh).concat([30]).concat(
                code);
        var contract = btoa(array_to_string(a));
        var codekey = "";
        var meta = [-6];
        return {ss: ss, bet: ["bet", contract, amount, codekey, meta]};
    };
    function test() {
        var m = make(10);
        var ss = m.ss;
        var cid = 1;
        var amount = 27000;
        var delay = 11;
        var spk = ["spk", 1, 2, [-6, m.bet], 10000,10000,cid, amount, 0, delay];
        var height = headers_object.top()[1];
        spk_object.spk_run("fast", [ss], spk, height, 0, 1000, 1000, function(ran) {
            console.log(JSON.stringify(ran));
        });
    };
    return {test: test, make: make, read: read, add: add};
}
var lightning_object = lightning_main();
//setTimeout(function() {
//    JSON.stringify(lightning_object.test());
//}, 500);

function encryption_main() {
    function aes_ctr(key) {
        return(new aesjs.ModeOfOperation.ctr(key, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]));
    };
    function bin_enc(key,bin){
        return Array.from(aes_ctr(key).encrypt(bin));
    };
    function bin_dec(key, bin){
        return Array.from(aes_ctr(key).decrypt(bin));
    };
    function shared(key, pub) {
	pub = pub.getPublic();
        return hex2array(key.derive(pub).toString(16));
    }
    function hex2array(x) {
        return string_to_array(fromHex(x));
    }
    function send(m, to_pub, fromkey) {
        var to = keys.ec().keyFromPublic(toHex(atob(to_pub)), "hex");
        var from_pub = btoa(fromHex(fromkey.getPublic("hex")));
        var newkey = keys.make();
        var eph_pub = hex2array(newkey.getPublic("hex"));
        var eph_priv = hex2array(newkey.getPrivate("hex"));
	console.log("signing on ");
	console.log(btoa(btoa(array_to_string(eph_pub))));
	console.log("signing with ");
	console.log(fromkey.getPublic("hex"));
        var msg = ["msg", btoa(array_to_string(sign(btoa(btoa(array_to_string(eph_pub))), fromkey))), m, btoa(from_pub)];
        var ss = shared(newkey, to);
	console.log("send message ");
	console.log(JSON.stringify(msg));
        var emsg = bin_enc(ss, string_to_array(JSON.stringify(msg)));
        return ["emsg", btoa(btoa(array_to_string(eph_pub))), btoa(btoa(array_to_string(emsg)))];
    };
    function get(emsg, my_key) {
        var eph_pub = atob(atob(emsg[1]));
	//console.log("get eph pub ");
	//console.log(eph_pub);
	//console.log(atob(emsg[1]));
        var eph_key = keys.ec().keyFromPublic(toHex(eph_pub), 'hex');
        var ss = shared(my_key, eph_key);
        var msg = JSON.parse(array_to_string(bin_dec(ss, string_to_array(atob(atob(emsg[2]))))));
	console.log("msg");
	console.log(JSON.stringify(msg));
        var fromkey = keys.ec().keyFromPublic(toHex(atob(atob(msg[3]))), 'hex');
	console.log("verify message received ");
	console.log(emsg[1]);
        //var b = verify(emsg[1], btoa(msg[1]), fromkey);
        var b = verify(emsg[1], msg[1], fromkey);
        if (b) { return msg[2]
        } else { throw("encryption get error");
        }
    }
    function assert_eq(x, y) {
        if (!(JSON.stringify(x) == JSON.stringify(y))) {
            console.log("failed assert_eq");
            console.log(JSON.stringify(x));
            console.log(JSON.stringify(y));
            throw("failed assert_eq");
        };
    }
    function test() {
        var pub1_64 = "BNFsD42eXjwHd4PyP4lODu+aybYjmVJF0bA0UYNcJ2/cELBl5Z6IA639jUl1km+8YAD3aL3of+SqLI8emuhsa2c=";
        var priv1_64 = "wruxx99+2hK4cT+j1SkJhV6VzBxgbl11iHxnq6ghASA=";
        var pub2_64 = "BA7HtKYPIvUwQjmUqNQ0UMsxHu+KtISveg45Jl+tl/y6OMgtyC3rE4/YrEHLtTprCPsxcus5CbhmlWo9IDKfnzo=";
        var priv2_64 = "4a6E2IK3hhhP2dK8xGYaUqg23Fk/n/Ms2VuORKC5Xvo=";
        var key1 = keys.ec().keyFromPrivate(toHex(atob(priv1_64)), "hex");
        var key2 = keys.ec().keyFromPrivate(toHex(atob(priv2_64)), "hex");
        var sm = send([-6, 1, 2, 3], btoa(fromHex(key2.getPublic("hex"))), key1);
        var sm2 = ["emsg","QlBKTURaYTZHTEdBc2FuM2Y5c3pjR0tja29KblVoLyt3NE92c21kT0hDa3pEcjlESlRDVmhHTFlqNWdINnhSYmszSlFkbzdRZ2ttZHByQVlVbDBiZkpBPQ==","ejFEWmM0TDEyY1g3Q3F0Q0ZZK3NETHptTld4UFhTeFpKSVAwUk9BZkZqWFVLRGUwQkdDMGl2ZFQ2Rk9IT0ZtTHJPSGRwbit0bWpKYzNjYzlKdEFLQW5aY3kxRVBmekNTSTRONWN4RDhFbzg3dEdWSUwzKytSbDdaZ1JWZE5STUp5MEhrUDJIZmVmSWZaQjV2VW9YTWhuYytKSVB3M0hmVFBlbjFUM29qdmxsanNBM3l6OC8vNGU5eWpKeDIwV0pHMnBFV3BmWEJYZDVJZklmeG53QWJTUGNhMTRGNE8rN1hYRjA0bks2U0ZQckZkYzgrUGxFZUZqbmxKRG9YOTMwenE3MHcrMDZjeElMV096RDE2bCtlSldZbzg5OGhSWUxHUlJvRTFGSE5XcjV3WDBCeWNjL3creWhCbDl3dkpsckM="];//This was generated by the erlang version of encrypter.
        //console.log("sms 2 are ");
        //console.log(JSON.stringify(sm));
        //console.log(JSON.stringify(sm2));
        var got = get(sm, key2);
        console.log("got");
        console.log(JSON.stringify(got));
        test2();
    };
    function test2() {
        var key = hash([1]);
        console.log(key);//same as hash:doit(<<1>>) from erlang.
        var textBytes = [1,2,3];
        var eb = bin_enc(key, textBytes);
        assert_eq(eb, [100, 131, 24]);
        assert_eq(bin_dec(key, eb), [1, 2, 3]);
        var fromKey = keys.make();
        var toKey = keys.make();
        var sm = send([-6,1,2,3], btoa(fromHex(toKey.getPublic("hex"))), fromKey);
        assert_eq(get(sm, toKey), [-6, 1, 2, 3]);
        var masterPub64 = "BLDdkEzI6L8qmIFcSdnH5pfNAjEU11S9pHXFzY4U0JMgfvIMnwMxDOA85t6DKArhzbPJ1QaNBFHO7nRguf3El3I=";
        var master = keys.ec().keyFromPublic(toHex(atob(masterPub64)), 'hex');
        console.log("encryption test passed.");
    }
    function test_shared() {
        var Key1 = keys.make();
        var Key2 = keys.make();
	//var ss1 = shared(Key1, Key2.getPublic());
	var ss1 = shared(Key1, Key2);
	var ss2 = shared(Key2, Key1);
	console.log(JSON.stringify(ss1));
	console.log(JSON.stringify(ss2));
	assert_eq(ss1, ss2);
	return "success";
    }
    //test();
    return {get: get, send: send, test: test, test2: test2, test_shared: test_shared};
}
var encryption_object = encryption_main();






var secrets_object = (function () {
    var db = {};
    function add(code, ss, amount){
	db[code] = [ss, amount];
    }
    function dump() {
	db = {};
    }
    function read(code) {
	return db[code];
    }
    function check() {
	return db;
    }
    return {add: add, dump: dump, read: read, check: check};
})();

(function() {
	var tab_id = "encryption";

	var div = document.createElement("div");
	div.className = "tabs__content-item " + tab_id;
	div.id = tab_id;

	var title = document.createElement("h3");
	title.className = "tabs__nav-item";
	title.dataset.tab = tab_id;
	title.innerHTML = tab_id;

	if (!nav.hasChildNodes()) {
		title.className += " active";
		div.className += " active";
	}

	tabs.appendChild(div);
	nav.appendChild(title);

	var b = button_maker2("Encrypt", encrypt);

	var msg_to_send = document.createElement("INPUT");
	msg_to_send.type = "text";
	msg_to_send.id = "msg_to_send";
	var msg_to = document.createElement("TEXTAREA");
	//msg_to.type = "text";
	var encrypted_to_send = document.createElement("div");
	var to_instructions = document.createElement("label");
	to_instructions.innerHTML = "To pubkey";

	var msg_instructions = document.createElement("label");
	msg_instructions.innerHTML = "Message to send";

	var encrypted_instructions = document.createElement("label");
	encrypted_instructions.innerHTML = "Decrypt";
	var b2 = button_maker2("Decrypt", decrypt);
	var encrypted_received = document.createElement("TEXTAREA");
	//encrypted_received.type = "text";
	var decrypted_received = document.createElement("div");

	var wrap = document.createElement("div");
	wrap.className = "tabs__col";
	var wrap2 = document.createElement("div");
	wrap2.className = "tabs__col";

	var fieldset1 = wrapper("fieldset", [to_instructions, msg_to]);
	var fieldset2 = wrapper("fieldset", [msg_instructions, msg_to_send]);

	append_children(wrap, [fieldset1, fieldset2, b, encrypted_to_send]);
	append_children(wrap2, [encrypted_instructions, encrypted_received, b2, decrypted_received]);
	append_children(div, [wrap, wrap2]);

	function encrypt() {
		var t = msg_to_send.value;
		var to = msg_to.value;
		var t2 = keys.encrypt(t, to);
		encrypted_to_send.innerHTML = "<div class='msg'>"+JSON.stringify(t2)+"</div>";
	}

	function decrypt() {
		var t = encrypted_received.value;
		var t2 = keys.decrypt(JSON.parse(t));
		decrypted_received.innerHTML = "<div class='msg'>"+t2+"</div>";
	}
})();

(function(){

    if(tabs && nav){
        var tab_id = "oracles"
        var div = document.createElement("div");
        div.className = "tabs__content-item "+tab_id;
        div.id = tab_id;
        var title = document.createElement("h3");
        title.className = "tabs__nav-item";
        title.innerHTML = tab_id;
        title.dataset.tab = tab_id;

        if (!nav.hasChildNodes()) {
            title.className += " active";
            div.className += " active";
        }

        tabs.appendChild(div);
        nav.appendChild(title);

        var oid = document.createElement("INPUT");
        oid.type = "text";
        oid.className = "wide";
        var b = button_maker2("Lookup oracle", lookup);
        var oracleOutput = document.createElement("div");
        oracleOutput.className = "output"


        var wrap = document.createElement("div");
        wrap.className = "tabs__col";

        var wrap2 = document.createElement("div");
        wrap2.className = "tabs__col";
        wrap2.innerHTML = "<div class='tabs__box'>"+veo_text+"</div>";

        var fieldset1 = wrapper("fieldset", [oid, b]);
        append_children(wrap, [fieldset1]);
        append_children(div, [wrap, wrap2, oracleOutput]);

        function lookup() {
    	oracleOutput.innerHTML = "";
    	var v = oid.value;
    	merkle.request_proof("oracles", v, function(x) {
    	    var result = x[2];
    	    var question = x[3];
    	    var starts = x[4];
    	    var type = x[5];
    	    var done_timer = x[9];
    	    var governance = x[10];
    	    var governance_amount = x[11];
    	    var orders_hash = x[7];
    	    var a;
    	    if (result == 0) {
    		a = htitle("This oracle is still open");
    	    } else if (result == 1) {
    		a = htitle("This oracle closed in state: true");
    	    } else if (result == 2) {
    		a = htitle("This oracle closed in state: false");
    	    } else if (result == 3) {
    		a = htitle("This oracle closed in state: bad-question");
    	    }
    	    oracleOutput.appendChild(a);
    	    if (governance == 0) {
    		oracleOutput.appendChild(pre("This is a question oracle"));
    		var asks_txt = "asks: ".concat(btoa(btoa(question)));
    		oracleOutput.appendChild(pre(asks_txt));
    	    } else {
    		oracleOutput.appendChild(pre("This is a governance oracle"));
    		var gov_txt = "governance variable: ".concat(JSON.stringify(governance));
    		oracleOutput.appendChild(pre(gov_txt));
    		var gov_amount_txt = "governance amount: ".concat(JSON.stringify(governance_amount));
    		oracleOutput.appendChild(pre(gov_amount_txt));
    	    }
    	    var starts_txt = "starts: ".concat(JSON.stringify(starts));
    	    oracleOutput.appendChild(pre(starts_txt));
    	    var type2;
    	    if (type == 3) {
    		type2 = "bad-question";
    	    } else if (type == 1) {
    		type2 = "true";
    	    } else if (type == 2) {
    		type2 = "false";
    	    }
    	    var type_txt = "current type: ".concat(type2);
    	    oracleOutput.appendChild(pre(type_txt));
    	    var done_txt = "done timer: ".concat(JSON.stringify(done_timer));
    	    oracleOutput.appendChild(pre(done_txt));

    	    console.log("new");
    	    console.log(v);
    	    merkle.request_proof("orders", orders_hash, function(x) {
    		console.log(x);
    	    });
    	    //variable_public_get(["oracle_bets", v], oracle_bets);

    	    //now display the whole thing.
    	    var x2 = pre(JSON.stringify(x));
    	    oracleOutput.appendChild(x2);

    	});
        };
        function oracle_bets(x) {
            console.log("inside oracle bets");
            console.log(JSON.stringify(x));
        }
    }
})();


function channels_main() {
    //Model
    var channel_manager = {};
    var tv = -1;
    function read(x) {
	console.log("read channel ");
	console.log(JSON.stringify(x));
        var y = channel_manager[x];
        if (y == undefined) {
            return undefined;
        } else {
            return JSON.parse(JSON.stringify(y));
        }
    }
    function write(key, value) {
	if (value == undefined) {
	    throw("error, deleting channel data");
	}
	if (value.ssme == undefined) {
	    throw("error, ssme needs to be defined");
	}
	if (!(value.me[3][0] == -6)) {
	    throw("bets badly formated");
	}
	if (!(value.them[1][3][0] == -6)) {
	    throw("them bets badly formated");
	}
	var l1 = value.ssme.length;
	var l2 = value.me[3].length - 1;
	if (!(l1 == l2)) {
	    console.log(JSON.stringify(value.ssme));
	    console.log(JSON.stringify(value.me[3]));
	    throw("error, we need the same number of ss and bets in me.");
	}
	var l3 = value.ssthem.length;
	var l4 = value.them[1][3].length - 1;
	if (!(l3 == l4)) {
	    console.log(JSON.stringify([l3, l4]));
	    console.log(JSON.stringify(value.ssthem));//there is an extra.
	    console.log(JSON.stringify(value.them[1][3]));
	    throw("error, we need the same number of ss and bets in them.");
	}
        channel_manager[key] = value;
    }
    function remove(key) {
	delete channel_manager[key];
    }
    function new_cd(me, them, ssme, ssthem, expiration, cid) {
        return {"me": me, "them": them, "ssme": ssme, "ssthem": ssthem, "cid":cid, "expiration": expiration};
    }
    function new_ss(code, prove, meta) {
        if (meta == undefined) {
            meta = 0;
        }
        return {"code": code, "prove": prove, "meta": meta};
    }

    //View
	var tab_id = "channel";
    var channel_title = document.createElement("h3");
    channel_title.innerHTML = tab_id;
    channel_title.className = "tabs__nav-item";

    channel_title.dataset.tab = tab_id;
    var channels_div = document.createElement("div");
    channels_div.className = "tabs__content-item " + tab_id;
    channels_div.id = tab_id;
    var channel_warning_div = document.createElement("div");
    //var channel_interface_div = document.createElement("div");
    var load_button = document.createElement("input");
    var load_button_btn = document.createElement("label");
    load_button.type = "file";
    load_button.id = "channel_file";
    load_button_btn.className = "btn";
    load_button_btn.htmlFor = "channel_file";
    load_button_btn.innerHTML = "Load channel from file";
    var save_name = document.createElement("INPUT");
    save_name.type = "text";
    save_name.className = "wide";
	save_name.id = "channel_name";
    save_name.value = "Channel state";
	var save_button = button_maker2("Save channel data to file", save_channel_data);
    var refresh_channels_button = button_maker2("Refresh channels interfaces.", function() {
        variable_public_get(["pubkey"], function(pubkey) {
            return refresh_channels_interfaces(pubkey);
        });
    });
	var refresh_text = document.createElement("p");
	refresh_text.innerHTML = "Useful if you swich channel servers";

	if (!nav.hasChildNodes()) {
		channel_title.className += " active";
		channels_div.className += " active";
	}

	var wrap = document.createElement("div");
	wrap.className = "tabs__col";
	var wrap2 = document.createElement("div");
	wrap2.className = "tabs__col";
	wrap2.innerHTML = "<div class='tabs__box'>" + veo_text + "</div>";

	var fieldset_load = wrapper("fieldset", [load_button, load_button_btn]);
	var fieldset_save = wrapper("fieldset", [channel_warning_div, save_name, save_button]);
	var fieldset_refresh = wrapper("fieldset", [refresh_channels_button, refresh_text]);

	append_children(wrap, [fieldset_load, hr(), fieldset_save, hr(), fieldset_refresh]);
	append_children(channels_div, [wrap, wrap2]);
	nav.appendChild(channel_title);
	tabs.appendChild(channels_div);

    var fee = 152050;
    var oid = document.createElement("INPUT");
    oid.setAttribute("type", "text");
    var oid_info = document.createElement("h8");
    oid_info.innerHTML = "market: ";
    var price = document.createElement("INPUT");
    price.setAttribute("type", "text");
    var price_info = document.createElement("h8");
    price_info.innerHTML = "price (between 0 and 100) : ";
    var trade_type = document.createElement("INPUT");
    trade_type.setAttribute("type", "text");
    var trade_type_info = document.createElement("h8");
    trade_type_info.innerHTML = "trade type (true/false): ";
    var trade_amount = document.createElement("INPUT");
    trade_amount.setAttribute("type", "text");
    var trade_amount_info = document.createElement("h8");
    trade_amount_info.innerHTML = "amount: ";
    var height_button = button_maker2("make channel ", function() { })
    var spend_amount = document.createElement("INPUT");
    spend_amount.setAttribute("type", "text");
    spend_amount.id = "spend-amount";
    var amount_info = document.createElement("label");
    amount_info.innerHTML = "amount to lock in channel ";
    amount_info.htmlFor = "spend-amount";
    var spend_delay = document.createElement("INPUT");
    spend_delay.setAttribute("type", "text");
    spend_delay.id = "channel-delay";
    spend_delay.value = "100";
    var delay_info = document.createElement("label");
    delay_info.htmlFor = "channel-delay";
    delay_info.innerHTML = "channel delay (in blocks)";
    var lifespan = document.createElement("input");
    lifespan.type = "text";
    lifespan.id = "lifespan-info";
    lifespan.value = "4000";
    var lifespan_info = document.createElement("label");
    lifespan_info.innerHTML = "how long should the channel last? In blocks. Longer costs more.";
    lifespan_info.htmlFor = "lifespan-info";
    var balance_div = document.createElement("div");
    balance_div.innerHTML = "your balance unknown";
    var channel_balance_button = button_maker2("check channel balance", function() { });
    var market_title = document.createElement("h3");
    market_title.innerHTML = "markets";
    var market_link = document.createElement("a");
    market_link.innerHTML = "<a href=\"/explorer.html\">see the available markets here</a>";
    //    market_link.innerHTML = "see the available markets here ";
    //    market_link.href = "http://159.89.106.253:8080/explorer.html";
    var bet_example = document.createElement("h8");
    bet_example.innerHTML = "if price is 30, and amount is 1, then you can win 0.7, or you can lose 0.3.";
    var button = button_maker2("make bet ", make_bet);
    var bet_update_button = button_maker2("check if any bets have been settled", function() {});
    var combine_cancel_button = button_maker2("combine bets in opposite directions to recover the money from the market ", function() {});
    var list_bets_button = button_maker2("update balance of off-chain assets ", bets_object.main);
    var close_channel_button = button_maker2("close channel", function(){ return; });
    var lightning_button = button_maker2("lightning spend", function(){ return; });
    var lightning_amount = document.createElement("INPUT");
    lightning_amount.setAttribute("type", "text");
    var lightning_amount_info = document.createElement("h8");
    lightning_amount_info.innerHTML = "amount: ";
    var lightning_to = document.createElement("INPUT");
    lightning_to.setAttribute("type", "text");
    var lightning_to_info = document.createElement("h8");
    lightning_to_info.innerHTML = "to pubkey: ";
    var channel_sync_button = button_maker2("trusted channel sync", function(){
        variable_public_get(["pubkey"], function(pubkey) {
            spk_object.pull_channel_state(function() {
		refresh_channels_interfaces(pubkey, function() {
		    refresh_balance(pubkey);
		});
	    });
	});
    });

    variable_public_get(["pubkey"], function(pubkey) {
        return refresh_channels_interfaces(pubkey);
    });
    function channel_warning() {
        channel_warning_div.innerHTML = "channel state needs to be saved!~~~~~~~";
    }
    function save_channel_data() {
        var save_name = document.getElementById("channel_name");
        download(JSON.stringify(channel_manager), save_name.value, "text/plain");
        channel_warning_div.innerHTML = "channel state is saved";
    }
    function load_channels(pubkey) {
	console.log("load channels");
        var file = (load_button.files)[0];
        var reader = new FileReader();
        reader.onload = function(e) {
            channel_manager = JSON.parse(reader.result);
	    //console.log(JSON.stringify(channel_manager));
            refresh_channels_interfaces(pubkey);
        }
        reader.readAsText(file);
    }
    function refresh_channels_interfaces(pubkey, callback) {
        console.log("refresh channels interfaces");
        variable_public_get(["time_value"], function(x) {
            tv = x;
            refresh_channels_interfaces2(pubkey, callback);
        });
    }
    function refresh_channels_interfaces2(pubkey, callback) {
	console.log("server pubkey is ");
	console.log(pubkey);
        load_button.onchange = function() {return load_channels(pubkey) };
        //refresh_channels_button.onclick = function() {return refresh_channels_interfaces2(pubkey)};
        //var div = channel_interface_div;
        //div.innerHTML = "";
        var tv_display = document.createElement("p");
        tv_display.className = "msg";
        tv_display.innerHTML = ("It costs this much to keep a channel open. per block per coin: ").concat((tv).toString());

        var fieldset_channel_1 = wrapper("fieldset", [channel_sync_button]);
        var fieldset_channel_amount = wrapper("fieldset fieldset_nowr", [amount_info, spend_amount]);
        var fieldset_channel_delay = wrapper("fieldset", [delay_info, spend_delay]);
        var fieldset_channel_life = wrapper("fieldset", [lifespan_info, lifespan]);

        height_button = wrapper("fieldset", [height_button]);

        append_children(wrap, [tv_display, fieldset_channel_1]);

        var bets_div = document.createElement("div");
	bets_div.id = "bets_div";
        //check if we have a chnnel with the server yet.
        //if we don't, then give an interface for making one.
        if (read(pubkey) == undefined) {
            console.log("give interface for making channels.");
            height_button.onclick = function() { return make_channel_func(pubkey) };
            append_children(wrap, [hr(), height_button, hr(), fieldset_channel_amount, fieldset_channel_delay, fieldset_channel_life]);
		} else {
            console.log("give interface for making bets in channels.");
            append_children(wrap, [close_channel_button, hr(), balance_div, channel_balance_button, lightning_button, lightning_amount_info, lightning_amount, lightning_to_info, lightning_to, br(), market_title, market_link, br(), bet_example, br(), price_info, price, trade_type_info, trade_type, trade_amount_info, trade_amount, oid_info, oid, button, br(), bet_update_button, br(), br(), combine_cancel_button, br(), br(), list_bets_button, br(), bets_div]);
			lightning_button.onclick = function() { lightning_spend(pubkey); };
            channel_balance_button.onclick = function() {refresh_balance(pubkey);};
            bet_update_button.onclick = function() {
                spk_object.pull_channel_state(function() {
                    refresh_channels_interfaces(pubkey);
		});
            };
            combine_cancel_button.onclick = function() {
                combine_cancel_object.main(pubkey);
            };
	    close_channel_button.onclick = function() { close_channel_func(pubkey); };
	    bets_object.draw();
        }
	if (!(callback == undefined)) {
	    callback();
	}
    }
    function close_channel_func(server_pubkey) {
	var cd = read(server_pubkey);
	var spk = cd.them[1];
	var height = headers_object.top()[1];
	var ss = cd.ssthem;
	if (!(JSON.stringify(ss) == JSON.stringify([]))) {
	    console.log("you need to close all smart contracts before you can close the channel");
	    return 0;
	};
	var fun_limit = 400;
	var var_limit = 10000;
	spk_object.spk_run(0, ss, spk, height, 0, fun_limit, var_limit, function(x) { close_channel_func2(x, spk, cd, height, ss, server_pubkey); });
    }
    function close_channel_func2(spk_result, spk, cd, height, ss, server_pubkey) {
	var amount = spk_result.amount + cd.them[1][7];
	var cid = spk[6];
	merkle.request_proof("channels", cid, function(x) {
	    close_channel_func3(x, height, cd, amount, ss, cid, server_pubkey); });
    }
    function close_channel_func3(channel, height, cd, amount, ss, cid, server_pubkey) {
	var mypub = keys.pub();
	merkle.request_proof("accounts", mypub, function(acc) {close_channel_func4(acc, channel, height, cd, amount, mypub, ss, cid, server_pubkey);});
    }
    function close_channel_func4(acc, channel, height, cd, amount, mypub, ss, cid, server_pubkey) {
	var expires = cd.expiration;
	var lifespan = Math.max(0, expires - height);
	var bal1 = channel[4];
	var bal2 = channel[5];
	var cfee = Math.floor(tv * lifespan * (bal1 + bal2) / token_units());
	var nonce = acc[2];
	var fee = 152050;
	var acc1 = channel[2];
	var acc2 = channel[3];
	var tx = ["ctc", acc1, acc2, fee, nonce+1, cid, amount - cfee];
	var stx = keys.sign(tx);
	//console.log(JSON.stringify([bal1, bal2, tv, expires, height, amount]));
	variable_public_get(["channel_close", cid, mypub, [-6].concat(ss), stx], function(tx) {
	    remove(server_pubkey);
	    refresh_channels_interfaces2(server_pubkey);
	});
    }
    function make_bet() {
        var oid_final = oid.value;
        variable_public_get(["market_data", oid_final], make_bet2);
    }
    function make_bet2(l) {
        var price_final = Math.floor(100 * parseFloat(price.value, 10));
        var type_final;
        var ttv = trade_type.value;
        if ((ttv == "true") ||
            (ttv == 1) ||
            (ttv == "1") ||
            (ttv == "yes") ||
            (ttv == "si") ||
            (ttv == "cierto") ||
            (ttv == "lon") ||
            (ttv == "真正") ||
            (ttv == "既不是")) {
            type_final = 1;
        } else if ((ttv == "false") ||
                   (ttv == 0) ||
                   (ttv == "0") ||
                   (ttv == 2) ||
                   (ttv == "2") ||
                   (ttv == "falso") ||
                   (ttv == "no") ||
                   (ttv == "lon ala") ||
                   (ttv == "也不是") ||
                   (ttv == "假")) {
            type_final = 2;
        }
        var amount_final = Math.floor(parseFloat(trade_amount.value, 10) * token_units());
        var oid_final = oid.value;
        var expires = l[1];
        var server_pubkey = l[2];
        var period = l[3];
	var sc;
	console.log("SCALAR ");
	console.log(JSON.stringify(l));
	console.log(JSON.stringify(l[4]));
	// if l[4] is ["binary"] then do this:
	if (l[4][0] == "binary") {
            sc = market_contract(type_final, expires, price_final, server_pubkey, period, amount_final, oid_final, headers_object.top()[1]);
	} else {
	    var lower_limit = l[4][1];
	    var upper_limit = l[4][2];
	    // sanity-check, verify 10 == l[4][3];
	//all scalar markets currently use 10 binary oracles to measure values.
            sc = scalar_market_contract(type_final, expires, price_final, server_pubkey, period, amount_final, oid_final, headers_object.top()[1], lower_limit, upper_limit, 10);
	}
        var cd = read(server_pubkey);
        var spk = market_trade(cd, amount_final, price_final, sc, server_pubkey, oid_final);
        var sspk = keys.sign(spk);
        var msg = ["trade", keys.pub(), price_final, type_final, amount_final, oid_final, sspk, fee];
        return variable_public_get(msg, function(x) {
            make_bet3(x, sspk, server_pubkey, oid_final);
        });
    }
    function make_bet3(sspk2, sspk, server_pubkey, oid_final) {
	var bool = verify_both(sspk2);
	if (!(bool)) {
	    throw("make bet3, badly signed sspk2");
	}
        var hspk2 = JSON.stringify(sspk2[1]);
        var hspk = JSON.stringify(sspk[1]);
	if (!(hspk == hspk2)) {
            console.log("error, we calculated the spk differently from the server. you calculated this: ");
            console.log(JSON.stringify(sspk[1]));
            console.log("the server calculated this: ");
            console.log(JSON.stringify(sspk2[1]));
	}
        var cd = read(server_pubkey);
        cd.me = sspk[1];
        cd.them = sspk2;
        var newss = new_ss([0,0,0,0,4], [-6, ["oracles", oid_final]]);
        cd.ssme = ([newss]).concat(cd.ssme);
        cd.ssthem = ([newss]).concat(cd.ssthem);
        write(server_pubkey, cd);
        trade_amount.value = "";
        channel_warning();
    }

    //Controller

    function make_channel_func(pubkey) {
        var amount = Math.floor(parseFloat(spend_amount.value, 10) * token_units());
        var delay = parseInt(spend_delay.value, 10);
        var expiration = parseInt(lifespan.value, 10) + headers_object.top()[1];
        var bal2 = amount - 1;
        spend_amount.value = "";
        var acc1 = keys.pub();
        var acc2 = pubkey;
        //let the server choose an unused cid for us.
        variable_public_get(["new_channel_tx", acc1, pubkey, amount, bal2,delay, fee], function(x) { make_channel_func2(x, amount, bal2, acc1, acc2, delay, expiration, pubkey); } );
    }
    function make_channel_func2(tx, amount, bal2, acc1, acc2, delay, expiration, pubkey) {
        //ask a server to make the tx for us, then check that all our data matches.
        //console.log("make channel tx is ");
        //console.log(tx);
        var amount0 = tx[5];
        var bal20 = tx[6];
        var fee0 = tx[3];
        var acc10 = tx[1];
        var acc20 = tx[2];
        var cid = tx[8];
        var delay0 = tx[7];
        if ((!(delay == delay0)) || (!(amount == amount0)) ||
            (!(bal2 == bal20)) || (!(fee == fee0)) ||
            (!(acc1 == acc10)) || (!(acc2 == acc20))) {
            console.log(JSON.stringify([[delay, delay0], [amount, amount0], [bal2, bal20], [fee, fee0], [acc1, acc10], [acc2, acc20]]));
            console.log("server edited the tx. aborting");
        } else {
            var current_height = headers_object.top()[1];
            var lifespan = expiration - current_height;
            var spk_amount = Math.floor((tv * (delay + lifespan) * (amount + bal2) ) / 100000000);
            var spk = ["spk", acc1, acc2, [-6], 0, 0, cid, spk_amount, 0, delay];
            var stx = keys.sign(tx);
            var sspk = keys.sign(spk);
            variable_public_get(["new_channel", stx, sspk, expiration], function(x) { return channels3(x, expiration, pubkey, spk, tx) });
        }
    }
    function channels3(x, expiration, pubkey, spk, tx_original) {
        var sstx = x[1];
        var s2spk = x[2];
        var tx = sstx[1];
	if (!(JSON.stringify(tx) ==
	      JSON.stringify(tx_original))) {
	    console.log(JSON.stringify(tx));
	    console.log(JSON.stringify(tx_original));
	    throw("the server illegally manipulated the tx");
	}
	var a = verify_both(sstx);
	if (!(a)) {
	    throw("bad signature on tx in channels 3");
	}
	a = verify2(s2spk);
	if (!(a)) {
	    throw("bad signature on spk in channels 3");
	}
	if (!(JSON.stringify(spk) ==
	      JSON.stringify(s2spk[1]))) {
	    throw("the server illegally manipulated the spk");
	}
        var cid = tx[9];
        var acc2 = tx[2];
        //console.log("double signed tx ");
        //console.log(JSON.stringify(sstx));
        //variable_public_get(["txs", [-6, sstx]], function(x) {});
        var spk = s2spk[1];
        var cd = new_cd(spk, s2spk, [], [], expiration, cid);
        write(acc2, cd);
        channel_warning();
        refresh_channels_interfaces(pubkey);
    }
    function refresh_balance(pubkey) {
        //console.log(channel_manager[pubkey]);
        var cd = read(pubkey);
        var trie_key = cd.me[6];//channel id, cid
	var top_header = headers_object.top();
        var top_hash = hash(headers_object.serialize(top_header));
	//console.log("refresh balance trie key is ");
	//console.log(trie_key);
        merkle.request_proof("channels", trie_key, function(val) {
            //var balance_div = document.getElementById("balance_div");
            var spk = cd.them[1];
	    var expiration = cd.expiration;
	    var height = top_header[1];
            var amount = spk[7];
            var betAmount = sum_bets(spk[3]);
	    console.log(JSON.stringify([val[4], amount, betAmount, val[5], token_units()]));
            var mybalance = ((val[4] - amount - betAmount)/ token_units()).toString();
            var serverbalance = ((val[5] + amount) / token_units()).toString();
            balance_div.innerHTML = ("server balance: ").concat(
                serverbalance).concat("your balance: ").concat(
                    mybalance).concat("time left in blocks: ").concat(
			(cd.expiration - height).toString());

        });
    }
    function channel_feeder_make_locked_payment(serverid, amount, code) {
        var cd = read(serverid);
        var spk = cd.me;
	console.log("channel feeder make locked payment");
	console.log(JSON.stringify(spk));
        var bet = ["bet", code, amount, code, 0];
        spk[3] = [-6, bet].concat((spk[3]).slice(1));
        spk[8] += 1;
        spk[5] += 1000;
        spk[4] = Math.max(spk[4], 1000);
        //spk[7] += amount;
        return keys.sign(spk);
    }
    function ss_to_external(ss) {
	return ["ss",
		btoa(array_to_string(ss.code)),
		([-6]).concat(ss.prove),
		([-6]).concat(ss.meta)];//prove and meta should start with -6.
    }
    function lightning_spend(serverid) {
	var header_height = headers_object.top()[1];
	variable_public_get(["height"], function(server_height) {
	    if (!(header_height == server_height)) {
		console.log("need to sync headers before you can make channel payments");
		throw("lightning spend error");
		    }
	            var fee = 20;
	            var a = Math.floor(parseFloat(lightning_amount.value, 10) * token_units());
	            var to = lightning_to.value;
	            var payment_contract = lightning_object.make(a);
	            var code = payment_contract.bet[1];
	            var ss = payment_contract.ss;
		    var emsg = [-6, ss_to_external(ss), code, a];
	            var encrypted = keys.encrypt(emsg, to);
		    console.log("lightning spend emsg, a, fee");
		    console.log(JSON.stringify(ss));
		    console.log(JSON.stringify(emsg));
		    console.log(a);
		    console.log(fee);
	            var sspk = channel_feeder_make_locked_payment(serverid, a+fee, code);
	            var msg = ["locked_payment", sspk, a, fee, code, keys.pub(), to, encrypted];
	            console.log("lightning spend msg is ");
	            console.log(JSON.stringify(msg));
		    console.log("lightning encrypted msg is ");
	            console.log(JSON.stringify(emsg));
	            variable_public_get(msg, function(sspk2) {
			spk1 = sspk[1];
			spk2 = sspk2[1];
			var bool = verify_both(sspk2);
			if (!(bool)) {
			    throw("lightning spend, bad signature on spk");
			}
			if (!(JSON.stringify(spk1) ==
			      JSON.stringify(spk2))) {
			    console.log("error, the spks calculated by you and the server are not identical.");
			    throw("lightning_spend error")
			}
			var cd = read(serverid);
			var defaultss = new_ss([], [], 0);
			//cd.ssme = ([-6, defaultss]).concat(cd.ssme.slice(1));
			//cd.ssthem = ([-6, defaultss]).concat(cd.ssthem.slice(1));
			cd.ssme = ([defaultss]).concat(cd.ssme);
			cd.ssthem = ([defaultss]).concat(cd.ssthem);
			cd.me = spk1;
			cd.them = sspk2;
			/*
	spk currently looks like this.
	{"me":["spk","BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4=","BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=",[-6],0,0,"uEHL7hd8f6hzyalwrYPOMKfL1DV4bshFb3qlc3mR3w0=",6374999,0,100],"them":["signed",["spk","BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4=","BIVZhs16gtoQ/uUMujl5aSutpImC4va8MewgCveh6MEuDjoDvtQqYZ5FeYcUhY/QLjpCBrXjqvTtFiN4li0Nhjo=",[-6],0,0,"uEHL7hd8f6hzyalwrYPOMKfL1DV4bshFb3qlc3mR3w0=",6374999,0,100],[-6],"MEYCIQCtc7a8h5AksJDzyJascAWo4OPq7eh1wtWSmcQ7ia+dzgIhANqTE+NFQaiMeY952P64MfY2b15SlhNpvoBKCij5/7le"],"ssme":[-6,{"code":[],"prove":[],"meta":0}],"ssthem":[-6,{"code":[],"prove":[],"meta":0}],"expiration":5020}"
		     */
			write(serverid, cd);
	            });
		});
	    }
	    function sum_bets(bets) {
		var x = 0;
		for (var i = 1; i < bets.length; i++) {
		    //console.log("sum bets bet is ");
		    //console.log(JSON.stringify(bets[i][2]));
		    x += bets[i][2];
		}
	        return x;
	    }
	    return {new_cd: new_cd,
	            read: read,
	            new_ss: new_ss,
	            write: write,
		    ss_to_external: ss_to_external}
	}
	var channels_object = channels_main();

'use strict';

function Tabs() {
  var bindAll = function() {
    var menuElements = document.querySelectorAll('[data-tab]');
    var spoilerContent = document.querySelectorAll('.spoiler__content_show');
    for(var i = 0; i < menuElements.length ; i++) {
      menuElements[i].addEventListener('click', change, false);
    }
  }

  var clear = function() {
    var menuElements = document.querySelectorAll('[data-tab]');
    for(var i = 0; i < menuElements.length ; i++) {
      menuElements[i].classList.remove('active');
      var id = menuElements[i].getAttribute('data-tab');
      document.getElementById(id).classList.remove('active');
    }
  }

  var change = function(e) {
    clear();
    e.target.classList.add('active');
    var id = e.currentTarget.getAttribute('data-tab');
    document.getElementById(id).classList.add('active');

    // close all spoiler (spoiler.js)
    var spoilerBtn = document.querySelectorAll('.spoiler__button');
    var spoilerContent = document.querySelectorAll('.spoiler__content_show');
    for(var s = 0; s < spoilerContent.length ; s++) {
      spoilerContent[s].classList.remove('spoiler__content_show');
    }
    for(var b = 0; b < spoilerBtn.length ; b++) {
      spoilerBtn[b].classList.remove('active');
    }
  }

  bindAll();
}

var connectTabs = new Tabs();

'use strict';

(function () {

	var toggleSpoilerVisibility = function (element) {
	  element.addEventListener('click', function () {
	    this.classList.toggle('active');
	    this.nextElementSibling.classList.toggle('spoiler__content_show');
	  });
	}

	var allSpoilerButton = Array.prototype.slice.call(document.querySelectorAll('.spoiler__button'));

	allSpoilerButton.forEach(toggleSpoilerVisibility);
}());
