#!/usr/bin/awk -Mf

@include "random"

function pow(a, b) {
    if(b == 0) { return(1)}
    if(b == 1){return(a)}
    if((b % 2) == 0){
	return(pow(a*a, int(b/2)))
    }
    return(a*pow(a, b-1))
}

function initialize_generator(G,         x, y){
    x = strtonum("0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")
    y = strtonum("0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8")
    G["x"] = x
    G["y"] = y
    #r["z"] = 1
    #r["t"] = mul(x, y)
}

function mul(a, b) { return(a*b % BASE) }
function div(a, b) { return(mul(a, inverse(b))) }
function sub2(a, b){ return((a - b + BASE) % BASE) }
function add(a, b){ return((a + b) % BASE) }

#extended euclidean algorithm finds A and T.
#bezout coefficients for A and B are S and T
# (S*A) + (T*B) = gcd(A, B)
function eea(a, b, r){
    if(a < 1) {
	print("error eea 1")
	r["fail"] = 1
	return(0)
    } else if(b < 1){
	print("error eea 2")
	r["fail"] = 1
	return(0)
    }
    return(eea2(a, 1, 0, b, 0, 1, r))
}
function eea2(G, S, T, G1, S1, T1, r){
    if(G1 == 0){
	r["g"] = G
	r["s"] = S
	r["t"] = T
	return(0)
    }
    Q = int(G / G1)
    return(eea2(G1, S1, T1, G - (Q*G1), S - (Q*S1), T - (Q*T1), r))
}

function inverse(a,       B, r) {
    if(!(B)) {
	B = BASE
    }
    eea(a, B, r)
    if(r["fail"] == 1){
	print("inverse does not exist")
	print(a)
	throw("error")
    } else if (r["g"] == 1) {
	return((r["s"]+B) % B)
    } else {
	print("inverse does not exist 2")
	print("make sure you used the command line flag -M when running AWK.")
	print(a)
	print(r["g"])
	print(r["s"])
	print(r["t"])
	throw("error2")
    }
}

function on_curve(r,     a, b){
    #curve definition: y^2 = x^3 + 7
    if(r["x"] == "neutral"){
	#handle the point at infinity
	return(1)
    }
    a = mul(r["y"], r["y"])
    b = (7 + pow(r["x"], 3)) % BASE
    return(a == b)
}

function point_add(A, B, C,      lt, lb, l) {
    if(A["x"] == "neutral") {
	#the point at infinity acts like zero under addition.
        C["x"] = B["x"]
	C["y"] = B["y"]
	return(0)
    }
    if(B["x"] == "neutral"){
	#the point at infinity acts like zero under addition.
        C["x"] = A["x"]
	C["y"] = A["y"]
	return(0)
    }
    if((A["x"] == B["x"]) && (A["y"] == B["y"])) {
	return(point_double(A, C))
    }
    lt = sub2(B["y"], A["y"])
    lb = sub2(B["x"], A["x"])
    if(lb == 0){
	#result is the point at infinity.
	C["x"] = "neutral"
        C["y"] = "neutral"
        return(0)
    }
    l = div(lt, lb)
    C["x"] = sub2(mul(l, l),
		  add(A["x"], B["x"]))
    C["y"] = sub2(mul(l, sub2(A["x"], C["x"])),
		  A["y"])
    return(0)
}
function point_double(A, C,      lt, lb, l){
    #print("point double")
    lt = mul(3, mul(A["x"], A["x"]))
    lb = mul(2, A["y"])
    if(lb == 0){
	#result is the point at infinity.
	C["x"] = "neutral"
        C["y"] = "neutral"
        return(0)
    }
    l = div(lt, lb)
    C["x"] = sub2(mul(l, l), add(A["x"], A["x"]))
    C["y"] = sub2(mul(l, sub2(A["x"], C["x"])), A["y"])
    return(0)
}

function point_mul(P, n, R,      P2){
    if(n < 1){ print("no negative for point_mul")
	throw("point mul error")
	R["fail"] = 1
	return(0)
    }
    if(n == 1) {
	R["x"] = P["x"]
	R["y"] = P["y"]
	return(0)}
    if((n % 2) == 0){
	point_double(P, P2)
	return(point_mul(P2, int(n/2), R))
    }
    point_mul(P, n-1, P2)
    return(point_add(P, P2, R))
}

function gen_key_pair(key){
    print("gen key pair 0")
    priv = make_private_key()#from random.awk
    print("gen key pair 10")
    privi = strtonum("0x" priv)% ORDER
    point_mul(G, privi, pub)
    print("gen key pair 1")
    key["priv"] = privi
    key["x"] = pub["x"]
    key["y"] = pub["y"]
}

function sign(msg, priv, Sig,     z, r, s, k, st, sb) {
    #private key is an integer
    z = strtonum("0x" hash_string(msg))
    r = 0
    s = 0
    while((r == 0) || (s == 0)){
	k = strtonum("0x" make_private_key()) % ORDER
	point_mul(G, k, R)
	r = R["x"] % ORDER
	st = (z + (r * priv)) % ORDER
	s = (st * inverse(k, ORDER)) % ORDER
	Sig["r"] = r
	Sig["s"] = s
	return(0)
    }
}
function verify(msg, sig, pub,      r, s, xy, y, z, w, u1, u2, P1, P2, P3) {
    r = sig["r"]
    s = sig["s"]
    x = pub["x"]
    y = pub["y"]
    z = strtonum("0x" hash_string(msg))
    w = inverse(s, ORDER)
    u1 = (z * w) % ORDER
    u2 = (r * w) % ORDER
    point_mul(G, u1, P1)
    point_mul(pub, u2, P2)
    point_add(P1, P2, P3)
    print("p3 r " P3["x"] " " r)
    return((P3["x"] % ORDER) == r)
}

function secp_test(){

    print("pow2 test")
    print(pow(2, 4) " " 16)
    print(pow(3, 3) " "  27)

    print("generator")
    #initialize_generator(G)
    print(G["x"] "," G["y"])

    print("order")
    print(ORDER)

    print("base")
    print(BASE)

    print("mul")
    print(mul(10, 10) == 100)
    print(mul(BASE-5, BASE-2) == 10)

    print("inverse")
    print(mul(5, inverse(5)) == 1)
    print("generator is")
    print(G["x"])
    print(G["y"])
    print("generator is on curve " on_curve(G))
    print("start calculation")
    point_mul(G, 3, G2)
    print("G2 is on curve " on_curve(G2))
    point_mul(G, ORDER, Neutral)
    print("neutral is on curve " on_curve(Neutral))
    print(Neutral["x"])
    print(Neutral["y"])
    point_add(G, Neutral, G2)
    print("here")
    print(G2["x"]  " , " G2["y"])
    print("G is G " ((G2["x"] == G["x"]) &&
		     (G2["y"] == G["y"])))
}

function shared_secret(Priv, Pub) {
    point_mul(Pub, Priv, Shared)
    return(Shared["x"])
}
	

BEGIN{
    #exit
 #print(57896044618658097711785492504343953926634992332820282019728792003956564819949 + 0)
    #finite field for numbers.
    #return(pow(2, 256) - pow(2, 32) - pow(2,9) - pow(2, 8) - pow(2, 7) - pow(2, 6) - pow(2, 4) - 1)
    BASE = strtonum("0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f")
    #print("base is " BASE)

    #number of points on the elliptic curve.
    ORDER = strtonum("0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141")
    #initialize the 
    initialize_generator(G)
    print("initialized")
    #secp_test()
    gen_key_pair(key)
    print("made key pair")
    message = "hello"
    sign(message, key["priv"], Sig)
    print("signed")

    gen_key_pair(key2)

    Secret1 = shared_secret(key["priv"], key2)
    Secret2 = shared_secret(key2["priv"], key)
    print("shared secret test " (Secret1 == Secret2))

    #print(key["priv"] " " key["x"] " " key["y"])
}
