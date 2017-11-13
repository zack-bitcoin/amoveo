
//pubkeys
pub1 = atob("BG1gMoweqvCGQLibo/PkAeictba6qGoyYNWcA8dEK29FaI0pktEwte6HKTCV/AT9IBLfc7iZ+fVlAB0YjdC09lM=");
pub2 = atob("BLO4W9Peld0TXMa1IcbQqGDS6O2BOJT8S3GECUMmcCyW46n3iarc6aVW6+DuiGJvKmmEP+Vp7l7i8rlBBnS2osM=");

//privkeys
priv1 = atob("2kYbRu2TECMJzZy55fxdILBvM5wJM482lKLTRu2e42U=");
priv2 = atob("YJx/2r3S6+wOPhu7HHAt5g39a8LMsAC1m1Ff6Unvjzk=");

//signed tx
//stx = ["signed",["channel_block",0,1],"TUVVQ0lGUmdLQlZZcTlCdFJZOUdHNHR3b2JYc1hDYzJHSTRvQ0UzKytraExwemN1QWlFQS8zTFZEbDlaVmk3MC90aEx5SlJXYlc0NnRMakRUUldXb2FTdllHcmlnN3c9","TUVVQ0lRQzVsTW9VVWxFbmJ1blNiVGVCZUk0VlRzTDZ3UEVERnBwU1B0dDBwRnZOL1FJZ1drM21mKzhtcXJmTEVDYWZFYkdMTWRjVDFWMVBWSjYwK3U3RVNBckdIMTg9",[-6]];
stx = ["signed",["create_acc_tx","BL18ctFCJ4/i0HuiIJbF/F/ktgjADSub5rbe3RBIrsDUHBUVqqoEmv5wLEmjq3d8pTA07J2PQo87CY2B+0baQQk=",1,20,"BJjOADT/mMg0BsqQkCDcEb/ylv6W85wipEKrY3qV5z3XvVrNygvVoEXsA6tncAoMuyvMB5Prepzqql3zZ1sDjjo=",150000000000],"TUVZQ0lRRGNveWNkZDRxM3JmMTdHZjJPWndBV0w4OFJ4QzJKeEQvYmxsU1R0cE55bWdJaEFQcTJjNS8xVkVvWG5xd1FvN29WK1F6WjJvRjVqK29MekF5ZVBjVGREbm5F",[-6]];
tx = stx[1];
//console.log(JSON.stringify(tx));
//console.log(pub1);


var EC = elliptic.ec
var ec = new EC('secp256k1');
var key1 = ec.genKeyPair({entropy: priv1});
var key2 = ec.genKeyPair({entropy: priv2});
//var key1 = ec.keyFromSecret(toHex(priv1), 'hex');
//var key2 = ec.keyFromSecret(toHex(priv2), 'hex');
//var key2 = ec.keyFromPublic(toHex(pub2), 'hex');
//var key = ec.genKeyPair();

//var msg = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
//var signature = key.sign(msg);

//var key2 = ec.keyFromPublic(pub1, "");

function toHex(str) {
    var hex = '';
    for(var i=0;i<str.length;i++) {
	hex += ''+str.charCodeAt(i).toString(16);
    }
    return '0'+hex;
}

console.log(toHex(pub2));
//04b3b85bd3de95dd135cc6b521c6d0a860d2e8ed813894fc4b718494326702c96e3a9f789aadce9a556ebe0ee88626f2a69843fe569ee5ee2f2b941674b6a2c3

function sign(data, key) {
    //ecdsa, sha356
    var d2 = json2erlb(data);
    var h = hash(d2);
    var sig = key.sign(h);
    return sig.toDER();
}
function verify(data, sig, key) {
    var d2 = json2erlb(data);
    var h = hash(d2);
    return key.verify(h, sig);
    
}

console.log(verify([], sign([], key1), key1));

bytes_test  = [131,104,10,100,0,6,104,101,97,100,101,114,97,0,109,0,0,
              0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,109,0,0,0,32,35,140,48,80,16,197,111,105,
              90,87,4,86,159,208,106,228,50,120,168,222,27,214,44,154,
              109,158,154,150,130,224,222,51,109,0,0,0,32,161,153,111,
              202,251,68,226,226,141,196,169,144,98,226,25,169,4,209,
              137,191,155,230,168,116,187,196,23,202,9,163,215,194,97,
              0,98,0,0,25,52,97,6,109,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,97,0];
console.log(JSON.stringify(erlb2json(bytes_test)));

var data0 = stx[1];
var sig0 = toHex(atob(stx[3]));
var key0 = ec.keyFromPublic(toHex(atob(stx[1][1])), "hex");
var foo = verify(data0, sig0, key0);
console.log(foo);
