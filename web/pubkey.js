var pubkey = document.createElement("div");
pubkey.id = "pubkey";
document.body.appendChild(pubkey);
function pubkey_func() {
    variable_get(["pubkey"], pubkey1);
}
setTimeout(pubkey_func, 2000);

function pubkey1(id) {
    console.log("pubkey 1");
    var balance = document.getElementById("pubkey");
    balance.innerHTML = "pubkey ".concat(atob(id));
    console.log("pubkey 3");
}
