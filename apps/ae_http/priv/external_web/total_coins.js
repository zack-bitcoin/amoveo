var total_coins = document.createElement("div");
total_coins.id = "total_coins";
document.body.appendChild(total_coins);
function total_coins_f(x) {
    var h = document.getElementById("total_coins");
    console.log(x);
    b = (x).toString();
    h.innerHTML = "current total coins: ".concat(b);
}
variable_public_get(["total_coins"], function(x) {total_coins_f(x)});
