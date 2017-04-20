
var addressA = document.createElement("div");
addressA.id  = "addressA";
document.body.appendChild(addressA);

function address_func()
{
    variable_get(["address"], address1);
}

setTimeout(address_func, 2000);

function address1(id)
{
    console.log("address 1");
    var balance = document.getElementById("addressA");
    balance.innerHTML = "address ".concat(atob(id));
    console.log("address 3");
}

