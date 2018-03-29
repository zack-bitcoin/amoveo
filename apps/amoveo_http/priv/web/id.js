var id_number = document.createElement("div");
id_number.id = "id_number";
document.body.appendChild(id_number);
function id_number_func() {
    variable_get(["id"], id_number1);
}
setTimeout(id_number_func, 2000);

function id_number1(id) {
    var balance = document.getElementById("id_number");
    //balance.innerHTML = "id_number ".concat(atob(id));
    balance.innerHTML = "id ".concat(id);
}
