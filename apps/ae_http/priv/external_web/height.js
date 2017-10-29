//var height = document.createElement("div");
//height.id = "height";
//document.body.appendChild(height);
//function height_f(x) {
//    var h = document.getElementById("height");
//    b = (x).toString();
//    h.innerHTML = "current height: ".concat(b);
//}
//variable_public_get(["height"], function(x) {height_f(x)});

var height = document.createElement("div");
height.id = "height";
document.body.appendChild(height);
var height_button = document.createElement("BUTTON");
var button_text_node = document.createTextNode("update height");
height_button.appendChild(button_text_node);
height_button.onclick = height_helper;
document.body.appendChild(height_button);
function height_helper() {
    variable_public_get(["height"], height_f);
}
function height_f(x) {
    var h = document.getElementById("height");
    b = (x).toString();
    h.innerHTML = "current height: ".concat(b);
}
//variable_public_get(["height"], function(x) {height_f(x)});
//variable_get(["height"], height_f);
