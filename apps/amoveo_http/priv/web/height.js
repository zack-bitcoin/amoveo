height1();
function height1() {
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(document.createElement("br"));
    var height = document.createElement("div");
    //height.id = "height";
    document.body.appendChild(height);
    var height_button = document.createElement("BUTTON");
    var button_text_node = document.createTextNode("update height");
    height_button.appendChild(button_text_node);
    height_button.onclick = height_helper;
    document.body.appendChild(height_button);
    function height_helper() {
        variable_get(["height"], height_f);
    }
    function height_f(height_integer) {
        //var h = document.getElementById("height");
        var height_string = (height_integer).toString();
        height.innerHTML = "current height: ".concat(height_string);
        console.log("updated height");
    }
}
//variable_public_get(["height"], function(x) {height_f(x)});
//variable_get(["height"], height_f);
