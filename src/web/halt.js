halt1();
function halt1() {
    document.body.appendChild(document.createElement("br"));
    var halt_button = document.createElement("BUTTON");
    halt_button.id = "halt_button";
    var halt_button_text = document.createTextNode("halt");
    halt_button.appendChild(halt_button_text);
    halt_button.onclick = function() {
	local_get(["halt"]);
    };
    document.body.appendChild(halt_button);

}
