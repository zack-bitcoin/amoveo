var combine_cancel_assets_div = document.createElement("div");
combine_cancel_assets_div.id = "combine_cancel_assets_div";
document.body.appendChild(combine_cancel_assets_div);

combine_cancel_assets1();
function combine_cancel_assets1() {
    var div = document.getElementById("combine_cancel_assets_div");
    var button = document.createElement("BUTTON");
    var button_text_node = document.createTextNode("combine to cancel assets");
    button.appendChild(button_text_node);
    button.onclick = combine_cancel_assets2;
    div.appendChild(document.createElement("br"));
    div.appendChild(document.createElement("br"));
    div.appendChild(button);
    function combine_cancel_assets2() {
        variable_get(["combine_cancel_assets",
                      JSON.parse(server_ip.value),
                      parseInt(server_port.value, 10)],
                     combine_cancel_assets3);
    }
    function combine_cancel_assets3(x) {
        return 0
    }
}
