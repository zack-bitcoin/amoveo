var recover_div = document.createElement("div");
recover_div.id = "recover_div";
document.body.appendChild(recover_div);
recover_channel1();
function recover_channel1() {
    var div = document.getElementById("recover_div");
    div.appendChild(document.createElement("br"));
    div.appendChild(document.createElement("br"));
    var button = document.createElement("BUTTON");
    var button_text_node = document.createTextNode("recover channel after reinstalling/updating/switching computers, or checks if any bets have been settled. ");
    button.appendChild(button_text_node);
    button.onclick = recover_channel2;
    div.appendChild(button);
    function recover_channel2() {
        variable_get(["pull_channel_state",
                      JSON.parse(server_ip.value),
                      parseInt(server_port.value, 10)], recover_channel3);
    }
    function recover_channel3() {
        return made_channel_refresh();
    }
}
