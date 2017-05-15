setTimeout(get_message, 10000);
function get_message() {
    console.log("get message");
    //local_get(["get_msg", IP, Port]);
    //mine:is_on().
    local_get(["mine:is_on()"]);
    setTimeout(get_message, 8000);
}
