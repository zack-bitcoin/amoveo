setTimeout(get_message, 5000);
function get_message() {
    console.log("get message");
    local_get(["get_msg", IP, Port]);
    setTimeout(get_message, 4000);
}
