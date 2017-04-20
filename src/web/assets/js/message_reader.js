setTimeout(get_message, 50000000);

function get_message()
{
    console.log("get message");

    local_get(["get_msg", IP, Port]);

    setTimeout(get_message, 40000000);
}
