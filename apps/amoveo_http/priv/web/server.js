//make ip and port as input things.

local_ip = [127,0,0,1];
local_port = 8081;
var server_ip = document.createElement("INPUT");
server_ip.setAttribute("type", "text");
server_ip.value = "[139,59,144,76]";// server
//server_ip.value = "[146, 185, 142, 103]";// server
//server_ip.value = "[127, 0, 0, 1]";// server
var server_ip_info = document.createElement("h8");
server_ip_info.innerHTML = "channel node ip: ";
var server_port = document.createElement("INPUT");
server_port.value = "8080";// server
//server_port.value = document.URL.substring(17,21);// server
server_port.setAttribute("type", "text");
var server_port_info = document.createElement("h8");
server_port_info.innerHTML = " port: ";
document.body.appendChild(server_ip_info);
document.body.appendChild(server_ip);
document.body.appendChild(server_port_info);
document.body.appendChild(server_port);


document.body.appendChild(document.createElement("br"));
document.body.appendChild(document.createElement("br"));

function get_port() {
    return parseInt(server_port.value, 10);
}
function get_ip() {
    return JSON.parse(server_ip.value);
}
