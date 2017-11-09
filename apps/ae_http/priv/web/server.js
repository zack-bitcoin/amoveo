//make ip and port as input things.

var server_ip = document.createElement("INPUT");
server_ip.setAttribute("type", "text");
server_ip.value = "[146, 185, 142, 103]";// server
var server_ip_info = document.createElement("h8");
server_ip_info.innerHTML = "channel node ip: ";
var server_port = document.createElement("INPUT");
server_port.value = "8080";// server
server_port.setAttribute("type", "text");
var server_port_info = document.createElement("h8");
server_port_info.innerHTML = " port: ";
document.body.appendChild(server_ip_info);
document.body.appendChild(server_ip);
document.body.appendChild(server_port_info);
document.body.appendChild(server_port);


document.body.appendChild(document.createElement("br"));
document.body.appendChild(document.createElement("br"));
