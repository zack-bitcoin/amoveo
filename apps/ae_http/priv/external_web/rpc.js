function getter(t, u, callback){
    t = JSON.stringify(t);
    //console.log("getter ".concat(t));
    var xmlhttp=new XMLHttpRequest();
    xmlhttp.onreadystatechange = callback;
    xmlhttp.open("POST",u,true);
    xmlhttp.send(t);
    return xmlhttp
}
//var PORT = parseInt(document.URL.substring(17, 21), 10);
function get(t, callback) {
    //u = url(PORT, IP);
    u = url(get_port(), get_ip());
    return getter(t, u, callback);
}
function url(port, ip) { return "http://".concat(ip).concat(":").concat(port.toString().concat("/")); }
/*function local_get(t, callback) {
    //u = url(PORT, "localhost");
    u = url(PORT, IP);
    return getter(t, u, callback);
}*/
function xml_check(x) {
    return ((x.readyState === 4) && (x.status === 200)); };
function xml_out(x) { return x.responseText; }
function refresh_helper(x, callback, n) {
    if (n < 1) { return "failed to connect"; }
    else if (xml_check(x)) {callback(xml_out(x));}
    else {setTimeout(function() {refresh_helper(x, callback, n - 1);}, 1000);}
};

my_status = "nil";

function variable_get(cmd, callback) {
    var x = local_get(cmd);
    var_get(x, callback);
}
function variable_public_get(cmd, callback) {
    var foobar = get(cmd);
    var_get(foobar, callback);
}
function var_get(x, callback) {
    refresh_helper(x, function(){
	p = JSON.parse(xml_out(x));
	callback(p[1]);
    }, 5);
}

