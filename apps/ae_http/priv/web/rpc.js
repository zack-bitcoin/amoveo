//var IP = [146, 185, 142, 103];// server
//var IP = [127, 0, 0, 1];
//var Port = 8080;
//var Port = 3030;
function getter(t, u, callback){
    t = JSON.stringify(t);
    //console.log("getter ".concat(t));
    var xmlhttp=new XMLHttpRequest();
    xmlhttp.onreadystatechange = callback;
    xmlhttp.open("POST",u,true);
    xmlhttp.send(t);
    return xmlhttp
}
var PORT = parseInt(document.URL.substring(17, 21), 10);
/*function get(t, callback) {
    //u = url(PORT - 1, "localhost");
    u = url(get_port(), "localhost");
    return getter(t, u, callback);
}*/
function url(port, ip) { return "http://".concat(ip).concat(":").concat(port.toString().concat("/")); }
//PORT = 3010;
function local_get(t, callback) {
    u = url(PORT, "localhost");
    //u = url(get_port(), "localhost");
    return getter(t, u, callback);
}
function xml_check(x) { return ((x.readyState === 4) && (x.status === 200)); };
function xml_out(x) { return x.responseText; }
function refresh_helper(x, callback) {
    if (xml_check(x)) {callback(xml_out(x));}
    else {setTimeout(function() {refresh_helper(x, callback);}, 1000);}
};

my_status = "nil";
//var x = local_get(["test"]);

//refresh_helper(x, function(){ 
//    my_status = JSON.parse(xml_out(x)); 
//    console.log("test response ".concat(JSON.stringify(my_status)));
//});
function variable_get(cmd, callback) {
    var x = local_get(cmd);
    var_get(x, callback);
}
function variable_public_get(cmd, callback) {
    var x = get(cmd);
    var_get(x, callback);
}
function var_get(x, callback) {
    refresh_helper(x, function(){
	p = JSON.parse(xml_out(x));
	callback(p[1]);
    });
}

