function getter(t, u, callback){
    var xmlhttp=new XMLHttpRequest();
    xmlhttp.onreadystatechange = callback;
    xmlhttp.open("POST",u,true);
    xmlhttp.send(JSON.stringify(t));
    return xmlhttp
}
var PORT = parseInt(document.URL.substring(17, 21), 10);
function url(port, ip) { return "http://".concat(ip).concat(":").concat(port.toString().concat("/")); }
function local_get(t, callback) {
    u = url(PORT, "localhost");
    return getter(t, url(PORT, "localhost"), callback);
}
function xml_check(x) {
    return ((x.readyState === 4) && (x.status === 200)); };
function xml_out(x) { return x.responseText; }
function refresh_helper(x, cmd, innercallback, callback, n) {
    if (n < 1) { return "failed to connect"; }
    else if (x.status == 400) {
        setTimeout(function() {
            return variable_get(cmd, innercallback);
        }, 200); }
    else if (x.status == 0)
    {
        setTimeout(function()
                   {
                       return refresh_helper(x,
                                             cmd,
                                             innercallback,
                                             callback,
                                             n - 1);
                   },
                   150);
    }
    else if (xml_check(x)) {return callback(xml_out(x));}
    else {
        console.log("should not happen");
        setTimeout(function() {return refresh_helper(x, cmd, innercallback, callback, n - 1);}, 150);}
};

my_status = "nil";
//var x = local_get(["test"]);

//refresh_helper(x, function(){ 
//    my_status = JSON.parse(xml_out(x)); 
//    console.log("test response ".concat(JSON.stringify(my_status)));
//});
function variable_get(cmd, callback) {
    return var_get(local_get(cmd), callback, cmd);
}
//function variable_public_get(cmd, callback) {
//    var_get(get(cmd), callback);
//}
function var_get(x, callback, cmd) {
    return refresh_helper(x, cmd, callback, function(){
        var xml_is = xml_out(x);
	p = JSON.parse(xml_is);
	return callback(p[1]);
    }, 5);
}

