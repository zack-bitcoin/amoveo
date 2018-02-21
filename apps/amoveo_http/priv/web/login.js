document.body.appendChild(document.createElement("br"));
document.body.appendChild(document.createElement("br"));
variable_get(["keys_status"], login_1);
function login_1(x) {
    if ( x == "locked" ) {
	login_locked();
    } else if ( x == "empty" ) {
	login_new();
    } else if ( x == "unlocked" ) {
	login_unlocked();
    } else {
        console.log("can't understand key status");
        console.log(x);
   }
}
function login_0(x) {
    if ( x == btoa("locked") ) {
	var con = document.createElement("p");
	con.innerHTML = "password no good";
	document.body.appendChild(con);	    
    } else if ( x == "unlocked" ) {
	login_unlocked();
   }
}
function login_new() {
    console.log("empty option");
    var new_password = document.createElement("INPUT");
    new_password.setAttribute("type", "text"); 
    var amount_info = document.createElement("h8");
    amount_info.innerHTML = "new password: ";
    document.body.appendChild(amount_info);
    document.body.appendChild(new_password);
    
    var new_password_check = document.createElement("INPUT");
    new_password_check.setAttribute("type", "text"); 
    var fee_info = document.createElement("h8");
    fee_info.innerHTML = "confirm new password: ";
    document.body.appendChild(fee_info);
    document.body.appendChild(new_password_check);
    
    var spend_button = document.createElement("BUTTON");
    spend_button.id = "spend_button";
    var spend_button_text = document.createTextNode("spend");
    spend_button.appendChild(spend_button_text);
    spend_button.onclick = function() {
	if (new_password.value == new_password_check.value) {
	    local_get(["keys_new", btoa(new_password.value)]);
	    variable_get(["keys_status"], login_0);
	} else {
	    var con = document.createElement("p");
	    con.innerHTML = "passwords don't match";
	    document.body.appendChild(con);	    
	}
    };
    document.body.appendChild(spend_button);
}
function login_unlocked() {
    console.log("unlocked option");
    var con = document.createElement("a");
    con.href = "/main.html";
    con.innerHTML = "continue to unlocked wallet";
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(con);
}
function login_locked() {
    var password = document.createElement("INPUT");
    password.setAttribute("type", "text"); 
    var password_info = document.createElement("h8");
    password_info.innerHTML = "password: ";
    document.body.appendChild(password_info);
    document.body.appendChild(password);
    
    var login_button = document.createElement("BUTTON");
    login_button.id = "login_button";
    var login_button_text = document.createTextNode("continue");
    login_button.appendChild(login_button_text);
    login_button.onclick = function() {
	local_get(["keys_unlock", btoa(password.value)]);
	variable_get(["keys_status"], function(x) {login_0(x)});
    };
    document.body.appendChild(login_button);
}
