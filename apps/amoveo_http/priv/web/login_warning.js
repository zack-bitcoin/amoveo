login_warning1();
function login_warning1() {
    var login_warning = document.createElement("div");
    login_warning.id = "login_warning";
    document.body.appendChild(login_warning);
    variable_get(["keys_status"], login_warning2);
}
function login_warning2(x) {
    if ( x == "unlocked" ) {
        console.log("ready");
    } else {
        
        var mydiv = document.getElementById("login_warning");
        var aTag = document.createElement('a');
        aTag.setAttribute('href',"/login.html");
        aTag.innerHTML = "warning, your private key is not decrypted. click here to decrypt your key so that you can spend money";
        mydiv.appendChild(aTag);
    }
}
    
