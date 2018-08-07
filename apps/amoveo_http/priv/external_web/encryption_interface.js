(function(){
    var div = document.createElement("div");
    document.body.appendChild(div);
    var title = document.createElement("h3");
    title.innerHTML = "encryption";
    div.appendChild(title);
    var b = button_maker2("encrypt", encrypt);
    div.appendChild(b);
    var msg_to_send = document.createElement("INPUT");
    msg_to_send.type = "text";
    var msg_to = document.createElement("INPUT");
    msg_to.type = "text";
    var encrypted_to_send = document.createElement("div");
    var to_instructions = document.createElement("h8");
    to_instructions.innerHTML = "to pubkey";
    div.appendChild(to_instructions);
    div.appendChild(msg_to);
    var msg_instructions = document.createElement("h8");
    msg_instructions.innerHTML = "message to send";
    div.appendChild(msg_instructions);
    div.appendChild(msg_to_send);
    div.appendChild(br());
    div.appendChild(encrypted_to_send);
    div.appendChild(br());
    var b2 = button_maker2("decrypt", decrypt);
    var encrypted_received = document.createElement("INPUT");
    encrypted_received.type = "text";
    var decrypted_received = document.createElement("div");
    div.appendChild(b2);
    div.appendChild(encrypted_received);
    div.appendChild(br());
    div.appendChild(decrypted_received);
    div.appendChild(br());
    function encrypt() {
	var t = msg_to_send.value;
	var to = msg_to.value;
	var t2 = keys.encrypt(t, to);
	encrypted_to_send.innerHTML = JSON.stringify(t2);
    }
    function decrypt() {
	var t = encrypted_received.value;
	var t2 = keys.decrypt(JSON.parse(t));
	decrypted_received.innerHTML = t2;
    }
})();
