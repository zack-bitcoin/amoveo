(function() {
	var tab_id = "encryption";

	var div = document.createElement("div");
	div.className = "tabs__content-item " + tab_id;
	div.id = tab_id;

	var title = document.createElement("h3");
	title.className = "tabs__nav-item";
	title.dataset.tab = tab_id;
	title.innerHTML = tab_id;

	if (!nav.hasChildNodes()) {
		title.className += " active";
		div.className += " active";
	}

	tabs.appendChild(div);
	nav.appendChild(title);

	var b = button_maker2("Encrypt", encrypt);

	var msg_to_send = document.createElement("INPUT");
	msg_to_send.type = "text";
	msg_to_send.id = "msg_to_send";
	var msg_to = document.createElement("INPUT");
	msg_to.type = "text";
	var encrypted_to_send = document.createElement("div");
	var to_instructions = document.createElement("label");
	to_instructions.innerHTML = "To pubkey";

	var msg_instructions = document.createElement("label");
	msg_instructions.innerHTML = "Message to send";

	var b2 = button_maker2("Decrypt", decrypt);
	var encrypted_received = document.createElement("TEXTAREA");
	encrypted_received.type = "text";
	var decrypted_received = document.createElement("div");

	var wrap = document.createElement("div");
	wrap.className = "tabs__col";
	var wrap2 = document.createElement("div");
	wrap2.className = "tabs__col";

	var fieldset1 = wrapper("fieldset", [to_instructions, msg_to]);
	var fieldset2 = wrapper("fieldset", [msg_instructions, msg_to_send]);

	append_children(wrap, [fieldset1, fieldset2, b, encrypted_to_send, hr()]);
	append_children(wrap, [encrypted_received, b2, decrypted_received]);
	append_children(div, [wrap, wrap2]);

	function encrypt() {
		var t = msg_to_send.value;
		var to = msg_to.value;
		var t2 = keys.encrypt(t, to);
		encrypted_to_send.innerHTML = "<pre>"+JSON.stringify(t2)+"</pre>";
	}

	function decrypt() {
		var t = encrypted_received.value;
		var t2 = keys.decrypt(JSON.parse(t));
		decrypted_received.innerHTML = "<p class='msg'>"+t2+"</p>";
	}
})();
