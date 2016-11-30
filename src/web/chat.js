var chat_button = document.createElement("BUTTON");
chat_button.id = "chat_button";
var t = document.createTextNode("load messages");
chat_button.appendChild(t);
function chat_func() {// variable_get(["msg_peers"], chat_func2); }
//function chat_func2(peers) {
    start = -1;
    var chat_buddy = parseInt(document.getElementById("talk_address").value, 10);
    //console.log(peers[1]);
    variable_get(["msg_ids", chat_buddy], function(x) {chat_func3(x, chat_buddy)} );
}
function chat_func3(ids, partner) {
    console.log(ids);
    msgs = document.getElementById("messages");
    //replacedNode = msgs.replaceChild(ul, msgs);
    //msgs.replaceChild(ul, msgs);
    while (msgs.firstChild) {
	msgs.removeChild(msgs.firstChild);
    }
    chat_func4(ids, partner, 1, ids.length);
}
function chat_func4(ids, partner, N, M) {
    console.log("chat 4");
    console.log(N);
    if (N > M) {
	console.log("done");
    } else if (ids[N] < start ) {
	chat_func4(ids, partner, N + 1, M);
    } else if (ids[N] > start ) {
	start = ids[N];
	variable_get(["read_msg", partner, parseInt(ids[N])], function(msg) {
	    msgs = document.getElementById("messages");
	    var li = document.createElement("li");
	    console.log(msg);
	    li.innerHTML = atob(msg);
	    msgs.appendChild(li);
	    chat_func4(ids, partner, N + 1, M);
	}
		    );
    } else {
	chat_func4(ids, partner, N+1, M);
    }
}

chat_button.onclick = chat_func;
document.body.appendChild(document.createElement("br"));
document.body.appendChild(chat_button);
var scrollBox = document.createElement("div");
//scrollBox.style = "height:120px;width:120px;border:1px solid #ccc;font:16px/26px Georgia, Garamond, Serif;overflow:auto;";
scrollBox.style = "height:400px;width:500px;border:1px solid #ccc;font:14px/14px Georgia, Garamond, Serif;overflow:auto;";
var ul = document.createElement("ul");
ul.id = "messages";
scrollBox.appendChild(ul);
scrollBox.id = "scrollBox"
document.body.appendChild(scrollBox);
