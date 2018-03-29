(function(){
    var height = document.createElement("div");
    height.id = "height";
    document.body.appendChild(height);
    var height_button = document.createElement("BUTTON");
    var button_text_node = document.createTextNode("update height");
    height_button.appendChild(button_text_node);
    height_button.onclick = height_helper;
    document.body.appendChild(height_button);
    function height_helper() {
	variable_public_get(["height"], height_f);
    }
    function height_f(x) {
	height.innerHTML = "";
	//var h = document.getElementById("height");
	b = (x).toString();
	//height.innerHTML = "current height: ".concat(b);
	var p1 = document.createElement("h");
	p1.innerHTML = "current height: ".concat(b);
	height.appendChild(p1);
	height.appendChild(document.createElement("br"));
	variable_public_get(["f", 1], function(d) {
	    var hpb = Math.round(d[1]/1000);
	    var hpb2 = document.createElement("h");
	    hpb2.innerHTML = "terahashes per block: ".concat((hpb).toString());
	    height.appendChild(hpb2);
	    height.appendChild(document.createElement("br"));
	    var hps = d[2];
	    var hps2 = document.createElement("h");
	    hps2.innerHTML = "gigahashes per second: ".concat((hps).toString());
	    height.appendChild(hps2);
	    height.appendChild(document.createElement("br"));
	    var bp = d[3];
	    var bp2 = document.createElement("h");
	    bp2.innerHTML = "seconds per block: ".concat((bp).toString());
	    height.appendChild(bp2);
	    height.appendChild(document.createElement("br"));
	    //hashes per block, hashes per second, block period
	});
    }
})();
