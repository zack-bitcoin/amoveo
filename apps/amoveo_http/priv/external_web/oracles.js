(function(){

    if(tabs && nav){
        var tab_id = "oracles"
        var div = document.createElement("div");
        div.className = "tabs__content-item "+tab_id;
        div.id = tab_id;
        var title = document.createElement("h3");
        title.className = "tabs__nav-item";
        title.innerHTML = tab_id;
        title.dataset.tab = tab_id;

        if (!nav.hasChildNodes()) {
            title.className += " active";
            div.className += " active";
        }

        tabs.appendChild(div);
        nav.appendChild(title);

        var oid = document.createElement("INPUT");
        oid.type = "text";
        oid.className = "wide";
        var b = button_maker2("Lookup oracle", lookup);
        var oracleOutput = document.createElement("div");
        oracleOutput.className = "output"


        var wrap = document.createElement("div");
        wrap.className = "tabs__col";

        var wrap2 = document.createElement("div");
        wrap2.className = "tabs__col";
        wrap2.innerHTML = "<div class='tabs__box'>"+veo_text+"</div>";

        var fieldset1 = wrapper("fieldset", [oid, b]);
        append_children(wrap, [fieldset1]);
        append_children(div, [wrap, wrap2, oracleOutput]);

        function lookup() {
    	oracleOutput.innerHTML = "";
    	var v = oid.value;
    	merkle.request_proof("oracles", v, function(x) {
    	    var result = x[2];
    	    var question = x[3];
    	    var starts = x[4];
    	    var type = x[5];
    	    var done_timer = x[9];
    	    var governance = x[10];
    	    var governance_amount = x[11];
    	    var orders_hash = x[7];
    	    var a;
    	    if (result == 0) {
    		a = htitle("This oracle is still open");
    	    } else if (result == 1) {
    		a = htitle("This oracle closed in state: true");
    	    } else if (result == 2) {
    		a = htitle("This oracle closed in state: false");
    	    } else if (result == 3) {
    		a = htitle("This oracle closed in state: bad-question");
    	    }
    	    oracleOutput.appendChild(a);
    	    if (governance == 0) {
    		oracleOutput.appendChild(pre("This is a question oracle"));
    		var asks_txt = "asks: ".concat(btoa(btoa(question)));
    		oracleOutput.appendChild(pre(asks_txt));
    	    } else {
    		oracleOutput.appendChild(pre("This is a governance oracle"));
    		var gov_txt = "governance variable: ".concat(JSON.stringify(governance));
    		oracleOutput.appendChild(pre(gov_txt));
    		var gov_amount_txt = "governance amount: ".concat(JSON.stringify(governance_amount));
    		oracleOutput.appendChild(pre(gov_amount_txt));
    	    }
    	    var starts_txt = "starts: ".concat(JSON.stringify(starts));
    	    oracleOutput.appendChild(pre(starts_txt));
    	    var type2;
    	    if (type == 3) {
    		type2 = "bad-question";
    	    } else if (type == 1) {
    		type2 = "true";
    	    } else if (type == 2) {
    		type2 = "false";
    	    }
    	    var type_txt = "current type: ".concat(type2);
    	    oracleOutput.appendChild(pre(type_txt));
    	    var done_txt = "done timer: ".concat(JSON.stringify(done_timer));
    	    oracleOutput.appendChild(pre(done_txt));

    	    console.log("new");
    	    console.log(v);
    	    merkle.request_proof("orders", orders_hash, function(x) {
    		console.log(x);
    	    });
    	    //variable_public_get(["oracle_bets", v], oracle_bets);

    	    //now display the whole thing.
    	    var x2 = pre(JSON.stringify(x));
    	    oracleOutput.appendChild(x2);

    	});
        };
        function oracle_bets(x) {
            console.log("inside oracle bets");
            console.log(JSON.stringify(x));
        }
    }
})();
