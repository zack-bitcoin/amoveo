(function() {
    document.body.appendChild(document.createElement("br"));
    document.body.appendChild(document.createElement("br"));
    var show_markets = document.createElement("input");
    show_markets.type = "button";
    show_markets.id = "show_markets_button";
    document.body.appendChild(show_markets);
    document.body.appendChild(document.createElement("br"));
    var div = document.createElement("div");
    document.body.appendChild(div);
    
    var lookup_oracle_div = document.createElement("div");
//    var lookup_oracle_address = "";
    var price = document.createElement("div");
    var canvas = document.createElement("canvas");
    show_markets2();
    function show_markets2() {
	list_oracles1();
	lookup_oracle1();
	var button = document.getElementById("show_markets_button");
	button.value = "hide markets";
	button.onclick = hide_markets;
    }
    function hide_markets() {
	div.innerHTML = "";
	var button = document.getElementById("show_markets_button");
	button.value = "show markets";
	button.onclick = show_markets2;
    }
    function list_oracles1() {
	lookup_oracle_div.innerHTML = "";
	div.appendChild(lookup_oracle_div);
	var lookup_oracle_button = document.createElement("BUTTON");
	var lookup_oracle_text_node = document.createTextNode("list markets");
	lookup_oracle_button.appendChild(lookup_oracle_text_node);
	lookup_oracle_button.onclick = lookup_helper;
	div.appendChild(lookup_oracle_button);
	function lookup_helper() {
            variable_public_get(["list_oracles"], lookup_helper2);
	}
    }
    function lookup_helper2(x) {
	lookup_oracle_div.innerHTML = "";
	var button = Array(x.length);
	for (var i = 1; i < x.length; i++) {
	    button[i] = document.createElement("input");
	    button[i].type = "button";
	    button[i].value = x[i];
	    button[i].onclick = (function(val) {
		return function() {
		    return lookup_oracle_helper(val);
		}
	    } )(x[i]);
	    lookup_oracle_div.appendChild(button[i]);
	}
    }
    function lookup_oracle1() {
	div.appendChild(document.createElement("br"));
	div.appendChild(document.createElement("br"));
	var lookup_oracle = document.createElement("div");
	div.appendChild(lookup_oracle);
	div.appendChild(price);
	var expires = document.createElement("div");
	div.appendChild(expires);
	var batch_period = document.createElement("div");
	div.appendChild(expires);
	var height = document.createElement("div");
	div.appendChild(height);
	var chart = document.createElement("div");
	div.appendChild(chart);
	canvas.id = "oracleCanvas";
	canvas.width = 900;//maybe number should be a string
	canvas.height = 500;
	div.appendChild(canvas);
    }
    function lookup_oracle_helper(x) {
	console.log("lookup oracle x is ");
	console.log(x);
	variable_public_get(["oracle", x], function(y) { return lookup_oracle_helper2(y, x)});
    }
    function lookup_oracle_helper2(l, oid) {
	price.innerHTML = "";
	var question = l.pop();
	var x = l.pop();
	var P = ((x[2])/100);
	var question_info = document.createElement("p");
	question_info.innerHTML = "betting on this: ".concat(atob(question)).concat("\n. . . . . . This is a market depth chart. yellow on the left are bets on 'true', grey on the right are bets on 'false'. The y-axis that goes up-and-down is the amount of Veo being bet. The x-axis is the odds of each bet, it is also the price. ");
	price.appendChild(question_info);
	var expires_info = document.createElement("p");
	expires_info.innerHTML = "expires at height: ".concat((x[6]).toString());
	price.appendChild(expires_info);
	var batch_info = document.createElement("p");
	batch_info.innerHTML = "batch period: ".concat((x[7]).toString());
	price.appendChild(batch_info);
	var height_info = document.createElement("p");
	height_info.innerHTML = "last height matched: ".concat((x[8]).toString());
	price.appendChild(height_info);
	console.log(x[9]);//scalar market data is in here.
	var oid_info = document.createElement("p");
	oid_info.innerHTML = "oracle id: ".concat(oid);
	price.appendChild(oid_info);
	var buys = price_amount(x[3]);
	var sells = price_amount(x[4]);
	var graph_height = Math.max(sum_amounts(buys), sum_amounts(sells));
	var ctx = canvas.getContext("2d");
	empty_canvas(ctx);
	background_color("black", ctx);
	draw_buys(graph_height, buys, ctx);
	var buy_color = "yellow";
	ctx.fillStyle = buy_color;
	ctx.fill();
	draw_sells("grey", graph_height, sells, ctx);
	draw_buys(graph_height, buys, ctx);
	ctx.lineWidth="2";
	ctx.strokeStyle = buy_color;
	ctx.stroke();
	ctx.fillStyle = "black";
	make_lines_vertical(numbers(180, 80, 9), ctx);
	make_lines_horizontal(numbers(40, 40, 19), ctx);
	x_units(ctx);
	y_units(graph_height, ctx);
    }
    
    function draw_sells(color, max_height, sells, ctx) {
	var buy_height = sum_amounts(sells);
	var portion = buy_height * 400 / max_height;
	ctx.beginPath();
	var x = 900;
	var y = 400;
	ctx.moveTo(x, y);
	y = y - portion;
	ctx.lineTo(x, y);
	for (var i = 0; i < sells.length; i++) {
            x = 900 - (sells[i][0] * 2 / 25);
            ctx.lineTo(x, y);
            y = y + (sells[i][1] * 400 / max_height);
            ctx.lineTo(x, y);
	}
	ctx.closePath();
	ctx.fillStyle = color;
	ctx.fill();
    }
    function draw_buys(max_height, buys, ctx) {
	var buy_height = sum_amounts(buys);
	var portion = buy_height * 400 / max_height;
	ctx.beginPath();
	var x = 100;
	var y = 400;
	ctx.moveTo(x, y);
	y = y - portion;
	ctx.lineTo(x, y);
	for (var i = 0; i < buys.length; i++) {
            x = 100 + (buys[i][0] * 2 / 25);
            ctx.lineTo(x, y);
            y = y + (buys[i][1] * 400 / max_height);
            ctx.lineTo(x, y);
	}
	ctx.closePath();
    }
    function numbers(start, gap, many) {
	// example numbers(10, 2, 5) -> [10, 12, 14, 16, 18].
	x = [];
	for (var i = 0; i < many; i++) {
            x.push(start + (i*gap));
	}
	return x;
    }
    function empty_canvas(ctx) {
	ctx.beginPath();
	ctx.rect(0, 0, 900, 500);
	ctx.fillStyle = "white";
	ctx.fill();
    }
    function background_color(c, ctx) {
	ctx.beginPath();
	ctx.rect(100, 0, 800, 400);
	ctx.fillStyle = c;
	ctx.fill();
    }
    function x_units(ctx) {
	ctx.font="20px Georgia";
	for (var i = 1; i < 10; i++) {
            ctx.fillText((i*10).toString(), 90 + (80 * i), 420);
	}
    }
    function y_units(M, ctx) {
	ctx.font="20px Georgia";
	for (var i = 1; i < 10; i++) {
            //ctx.fillText((M*(10-i)/10).toString(), 0, 6 + (40 * i));
            ctx.fillText((M*(10-i)/10/token_units()).toString(), 0, 6 + (40 * i));
	}
    }
    function make_lines_horizontal(l, ctx) {
	for (var i = 0; i < l.length; i++){
            make_line_horizontal(l[i], ctx);
	}
	return ctx;
    }
    function make_lines_vertical(l, ctx) {
	for (var i = 0; i < l.length; i++){
            make_line_vertical(l[i], ctx);
	}
	return ctx;
    }
    function make_line_horizontal(y, ctx) {
	return make_line(100, y, 900, y, ctx);
    }
    function make_line_vertical(x, ctx) {
	return make_line(x, 0, x, 400, ctx);
    }
    function make_line(a, b, c, d, ctx) {
	ctx.beginPath();
	ctx.lineWidth="1";
	ctx.strokeStyle="white";
	ctx.moveTo(a,b);
	ctx.lineTo(c, d);
	ctx.stroke(); // Draw it
    }
    function price_amount(L) {
	var x = [];
	for (var i = 1; i < L.length; i++) {
            x.push([L[i][2], L[i][4]]);
	}
	return x.reverse();
    }
    function sum_amounts(L) {
	var x = 0;
	for (var i = 0; i < L.length; i++) {
            x = x + L[i][1];
	}
	return x;
    }
}());
