var balance = 1000;
var round = 1;
var marketProfit = 0;

var div = document.createElement("div");
document.body.appendChild(div);

var balanceDiv = document.createElement("div");
div.appendChild(balanceDiv);
display_balance();
function display_balance() {
    balanceDiv.innerHTML = ("your balance: ").concat(balance.toString());
}

div.appendChild(document.createElement("br"));
var roundDiv = document.createElement("div");
div.appendChild(roundDiv);
display_round();
function display_round() {
    roundDiv.innerHTML = ("round number: ").concat((round).toString());
}

div.appendChild(document.createElement("br"));
var marketDiv = document.createElement("div");
div.appendChild(marketDiv);
display_market();
function display_market() {
    marketDiv.innerHTML = ("the finance system made this much profit from serving you: ").concat((marketProfit).toString());
}

var interfaceDiv = document.createElement("div");
div.appendChild(interfaceDiv);

interfaceDiv.appendChild(document.createElement("br"));
var investAmount = document.createElement("input");
investAmount.type = "text";
investAmount.value = "0";
var investAmountInfo = document.createElement("h8");
investAmountInfo.innerHTML = "amount to invest: ";
interfaceDiv.appendChild(investAmountInfo);
interfaceDiv.appendChild(investAmount);

interfaceDiv.appendChild(document.createElement("br"));
var insureAmount = document.createElement("input");
insureAmount.type = "text";
insureAmount.value = "0";
var insureAmountInfo = document.createElement("h8");
insureAmountInfo.innerHTML = "amount of insurance to purchase: ";
interfaceDiv.appendChild(insureAmountInfo);
interfaceDiv.appendChild(insureAmount);

interfaceDiv.appendChild(document.createElement("br"));
var doneButton = document.createElement("BUTTON");
var doneButtonText = document.createTextNode("next round");
doneButton.appendChild(doneButtonText);
doneButton.onclick = function(){
    var insure = parseInt(insureAmount.value, 10);
    var invest = parseInt(investAmount.value, 10);
    if ((insure + invest) > balance) {
        console.log("not enough funds");
    } else {
        round +=1;
        display_round();
        
        var r = Math.floor(Math.random() * 10);
        if (r > 3) {
            console.log("exports are possible");
            balance = balance + invest - insure;
            marketProfit = marketProfit + insure;
            
        } else {
            console.log("exports are blocked");
            balance = balance - invest + Math.floor((14 * insure) / 10);
            marketProfit = marketProfit - insure;
        }
        display_balance();
        display_market();
        if (round == 6) {
            var result = document.createElement("div");
            div.appendChild(result);
            interfaceDiv.innerHTML = "";
            if (balance > 1500) {
                result.innerHTML = "you win!";
            } else {
                var loser = document.createElement("div");
                result.innerHTML = "you lose!";
            }
        }
    }
}
interfaceDiv.appendChild(doneButton);
