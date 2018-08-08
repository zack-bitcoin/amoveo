(function() {
    var title = document.createElement("h1");
    title.innerHTML = "Blockchain Explorer Amoveo";
    document.body.appendChild(title);

    var github = document.createElement("p");
    github.innerHTML = "<a href=\"https://github.com/zack-bitcoin/amoveo\">home page</a>";
    document.body.appendChild(github);

    var reddit = document.createElement("p");
    reddit.innerHTML = ("<a href=\"https://www.reddit.com/r/Amoveo/\">discuss on reddit</a>");
    document.body.appendChild(reddit);

    var wallet = document.createElement("p");
    wallet.innerHTML = ("<a href=\"/wallet.html\">").concat("Wallet on same server").concat("</a>");
    document.body.appendChild(wallet);

    var blockchain_title = document.createElement("h3");
    blockchain_title.innerHTML = "blockchain ";
    document.body.appendChild(blockchain_title);
})();
