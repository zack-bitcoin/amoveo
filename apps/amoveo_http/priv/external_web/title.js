title_maker();
function title_maker() {
    var title = document.createElement("h1");
    title.innerHTML = "".concat(translate.words("light_title")).concat(translate.words("amoveo"));
    document.body.appendChild(title);

    var github = document.createElement("p");
    github.innerHTML = ("<a href=\"https://github.com/zack-bitcoin/amoveo\">").concat(translate.words("home_page")).concat("</a>");
    document.body.appendChild(github);

    var reddit = document.createElement("p");
    reddit.innerHTML = ("<a href=\"https://www.reddit.com/r/Amoveo/\">").concat(translate.words("discuss_on_reddit")).concat("</a>");
    document.body.appendChild(reddit);

    var explorer = document.createElement("p");
    explorer.innerHTML = ("<a href=\"/explorer.html\">").concat("Explorer on same server").concat("</a>");
    document.body.appendChild(explorer);

    var blockchain_title = document.createElement("h3");
    blockchain_title.innerHTML = translate.words("blockchain");
    document.body.appendChild(blockchain_title);
};
