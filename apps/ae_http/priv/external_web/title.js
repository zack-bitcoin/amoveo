title_maker();
function title_maker() {
    var title = document.createElement("h1");
    //title.innerHTML = "Amoveo Light Wallet";
    //title.innerHTML = language_object.light_title[language_mode];
    title.innerHTML = get_words("light_title");
    document.body.appendChild(title);

    var github = document.createElement("p");
    github.innerHTML = ("<a href=\"https://github.com/zack-bitcoin/amoveo\">").concat(get_words("home_page")).concat("</a>");
    document.body.appendChild(github);

    var reddit = document.createElement("p");
    reddit.innerHTML = ("<a href=\"https://www.reddit.com/r/Amoveo/\">").concat(get_words("discuss_on_reddit")).concat("</a>");
    document.body.appendChild(reddit);

    var blockchain_title = document.createElement("h3");
    blockchain_title.innerHTML = get_words("blockchain");
    document.body.appendChild(blockchain_title);
};
