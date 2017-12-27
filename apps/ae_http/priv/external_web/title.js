title_maker();
function title_maker() {
    var title = document.createElement("h1");
    //title.innerHTML = "Amoveo Light Wallet";
    title.innerHTML = language_object.light_title[language_mode];
    document.body.appendChild(title);

    var github = document.createElement("p");
    github.innerHTML = "<a href=\"https://github.com/zack-bitcoin/amoveo\"> Amoveo homepage </a>";
    document.body.appendChild(github);

    var reddit = document.createElement("p");
    reddit.innerHTML = "<a href=\"https://www.reddit.com/r/Amoveo/\"> discuss on reddit </a></p>";
    document.body.appendChild(reddit);
};
