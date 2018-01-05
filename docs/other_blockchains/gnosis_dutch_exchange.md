https://blog.gnosis.pm/introducing-the-gnosis-dutch-exchange-53bd3d51f9b2


Nadja Beneš
Brand & Content Strategy @gnosis.pm
Dec 15, 2017
Introducing the Gnosis Dutch Exchange

In the past months, a team of mathematicians and developers at Gnosis took a close look at both centralized and decentralized implementations of exchanges. After facing some serious limitations with both types of exchanges, we decided to work on an entirely new solution — a decentralized exchange for ERC-20 tokens based on the Dutch auction principle.

Before introducing our exchange model in detail next week, we’d like to give a brief overview of centralized exchanges, highlight their shortcomings and explain how decentralized models can solve some of the aforementioned problems. Finally, we’ll introduce the auction mechanism and show how it’s a very efficient way of determining a fair price while also eliminating the problem of front-running that order book based decentralized exchanges face.
Centralized Order Book Exchanges

Centralized exchanges (Kraken, Coinbase, Bittrex, Poloniex, HitBTC, Binance, etc.) are based on a centralized order book. An order book contains a list of buy and sell orders for a specific token, organized by price level. It lists the numbers of tokens being bid or offered at each price point.

An order to buy is called a “bid” and an order to sell is called an “ask”. Bids and asks are paired up as soon as their requirements are fulfilled, resulting in a trade. Orders for which the bid price is equal or higher than the lowest ask can be immediately fulfilled and will not be part of the open order book.

The highest bid and the lowest ask are referred to as the top of the book. They signal the prevalent market sentiment and the bid and ask price that would be needed to get an order fulfilled. The difference between the highest bid and the lowest ask is called the spread.

On order book based exchanges, you can submit two types of orders: a market order or a limit order. When submitting a market order, you buy or sell immediately for the best available price. Market orders are filled by instantly pairing buyers and sellers with orders currently in the books. When submitting a limit order, you buy or sell a set number of tokens at a specified price or better (f.ex., you bid $800 to buy 2 ETH at $400).
Order book based exchange MtGox (image via TradeBlock)
Shortcomings

One of the major shortcomings of centralized exchanges is the high risk of fund loss. With centralized exchanges, the user sends funds to the exchange — these funds are then held by a wallet owned by the exchange, and the equivalent value is credited to the user’s account via a database entry. Both the user and the exchange incur counterparty risk: theft (by the exchange or from the exchange through third-party hackers) or withdrawal capacity restrictions, to name a few.

Just imagine if someone found the private key of the exchange wallet owner or manages to hack the exchange, and transfers all the available funds to their private wallet. Since transactions are instant and irreversible once mined on the blockchain, the owners will most likely never see their money again. Fraud protections traditional bank depositors rely on are unavailable, and not even a government or central bank can stop a cryptocurrency transaction from happening.

Exchanges have indeed been hacked several times: The largest hack occurred in February 2014, when hackers stole approximately 850,000 bitcoins (worth around $400 million at the time, today worth about $14 billion) from Tokyo based bitcoin exchange Mt. Gox that handled 70% of all bitcoin transactions at the time of the hack. Employees at Bitstamp, a bitcoin exchange from Luxembourg, were targeted in a weeks-long phishing attempt in 2015–18,866 bitcoins were lost, worth approximately $5.2 million back then (about $315 million today). That same year, thieves grabbed nearly 120,000 bitcoins (worth $72 million then, currently worth about $2 billion) from Bitfinex, a crypto exchange based in Hong Kong. Even last week, hackers compromised Slovenian-based bitcoin mining marketplace NiceHash and stole 4700 bitcoin, worth about $80m at current prices.
News on Mt.Gox large-scale hack in 2014
Decentralized Order Book Exchanges

With decentralized exchanges, funds always reside in the users’ control. Users own the account and hold the private keys to access it, which means that they are the only ones who can move their funds — even in the event that the exchange goes down. Funds which have been used to purchase tokens will be stored in automated smart contracts, which are incorruptible by humans. Through decentralization, the user does no longer need to trust the exchange — risks are removed from both the exchange and the user.
Shortcomings

While order book based exchanges, both centralized and decentralized, work quite well for liquid markets, they aren’t an optimal model for illiquid markets. Since within an order book based exchange, your order will only be fulfilled if your bid price matches an ask price or vice versa, you can end up not being able to make a trade since your highest bid might still be lower than the lowest ask.

Hence, with illiquid markets, order book based exchanges result in large spreads and very variable prices, on top of the regular volatility that already comes with cryptocurrencies.

Another major problem order book based decentralized exchanges are facing is front-running. Since on an order book based exchange, you can submit an order to trade like you would submit a transaction to the blockchain, miners can see all the transactions before they are put into a block. That means that they are free to put in their own transaction into the block before your transaction is put in.

For example, let’s say you submit a large order to buy a token on EtherDelta. Once this order is seen by a miner who realizes that your order is big enough to drive up the price, they can create their own order to buy the token and put it in the blockchain before you do. Then, assuming that the miner is able to mine this particular block, they would have earned a risk-free profit by taking advantage of the information in your order.
Image via Fortune

Front-running of course also is a big problem on centralized exchanges where even the exchange could act on orders before they’re published (which is illegal but may still happen), procuring profits at the expense of other participants.

An additional shortcoming of decentralized order book exchanges is that traders are no longer rewarded if they reveal price information by giving them priority of trade execution. On centralized order book exchanges, the priority of trade execution encourages traders with new price information to publicly disclose it and, as a result, improve the price accuracy of the underlying asset. In decentralized order book exchanges, the miners are always favored to have the priority of execution, which leaves traders with no incentive to reveal price information.

As decentralized order book based exchanges are inherently set up in favor of the miners, we decided to design an exchange that is built on an auction mechanism.
Auction Based Decentralized Exchanges

In an auction based exchange, there are discrete windows for traders to submit buy and sell orders. First, all asks are collected during a time period before the auction starts. Then, once the auction has started, the buyers submit their bid at the point in time where the current price reflects their maximum willingness to pay.
Image via Newsfeed.org

Instead of trading continuously, the exchange would collect the sell orders as batches until the auction starts, and clear them at the end of the auction all at once.

Therefore, by accumulating orders that are executed at the same time, a batch auction exchange not only represents a better price finding mechanism than an order book, but also eliminates the inherent flaw of the order book exchange: front-running — the discrete advantage of miners being able to act on transactions before they are put into a block. With batched orders entering the block at the time of auction, miners will no longer be able to game the system.

That being said, one drawback of the batch auction model is that the exchange is not instantaneous (and funds can’t immediately be withdrawn), which makes fast trading impossible. While the Gnosis Dutch auction will partially remedy this problem for buyers, it is, however, often secondary to achieving a fair price for most users.

We look forward to introducing the Gnosis Dutch Exchange in a follow-up post next week!

Thanks to Friederike, Dominik, and Chris.

