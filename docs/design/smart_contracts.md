## The nature of smart contracts

Blockchains need to scale much bigger for global adoption.

Scalability plans today tend to involve turing complete state channels.

The majority of smart contracts will exist off-chain, inside of state channels, in the lightning network.

We used to think that sharding would solve scalability entirely, allow for scalable contracts on-chain. But, modern sharding techniques tend to be a compliment to state channels rather than a replacement. The shards can depend upon channels to achieve certain security requirements, like front-running prevention.

So, let's make the assumption that smart contracts will be in the channels, and let's use cryptoeconomics to try to predict what these contracts would look like.

1) Cryptography tells us it is impossible for the owners of a channel to prove to anyone else the current or past state of their channel. This means it is impossible for anyone to prove how much they paid for a smart contract.

2) Economics tells us that markets are innefficient if only pairs of people make contracts at a secret price, because it is harder to measure the market price of anything. It is better to have one big market with lots of traders, that way we can measure the price accurately and the average person gets a better deal on their contract.

(1) + (2) means that many channels will be connected together into markets. Any smart contract which isn't built as a market will be less profitable for the participants in comparison to a smart contract that is built as a market.
Even contracts to play the game blackjack could be offered as a market, that way dealers and players are matched at a fair market price.

Techniques like [hashlocking](https://en.bitcoin.it/wiki/Atomic_cross-chain_trading) are used to connect many channels together into markets.

Amoveo has passing tests for smart contracts that connect many channels together to make a big market. Amoveo has off-chain order books to enforce these markets in the channels.

Because of (2), we know that most contracts will be formatted in a simple way that will be useful to the maximum number of people, that way the markets can be as big as possible.
Traditional markets for financial contracts face these same problems. They have settled on a handful of popular contracts, like: contract for difference, options, forwards.
