## The nature of smart contracts

Blockchains need to scale much bigger for global adoption.

Scalability plans today tend to involve [turing complete state channels](programmable_state_channels.md)

Putting the smart contracts into the channels gives many advantages
* speed - off-chain state can be updated as quickly as you can communicate with your channel partner. This is much faster than waiting for confirmations on-chain. If your partner is a server, you can use Amoveo smart contracts instantly.
* privacy - off-chain state can stay secret. As long as neither channel participant disappears or tries to cheat, they never have to publish the contract on-chain.
* scalability- parallel off-chain computation. The channel contracts are usually only processed by the 2 participants in the channel. Every pair of channel participants can be running different channel contracts simultaneously.
* scalability- parallel on-chain transaction computation. Most transactions in a block do not depend on each other. Since there is no on-chain smart contract state, it is easy to calculate which transactions depend on each other. So we can parallelize processing of transactions within the same block on the same machine.

The majority of smart contracts will exist off-chain, inside of state channels, in the lightning network.

We used to think that sharding would solve scalability entirely, allow for scalable contracts on-chain. But, modern sharding techniques tend to be a compliment to state channels rather than a replacement. The shards can depend upon channels to achieve certain security requirements, like front-running prevention.

So, let's make the assumption that smart contracts will be in the channels, and let's use cryptoeconomics to try to predict what these contracts would look like. 
[Why we need markets.](why_markets.md)

Techniques like [hashlocking](https://en.bitcoin.it/wiki/Atomic_cross-chain_trading) are used to connect many channels together into markets.

Amoveo has passing tests for smart contracts that connect many channels together to make a big market. Amoveo has off-chain order books to enforce these markets in the channels.

Since you get a better price in a bigger market, we know that most contracts will be formatted in a simple way that will be useful to the maximum number of people, that way the markets can be as big as possible.
Traditional markets for financial contracts face these same problems. They have settled on a handful of popular contracts, like: contract for difference, options, forwards.
