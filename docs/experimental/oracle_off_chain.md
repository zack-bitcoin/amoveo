In other documents, we discussed why the oracle will be a market, and how this market will work.

We could build this market on-chain, but that would come with serious anti-scalability costs.
We would have to remember the sorted list of open orders orders at many different block heights.

Here you can read about how the oracle can be built off-chain.

The steps are like this:
1) Someone commits to running the oracle market. They allow anyone to make channels with them and bet.
2) The miners make multiple bets in the market to cancel their risk. The miners do this anonymously. 
3) The oracle periodically publishes the prices of the different kinds of shares in the oracle. The act of publishing a price closes many open orders made in many different channels. It closes all the orders that agreed to trade at that price.
4) Once the price stays stable for a long enough period of time, the coorelation of the oracle's resolution with the difficulty is computed, and from it the blockchain learns the true fact about our world.



Attack vectors to analyse
1) What if the oracle refuses to take any bets in the market, then the oracle has no incentive to write the correct prices.
* The miners make multiple bets in the market to cancel their risk. The miners do this anonymously via onion lightning. They do enough bets to have statistical confidence about the percentage of bets that are censored.
* If too many of the miner's bets are censored (> ~95%), then the miners censor the oracle from publishing the result.

2) What if the oracle write the wrong price at the last minute, so the volume of trades is small?
* The oracle doesn't settle until the price stays in one range for a long time.

3) What if the oracle censors just enough votes so that half the miners split onto one side of a fork, and the other half are on the other side of a fork?
*There are 3 possible outcomes:
*1 >95% of bets are censored, in which case we censor the oracle, miners refuse to build on top of blocks that include oracle transactions.
*2 Less than 80% of bets are cesored, in which case we don't censor the oracle, and we have a strong preference for the side of the fork that publishes the oracle result. It doesn't matter the height at which a fork includes the oracle transactions.
*3 Between 80% and 95% are censored, in which case the miners accept whichever side of the fork is longer without preference, and they censor the oracle txs from the blocks they mine.