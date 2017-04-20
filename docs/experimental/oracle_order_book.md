LMSR order book which is suitable for blockchains.


Every full node does not need to remember all the open orders.
The miners can choose to remember some of the open orders to collect additional transaction fees.

The miners who choose to do this need to be able to recall the open orders they choose to keep track of, in sorted order, from each side of forks that may occur, from any block in history that has a reasonable chance of being built upon.

The trick is that each block can only have one price, and the miners can only include transactions that agreed to trade at that price. Then the nash equilibrium is for the miner to remember all the trades with a high enough fee, that stand a reasonable chance of being matched in the near future. The miners would include as many transactions as possible to collect as many fees as possible.

maybe accounts should have an additional nonce, and if this nonce is raised, it cancels your open orders?

One way to cancel orders is by moving all your money to a new account. But this has the unfortunate side effect that you get a new account id. Which means that users would need seperate accounts for accepting payments and for participating in the oracle.

Maybe orders should come with a timelimit, and if they aren't met by that block, then they can't be included in any block.

This technique does work with channels, because everyone who has a channel with open orders that can trade at the current price can be posted on-chain to close at that price, even if the market maker doesn't want us to.

So, one or two of the biggest, most connected miners will make channels with everyone who wants to bet in the oracle. We will enforce non-censorship in miners by making commitments to include trades in the channels.


Each miner can choose to keep track of different oracles, that way the number of channels to any individual miner can be small.

The miner should commit to including the order before knowing what the order's price is.
What if the user refuses to reveal the price?
-Then he can use a channel_solo_close transaction to get his trade matched anyway.
This is bad, if the miner doesn't know all the open orders' prices, then he can't pick the right price for the batch.

If the miner takes on risk, can he quickly get rid of his risk by the next block?
not necessarily.

The miner can refuse to do oracle bets with people who have refused to reveal in the past.

The miner can accept bets a little at a time, that way the volume of unrevealed bet at any time can be arbitrarily small.


What is the on-chain evidence that any betting occured?
Can't the miner just write whatever price they want on the block without accepting any trades at all?
How is a light client that doesn't bet at all supposed to know if the honest price was written on the block?

The miners should all make little bets in each other's oracles, and refuse to mine on top of blocks that were made by an oracle that doesn't accept trades.
So long as the majority is following this rule, you save money by following this rule.

What if the oracle manager looks at all the little bets and realizes that a customer wants to bet big in one direction, and then the oracle manager censors all trades from that person?
Maybe oracle bets should go through lightning instead of being direct, that way the oracle manager doesn't know who's trade it is.

What if the oracle manager bribes the lightning hubs to censor certain customers?
If the lightning has enough layers, then it will be hard to censor.

Also, customers could try to tick the oracle manager into thinking a bigger trade will happen than they actually want, so the manager can't infer much data from looking at trades.