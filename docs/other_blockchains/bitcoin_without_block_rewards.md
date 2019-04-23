Update April 23, 2019
========
The solution I described below is not so good. A better solution was described in September 2018 https://twitter.com/fnietom/status/1037235118136602625

Anyone-can-spend txs are a great way to share the miner reward between multiple blocks.











Bitcoin Without Block Rewards
========


The problem was that if bitcoin's block reward got lower than the tx fees, it could cause conensus to break.
http://randomwalker.info/publications/mining_CCS.pdf

The solution is that tx fees should all go to a reward-pool. And only 1/1024th of the pool should be paid out per block to the miner who found that block.



A balance between instant fees, and delayed shared fees
=============

If we apply the solution in excess, it can cause a different problem. It reduces the incentive for miners to include any txs at all.
Since mining pools are so big, they are likely to find many blocks in a 1024-block period.
Which somewhat reduces the effect.
The biggest mining pools have the biggest incentive to include txs if we do it this way.
You can balances out these 2 potential issues by selecting the correct portion of the reward-pool to give out per block.

If you give out too much of the fund per block, then you have problems where cartels will re-mine history to get bigger rewards.
If you give out too little of the fund per block, then many blocks will be completely empty of txs.

There is some optimal balance between the extremes.

Currently we are 100% at one extreme, which is not the optimal value.


Considering off-chain exploits
================

Now we need to consider what if the user pay their tx fees off-chain.
1) if they pay off-chain so only one pool can accept the fee, there is no issue.
2) if they pay off-chain so any pool can accept the fee, then we will all see what is happening, and miner will be incentivized to stop using any pool that participates.
3) if they pay off-chain so that a white-list of pools can accept the fee, it is more complicated.

The users prefer to make txs so any pool can take the fee. This way the tx is included as soon as possible.

Mining pools prefer txs so that they have exclusive ability to take the fee. This way it their block wont get re-mined by some other pool.

As the frequency of re-mining blocks increases, mining pools start having more and more of extreme preferences towards txs that only pay fees to them.

So this means the result is a mixed solution, where some txs have fees that any pool can accept, and other txs have fees that only one pool can accept. The ratio of each kind of tx changes over time so that blocks don't get re-mined too frequently, and consensus never completely breaks. If you wait enough confirmations, you can be sure that your tx will not get double-spent.

