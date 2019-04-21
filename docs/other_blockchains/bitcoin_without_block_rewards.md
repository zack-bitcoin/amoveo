Bitcoin Without Block Rewards
========


The problem was that if bitcoin's block reward got lower than the tx fees, it could cause conensus to break.
http://randomwalker.info/publications/mining_CCS.pdf

The solution is that tx fees should all go to a reward-pool. And only 1/1024th of the pool should be paid out per block to the miner who found that block.


If we apply the solution in excess, it can cause a different problem. It reduces the incentive for miners to include any txs at all.
Since mining pools are so big, they are likely to find many blocks in a 1024-block period.
Which somewhat reduces the effect.
The biggest mining pools have the biggest incentive to include txs if we do it this way.
You can balances out these 2 potential issues by selecting the correct portion of the reward-pool to give out per block.

If you give out too much of the fund per block, then you have problems where cartels will re-mine history to get bigger rewards.
If you give out too little of the fund per block, then many blocks will be completely empty of txs.

There is some optimal balance between the extremes.

Currently we are 100% at one extreme, which is not the optimal value.