Here you can see Zilliqa describe how sharding works on their blockchain.
https://blog.zilliqa.com/https-blog-zilliqa-com-the-zilliqa-design-story-piece-by-piece-part1-d9cb32ea1e65

Zilliqa'a strategy is very dangerous, if any one of their shards has more than 2/3rds attackers, then the attackers on that shard can print money from nothing.

Amoveo's sharding strategy is very different.
For us, each block comes with all the merkel proofs you need to verify that the block is valid.
So with Amoveo, even if you delete all the consensus state and have an empty hard drive, you can still verify any block.
With Amoveo you can even verify the blocks out of order, which is something we do to parallelize verification so you can sync more quickly.

In Amoveo, if a shard tried to print money, then the block would be invalid. Everyone would reject their block.
Even if they are the only node on their shard, they still can't trick us into accepting an invalid block.

With Zilliqa, there is danger that 2/3rds of a shard could lie and print money from nothing. So Zilliqa has a complicated process to assign shards to miners, in an effort to prevent this kind of attack. In Zilliqa, you need at least 600 nodes on each shard. In Amoveo you only need 1 node per shard.

With Amoveo any miner can freely mine on whatever shard they want. If a miner lacks some consensus state, then the miner is unable to write a tx that uses that state.
But if someone else writes a tx that uses that state, and includes some merkel proofs with the tx, then any miner will accept that tx into the tx pool as valid. Even if the miner is using a shard and doesn't keep a record of the state used by that tx.

So in the long run, I expect that all Amoveo miners will be using shards that store 0% of the consensus state. That way the mining pool is fast to install, and you only need like 1 Gigabyte hard drive to run a mining pool.

There will be other nodes that specialize in storing consensus state and generating transactions for the users.
