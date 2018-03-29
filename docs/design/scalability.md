The channels in Amoveo are more scalable for 2 reasons.
1) Channels move computation off-chain, so we can parallelize the smart contracts. So there is no limit to the amount of smart contract computation that can happen per second. 
2) The channels don't share state, so we can process all the channel-transactions in a block in parallel. So there is no limit to the volume of channels that can be closed and opened per second. Ethereum channels do not have this capability.

The typical transaction has 2.5 merkle proofs.
If we had 1 billion users, then the typical merkle proof has 7.5 steps.
Each step is 512 bytes.
So the typical transaction will be 10 kilobytes.

Each user needs about 1 transaction per month.
Assuming we want to scale to 10 billion users.
There are 144 blocks per day, 30 days per month.
That would mean 2 million tx per block.
So each block is at least 20 gigabytes.

6 gigabytes per 10 minutes is about as fast as an internet speed as we can reasonably expect for full nodes.

Full nodes that maintain the entire consensus state can ignore the proofs and just download blocks, so for then it is only about 130 bytes per tx, so blocks would be about 1/70 as big. So it would only be 300 megabytes per block.


It costs about $5 to download a terabyte of data. Assuming each node uploads and downloads each block once, that is 300 mb * 144 * 2 = 86 gb per day. So it should cost less than $35.00 per day to maintain a full node at this scale.

Or if you run a full node with the entire consensus state, since blocks are only 300 mb each, it only costs $0.50 per day. But you need massive hard drives to store all that consensus state. 20kb * 10 billion people = 200 terabytes
So it is a one-time payment of $10 000.

So if you are going to run your full node for more than a year, you might be able to save money by investing in lots of hard drives.

Amoveo supports sharding, so a full node can decide to only keep track of the portion of the state it cares about.

Consensus costs of running a mining pool will be as much as the cost of running a light node. The lightning nodes generate the proofs for each customer, so the miner doesn't have to keep track of any consensus state.

Miners can completely ignore the blockchain. They just do the work provided by the mining pool.

The consensus cost to run an light node will be practically free. You can turn one on by visiting a webpage.

I'm guessing we would have about 1000 full nodes to maintain the network's security. So the cost of operating Amoveo at a capacity of 10 billion people will be about $35 000 per day.
Bitcoin is giving out 12.5 * 144 * 12000= $21 million a day in block rewards, so $35k shouldn't matter.
