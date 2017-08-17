The channels in Aeternity are more scalable for 2 reasons.
1) Channels move computation off-chain, so we can parallelize the smart contracts. So there is no limit to the amount of smart contract computation that can happen per second. Ethereum channels also have this capability.
2) The channels don't share state, so we can process all the channel-transactions in a block in parallel. So there is no limit to the volume of channels that can be closed and opened per second. Ethereum channels do not have this capability.

The typical transaction has 2.5 merkle proofs.
If we had 1 billion users, then the typical merkle proof has 7.5 steps.
Each step is 512 bytes.
So the typical transaction will be 10 kilobytes.

If each user needs about 1 transaction per month, and we had 10 billion users.
There are 144 blocks per day, 30 days per month.
That would mean 2 million tx per block.
So each block is at least 20 gigabytes.

6 gigabytes per 10 minutes is about as fast as an internet speed as we can reasonably expect for full nodes.

If full nodes ignore the proofs and just download blocks, then blocks would be about 1/20 as big, so it would only be 1 gigabyte per block.

Since each block has 5 million updates, we can compress the proofs.
The root of the tree only needs be proved once, which saves (5 million - 1) * 512 bytes.
5 million + (5 million - 16) + (5 million - 256) + (5 million - 4096) + (5 million - 65536) + (5 million - 1048576) = 30 million - 1.1 million = 28.9 million.
So we can save about 14.8 gigabytes by compressing the proofs.
Which means each block with proofs would be about 6 gigabytes.

It costs about $5 to download a terabyte of data. Assuming each node uploads and downloads each block once, it would cost about $10 a day to maintain a light node without the state tree.
A full node with the state tree would cost 20 times less for networking, $0.50 per day.
They would need to maintain a massive state tree.
10 billion users * 512 bytes per step * 3 proofs per user * 8.3 steps per proof = 127.5 terabytes.
it costs about $0.03 per gigabytes, so this would cost about $3825.

So if you are going to run a node continuously for more than a year, it probably makes sense to buy the hard drives to run a full node.
If you are running a node for less than a year, then it is likely better to use a light node.

So consensus costs of running a mining pool that only mines empty blocks will either (stay below $10 per day) or a (one time payment of less than $3825 and daily payments of less than $0.50).

The cost for a light node to have N confirmations of security will stay below (N * $0.07). You can pay money to get more confirmations quickly by downloading blocks.

The consensus cost to run an extra-light node will be about $0.0000001 per day, but you have to wait about 10 minutes for each confirmation of security.
