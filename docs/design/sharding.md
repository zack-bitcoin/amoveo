## Sharding in Amoveo

Sharding is a strategy to make blockchains more scalable. Sharding would reduce the total memory and computational requirements of each node. By increasing the number of shards, we should be able to increase the computational capabilitys of the blockchain without limit.

Strategies for sharding today are becoming increasingly complex. Over time as more vulnerabilities are found, the solution is always to increase complexity. This is evidence that blockchain designs today are suffering a fundamental design flaw that resists sharding.

In software engineering, the way to shard large programs across multiple computers is to make sure that the pieces that go onto different computers don't share any state. A stateful smart contract system like Ethereum is fundamentally contradictory with the goal of sharding.

Amoveo is a stateless smart contract system. Since the blockchain isn't storing any state from the smart contracts, sharding transforms from an interesting research problem into a trivial coding puzzle, the solution of which is explained below:

You pay a server to have a channel with you. The channel is what enables smart contracts on Amoveo. You are also paying that server to maintain a proof of your account state, and the channel's state. These proofs are needed in order to publish transactions.

The server requires your pubkey to be in a particular branch of the accounts merkle tree. This server sets up the channel to be in a particular branch of the channels merkle tree. This way, the server only has to maintain a small fraction of the merkle trees to maintain proofs of the state of all of it's customers.

If the customer is willing to spend N fold more time generating their public key, then the server only has to store 1/N of the merkle trees. [According to the bitcoin wiki](https://en.bitcoin.it/wiki/Vanitygen) high end computers without GPUs can generate about 1 million addresses per second, so it would be reasonable for the server so only have to maintain one in one million of the accounts in the accounts tree, and one in one million of the channels in the channels tree.

When you want to publish a tx, the server has the necessary proofs to give the miner so that the tx can be included in a block soon. Miners don't have to maintain any of the state tree, because full nodes share transactions and the merkle proofs needed to verify them at the same time.

Amoveo shards have a cool advantage: Each node can decide for itself exactly which part of the state it wants to keep track of. You aren't restricted to tracking any particular subset.
So Amoveo could have some shards keeping track of only a single account, and other shards that keep track of more than half of the consensus state.