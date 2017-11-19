## Sharding in Amoveo

Amoveo is a stateless smart contract system. Since the blockchain isn't storing any state from the smart contracts, sharding transforms from an interesting research problem into a trivial puzzle, the solution of which is explained below:

You pay a server to have a channel with you. The channel is what enables all of the smart contracts. You are also paying that server to maintain a proof of your account state, and the channel's state. These proofs are needed in order to publish transactions.

The server requires your pubkey to be in a particular branch of the accounts merkle tree. This server sets up the channel to be in a particular branch of the channels merkle tree. This way, the server only has to maintain a small fraction of the merkle trees to maintain proofs of the state of all of it's customers.
If the customer is willing to spend N times more time generating their public key, then the server only has to store 1/N of the merkle trees. [according to the bitcoin wiki](https://en.bitcoin.it/wiki/Vanitygen) high end computers without GPUs can make about 1 million addresses per second, so it would be reasonable for the server so only have to maintain one in one million of the accounts in the accounts tree, and the channels in the channels tree.

When you want to publish a tx, the server has the necessary proofs to give the miner so that the tx can be included in a block soon.
