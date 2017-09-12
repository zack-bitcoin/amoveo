## Existing light node technology

A light node is a way to participate in a blockchain that requires less resources than a full node. A light node gives almost as strong of security guarantees as a full node.

Instead of downloading the entire blockchain, light nodes only download a small header for each block. Looking at just the header, it is possible to calculate which fork of the blockchain has the most cumulative work done to it.

## How Amoveo improves

In Amoveo we plan for light nodes from day one.

### Attack 1

There is an attack where someone with > 50% of the mining power mines headers for invalid blocks. He can give himself as much money as he wants in the invalid blocks, and trick light nodes into accepting payments of invalid money.

The way to defend against this attack is to quickly communicate to the network which headers is invalid. If most of the network is light nodes, and an attacker is controlling many nodes and sending data to confuse us, we still need a way for the netowrk to realize which headers are invalid.

The theoretical minimal cost of recovering from an attack like this is (number of nodes that need to recover) * (the cost of checking if a particular block is invalid).
So one way to protect from this attack is to lower the cost of checking if a particular block is valid.

In bitcoin the cost of checking if a particular block is valid is very high. The standard way of doing this is to first download the entire history of blocks, in total over 100 gigabytes.

In Ethereum the cost can be lower than bitcoin. It is possible to download only the parts of the state tree that you need to verify a single block.
But, since the blockchain is turing complete, the amount of state you need to download to verify a single block can be very high. The attacker makes this block, so they can make the amount of state you need to download as high as possible.

In Amoveo the amount of state a light node needs to download and verify a single block is limited to a small amount.

### Attack 2

Another attack that can happen with light nodes is a type of DDOS.
where an attacker shares a block with enough proof of work and with invalid state-proofs. The attacker can trick us into repeatedly downloading and verifying proofs, and on the last step of verification we realize that the proof is invalid.

In some cases, these 2 attacks can combine to make something more dangerous than either alone.

In Amoveo every header includes a cryptographic checksum of a minimal datastructure containing all the proofs you need to verify the block. So if the attacker modifies any of the proofs, we can immediately know that they are bad, because the checksum wont match. So we wont waste time verifying them.
We only check at most one set of state proofs per block verified.
This stops the DDOS.

## Mining with light nodes

With bitcoin or ethereum, light nodes can only mine empty blocks, because a light node cannot tell which txs are valid.
With Amoveo, every tx submitted to a miner can include the merkle proof to show that it is valid.
The miner can use these proofs to build the state proofs that get included with the block he is mining.
The miner can update these proofs with each block added to his chain, so he is always ready to include them in the block he is mining.

## Why can Amoveo make this light node upgrades? Can Ethereum copy our upgrades?

Ethereum is committed to having turing completeness on-chain. So the amount of consensus state you need to know about to verify a particular block can be big.
Ethereum might do a hard fork to start including a checksum of the minimal proofs into the ethereum headers, but this could make it much more expensive to verify blocks. You need to construct the proof of the consensus state necessary to verify the block in order to know it's checksum. 
Ethereum probably can't mine full blocks with light nodes, because of the commitment to turing completeness. It is too complicated to update the merkle proofs when blocks are added to the chain.
