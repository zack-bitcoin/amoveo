## Existing light node technology

A light node is a way to participate in a blockchain that requires less resources than a full node. A light node gives almost as strong of security guarantees as a full node.

Instead of downloading the entire blockchain, light nodes only download a small header for each block. Looking at just the header, it is possible to calculate which fork of the blockchain has the most cumulative work done to it.

## How Amoveo improves

In Amoveo we plan for light nodes from day one.

# Attack 1

There is an attack where someone with > 50% of the mining power mines headers for invalid blocks. He can give himself as much money as he wants in the invalid blocks, and trick light nodes into accepting payments of invalid money.

The way to defend against this attack is to quickly communicate to the network which headers is invalid. If most of the network is light nodes, and an attacker is controlling many nodes and sending data to confuse us, we still need a way for the netowrk to realize which headers are invalid.

The theoretical minimal cost of recovering from an attack like this is (number of nodes that need to recover) * (the cost of checking if a particular block is invalid). Amoveo is near to this limit.
If I know a block is invalid, and you don't, I can send you the hash of the block and tell you to check it.

In bitcoin the cost of checking if a particular block is valid is very high. The standard way of doing this is to first download the entire history of blocks, in total over 100 gigabytes.

In Ethereum the cost can be lower than bitcoin. It is possible to download only the parts of the state tree that you need to verify a single block.
But, since the blockchain is turing complete, the amount of state you need to download to verify a single block can be very high. The attacker makes this block, so they can make the amount of state you need to download be as high as possible.

In Amoveo the amount of state a light node needs to download and verify a single block is limited to a small amount.

# Attack 2

Another attack that can happen with light nodes is a type of DDOS.
where an attacker shares a block with enough proof of work and with invalid state-proofs. The attacker can trick us into repeatedly downloading and verifying proofs, and on the last step of verification we realize that the proof is invalid.

In some cases, these 2 attacks can combine to make something more dangerous than either alone.

In Amoveo every header includes a cryptographic checksum of a minimal datastructure containing all the proofs you need to verify the block. So if the attacker modifies any of the proofs, we can immediately know that they are bad, because the checksum wont match. So we wont waste time verifying them.
We only check at most one set of state proofs per block verified.
This stops the DDOS.

