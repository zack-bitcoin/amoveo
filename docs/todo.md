we need a channel powered satoshi dice for gambling.

we need to re-write the channel manager stuff for the new channels.

download_blocks could be more efficient.

we should have tests to make sure we can add accounts to the trie in random order.

block_hashes should remember its data to the hard drive. That way we don't re-download all the blocks every time we reboot. This is important for DDOS protection.


maybe nodes need to advertise their own IP/port combo as a peer?


block:check2
We need to check that the time on each block is later than the median of the last finality of blocks.



each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible.