There is a problem where if you crash while syncing with a peer, then you skip trying to sync with any peer lower on the list. this is very bad.

the entropy isn't matching. it is created in spk:new and in new:channel.


wait for a hard fork to do this upgrade: blocks should point to the previous header, not the previous block. 


block:check2 needs an update. It should only accept blocks that were made after the median of the last 100 blocks



make the api networking/handler be entirely encrypted. This is to protect information about the channels.


we need a channel powered satoshi dice for gambling.

we need to re-write the channel manager stuff for the new channels.

download_blocks could be more efficient.

we should have tests to make sure we can add accounts to the trie in random order.

block_hashes should remember its data to the hard drive. That way we don't re-download all the blocks every time we reboot. This is important for DDOS protection.


maybe nodes need to advertise their own IP/port combo as a peer?


block:check2
We need to check that the time on each block is later than the median of the last finality of blocks.



each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible.