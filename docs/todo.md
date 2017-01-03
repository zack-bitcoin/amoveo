We need to make sure to never re-use the same entropy-cid combo for 2 different channels. The old channel state could get re-used.

channel_feeder new_channel needs to use the result of a distributed entropy generator for the entropy of the new channel. We need to make sure we never re-use the same entropy-cid

channel_manager should store by {CID, Entropy}, instead of just CID.
If there is a fork, they might use the same CID for a different channel on each side of the fork. It is best if each node stays aware of both sides, until we find out which side will win.

channel_solo_close and channel_slash need to be updated because spk:code is a list of bets, not a single bet.
Also, we need to care about spk:amount, which is the base movement of money before we run any contracts.


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