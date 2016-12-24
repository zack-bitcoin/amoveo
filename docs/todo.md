block:check2
We need to check that the time on each block is later than the median of the last finality of blocks.




working in download_blocks


Channels should have a unique identifier.
The worry is that an attacker could make a second channel with the same person using the same channel ID. In such a situation, they would be able to reuse old channel contracts from the previous channel.
Making the defender remember every channel they ever made is unacceptable.



Transactions need proofs of their data to be valid.
Sometimes a transaction isn't included in the first block after the transaction was written. So the proof needs to be updated.
We need to improve the trie library so that we will be able to update proofs on transactions.


tx_pool uncomment in init.

block_tree.erl needs to be updated. write/2 should check to make sure that the block has enough POW.

accounts and channels should be stored in a merkle trie instead of the finality/ folder.

download_blocks.erl needs to be re-written.
We should probably reconsider the api for sharing blocks.

block finality needs to be replaced. We should store blocks by their hash, not by their height. Because we want to be able to store more branches on the hard drive.


each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible.