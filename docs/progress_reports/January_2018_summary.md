Amoveo Progress in January 2018

* We can now prune old blocks from the merkle tree to recover space on our hard drive. This greatly reduces the hard-drive requirements of running a full node.

* the merkle tree is now updated in batches, which is faster and doesn't waste memory.

* We have tests with blocks that are completely filled with txs. Amoveo will survive even if there is a full month of full blocks. The rate of block verification doesn't slow, even after a month of full blocks.

* Updated the tx pool so that we do not touch the hard drive. All tx processing is done in ram, and if the block gets included, then we would write the changes to disk.
This makes us secure against certain types of attacks that would waste our memory.
It means we can process txs much faster, especially on computers that don't have solid state hard drives.

* fixed various security issues with the channels in the light node: channel nonce is increased if a bet is canceled to protect against zombie contracts that come back to life after we thought they were dead. We now verify signatures in a few places to make sure that the channel smart contract is valid.

* The readme was rewritten for clarity.

* javascript light node code was rewritten to be shorter and clearer. We abstracted some patterns to remove repetition.

* the encryption library was rewritten in javascript so that we will be able to make lightning payments from the browser light node.

