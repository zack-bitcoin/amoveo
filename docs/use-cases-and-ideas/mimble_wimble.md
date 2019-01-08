I was thinking that we could use mimble wimble, but without the privacy parts. So each tx will both include the quantity of veo being spent, and the elliptic-curve-encoded version of that same quantity.

What is nice about mimble wimble is that we can prune a lot of data from our history, so it is faster to sync, and that txs are a lot smaller, because we don't have to store signatures or addresses.

Even without hiding quantities, mimble wimble is still increasing privacy by merging all the txs into one. Your privacy is exposed when the 0th confirmation tx is sitting in a mempool, but as soon as it is in a block, then there is no record in the full nodes of which input matches which output. The miner is the only person who you have to expose the tx to.
People have payed for mixing services that are worse than this.

The complicated parts of mimble wimble are the range proofs, these parts are still in research and might change. They are the parts that would make mimble wimble expensive. It seems to me that 90% of the effort in building something like Grin is in the range proofs.
We don't need to use those parts of mimble wimble.

By just focusing on the beautiful and understood parts of Mimble Wimble, I think we can extract a lot of value and not waste much time.

Another benefit of mimble wimble's strategy is that light node operation is far simpler than light node operation in Amoveo. The user never has to deal with fraud proofs or game theory, and the light node is as secure as a full node.