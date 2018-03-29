I made some tests today to see how fast we can process txs.
The way the governance variables start, a block can hold about 636 create-account transactions.
Using my old-ish computer with this CPU: 2.4 GHz Intel Core 2 Duo, it takes about 11 second to process a block that is completely filled.
We can safely support block times about 10x bigger than the average time to process a block, so this means a 110-second blocktime should be secure.
We will still start with a 10 minute blocktime for a safety margin.

This means that the initial rate of txs will be slightly faster than 1 tx per second.

A full block is currently limited to about 0.8 megabytes of space.
This includes about 0.23 megabytes of txs, and 0.63 megabytes of merkle proofs so that light nodes can verify the block.

So each create-account tx takes up about 1.3 kilobytes of space, which is about 2x bigger than the average bitcoin tx.
