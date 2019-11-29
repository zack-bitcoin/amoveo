Optimistic Rollups Review
===========

Optimistic rollups are a tool for efficiently making large quantities of data available to a blockchain consensus mechanism. Here is the paper introducing them: https://arxiv.org/pdf/1904.06441.pdf

Here is a podcast where they talk about how it works. https://thebitcoinpodcast.com/hashing-it-out-67/

Making data available at such a high rate is an impressive achievement. This is some of the coolest research happening in blockchain right now, so I had to write a review.

Here is a blog post that explains the plan to combine optimistic rollups with an erasure encoded database to allow for quadratic scaling https://ethresear.ch/t/on-chain-non-interactive-data-availability-proofs/5715

The goal of these efforts is to try and enable scalable stablecoin payments.

Why availability without sharding can not scale.
===========

If we increase the amount of available data we can store in the consensus space, but still try to maintain only one single merkel tree of everyone's balances, we would run into scaling limitations.
It just isn't possible to insert elements into a single merkel tree and re-calculate the merkel roots above a certain rate. This computation has some aspects that are not parallelizeable.
So no matter how many computers we add to the network, there will still be an upper limit on how quickly we can update account balances in any single merkel tree.

So to overcome this limit, we would need to maintain multiple databases of account balances, called shards. And we need some slower mechanism to move value from one shard into another.

How many fraud proofs?
==========

In order to know that a payment you received is valid, you need to confirm that the shard update was valid.
A shard update is only valid if no contradictory update to that shard had come before.

We find out about contradictory updates by incentivizing provers to produce fraud proofs.

So, lets say the blockchain has T txs during a period of time, and they are spread out among many shards.
There are many provers searching for contradictory shard updates so they can profit from fraud proofs.

For every tx, there will be a user requesting a fraud proof.

How many fraud provers?
==========

In order to achieve quadratic scalability, we would need it to be the case that each prover only downloads sqrt(T) many txs.
That means they can only download sqrt(T) many requests from users for fraud proofs, and they can only download sqrt(T) many txs from the available history.

if the number of shards is proportional to sqrt(T), then that means a validator focused on only one shard would only need to look at O(sqrt(T)) many txs.

We would need O(sqrt(T)) many fraud provers.

The fraud prover would scan through sqrt(T) many txs in side-blocks, and for each one do log2(sqrt(T)) many comparisons to see if it is one of the fraud proofs we are looking for.

So the cost of making fraud proofs would grow as O(T*log2(sqrt(T))), which is basically linear.

So the cost per tx of making fraud proofs will stay basically constant.


