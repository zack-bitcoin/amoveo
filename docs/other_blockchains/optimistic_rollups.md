Optimistic Rollups Review
===========

Optimistic rollups are a tool for efficiently making large quantities of data available to a blockchain consensus mechanism. Here is the paper introducing them: https://arxiv.org/pdf/1904.06441.pdf

Making data available at such a high rate is an impressive achievement. This is some of the coolest research happening in blockchain right now, so I had to write a review.

Here is a blog post that explains the plan to combine optimistic rollups with an erasure encoded database to allow for quadratic scaling https://ethresear.ch/t/on-chain-non-interactive-data-availability-proofs/5715

The goal of these efforts is to try and enable scalable stablecoin payments.

Why availability without sharding can not scale.
===========

If we increase the amount of available data we can store in the consensus space, but still try to maintain only one single merkel tree of everyone's balances, we would run into scaling limitations.
It just isn't possible to insert elements into a single merkel tree and re-calculate the merkel roots above a certain rate. This computation has some aspects that are not parallelizeable.
So no matter how many computers we add to the network, there will still be an upper limit on how quickly we can update account balances in any single merkel tree.

So to overcome this limit, we would need to maintain multiple databases of account balances, called shards. And we need some slower mechanism to move value from one shard into another.

Why shards + fraud proofs cannot scale quadratically
==========

In order to know that a payment you received is valid, you need to confirm that the shard update was valid.
A shard update is only valid if no contradictory update to that shard had come before.

We find out about contradictory updates by incentivizing untrusted third parties to produce fraud proofs.

So, lets say the blockchain has T txs during a period of time, and they are spread out among many shards.
There are many untrusted third parties searching for contradictory shard updates so they can profit from fraud proofs.

For every tx, there will be a user requesting a fraud proof.

In order to achieve quadratic scalability, we would need it to be the case that each untrusted third party only downloads sqrt(T) many txs.
That means they can only download sqrt(T) many requests from users for fraud proofs, and they can only download sqrt(T) many txs from the available history.

So, if there exists and incidence of fraud, the odds that one particular untrusted third party would find it is the odds that they downloaded that request from the correct user, and that they downloaded that part of the available consensus state O((1/sqrt(T)) * (1/sqrt(T))) = O(1/T).

So this means the total number of untrusted third parties scanning for fraud proofs, it would need to be proportional to the total number of txs.
And since each untrusted third party needs to download sqrt(T) much data, this means the cost of making a tx actually O(sqrt(T)).
So optimistic rollup has worse than linear scalability.

A successful scaling solution would have this property: If you increase the number of txs per second being processed, the fee per tx decreases.

With bitcoin, you can double the scalability by doubling the size of the blocks, the fee per tx would remain constant. 

But with optimistic rollup, if you want to double the txs per second, the fee per tx would increase. So optimistic rollup is actually less scalable than bitcoin.

