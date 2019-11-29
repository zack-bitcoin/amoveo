Optimistic Rollups Fraud Proof Cost Estimate
===========

[optimistic rollup review home](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/optimistic_rollups.md)



Why availability without sharding can not scale
===========

If we increase the amount of available data we can store in the consensus space, but still try to maintain only one single merkel tree of everyone's balances, we would run into scaling limitations.
It just isn't possible to insert elements into a single merkel tree and re-calculate the merkel roots above a certain rate. This computation has some aspects that are not parallelizeable.
So no matter how many computers we add to the network, there will still be an upper limit on how quickly we can update account balances in any single merkel tree.

So to overcome this limit, we would need to maintain multiple databases of account balances, called shards. And we need some slower mechanism to move value from one shard into another.

How many fraud proofs
==========

In order to know that a payment you received is valid, you need to confirm that the shard update was valid.
A shard update is only valid if no contradictory update to that shard had come before.

We find out about contradictory updates by incentivizing provers to produce fraud proofs.

So, lets say the blockchain has T txs during a period of time, and they are spread out among many shards.
There are many provers searching for contradictory shard updates so they can profit from fraud proofs.

For every tx, there will be a user requesting a fraud proof.

How many fraud provers
==========

In order to achieve quadratic scalability, we would need it to be the case that each prover only downloads sqrt(T) many txs.
That means they can only download sqrt(T) many requests from users for fraud proofs, and they can only download sqrt(T) many txs from the available history.

if the number of shards is proportional to sqrt(T), then that means a validator focused on only one shard would only need to look at O(sqrt(T)) many txs.

We would need O(sqrt(T)) many fraud provers.

Total cost of fraud proofs
===========

for each sidechain, we can have the number of blocks in a period of time be proportional to sqrt(sqrt(T)), so each block has O(sqrt(sqrt(T))) many txs in it.

For each block the fraud prover needs to do log2(sqrt(sqrt(T))) many comparisons to see if it is possible to make fraud proofs.

so the cost is O((# provers) * (# blocks) * (comparisons per block)) = O(sqrt(T)*sqrt(sqrt(T))*log(T)) = O(log(T) * T^(3/4))

O(log(T) * T^(3/4)) is practically the same thing as O(T).

if we increase T from 1 to one billion, then the cost per tx will only decrease by a factor of about 6.


Conclusions
==========

The cost of checking for fraud proofs per tx is approximately constant.
If the total number of txs is very small, then the cost of checking for a fraud is less than the cost of generating a signature.

So, the cost of paying to check for fraud proofs is always less than the cost of generating the signature on that tx.

So the cost of fraud proofs will never become the bottleneck.

