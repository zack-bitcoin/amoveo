Optimistic Rollups Review
===========

Optimistic rollups are a tool for efficiently making large quantities of data available to a blockchain consensus mechanism. Here is the paper introducing them: https://arxiv.org/pdf/1904.06441.pdf

Here is a podcast where they talk about how it works. https://thebitcoinpodcast.com/hashing-it-out-67/

Making data available at such a high rate is an impressive achievement. This is some of the coolest research happening in blockchain right now, so I had to write a review.

Here is a blog post that explains the plan to combine optimistic rollups with an erasure encoded database to allow for quadratic scaling https://ethresear.ch/t/on-chain-non-interactive-data-availability-proofs/5715

The goal of these efforts is to try and enable scalable stablecoin payments.

Scalability analysis
========

[estimating the cost of fraud proofs in optimistic rollup](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/optimistic_rollups_fraud_proof_cost.md) Fraud proof security costs the same amount per tx, no matter how many txs are being processed per second.
This means that there is some lower limit cost per tx, and the fee will never be below that limit.

[estimating the cost of attacking a sidechain inside optimistic rollup](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/optimistic_rollups_sidechain_attack.md)
The amount of stake that needs to be locked up by sidechain validators is proportional to (rate of tx production)^(3/2).
This means that the lower limit fee cost keeps getting more expensive as more people join the network.

So this is not a scalability solution, we need to keep looking to find a scalability solution.

