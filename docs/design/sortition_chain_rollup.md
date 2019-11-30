Sortition Chain Rollup
==========

[sortition chains home](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/sortition_chains.md)

With optimistic rollup, everyone could cheaply have data availability guarantees for all of the sortition chains they are involved in, which allows for much better consensus mechanisms for the sortition chain operators.

with lotteries, each person needs to run the computation to see if their ownership proof is valid.
No one cares if other people's lottery tickets are valid, until they want to receive them as a payment.

How it works
=========

An operator team could gather all the changes for one update into a single sortition-block, and include all the merkel proofs needed to verify that sortition-block.
So anyone can cheaply follow along with this team of operators, by running a sortition full node to verify if this team of operators has every done an availability attack on any part of the data.

The operators aren't ever running computation in this blocks, they just commit data from the users. So a sortition full node is just downloading blocks and verifying that merkel-tree relationships in the block are valid. It is using the optimistic roll-up strategy. This makes running a sortition full node very cheap.

Sortition chains are embedded inside each other in a tree structure, so a typical account might involve proving parts of the history of 3 or more sortition chains, but each of those 3 sortition chains will be very small.
If each sortition chain has 1000 things in it, then 3 layers deep can have 1 trillion accounts.

So even if we use the optimistic roll-up strategy, the amount of data required to verify availability for the sortition chains that you care about can be very cheap.

Computation only needs to be done by people who want to personally own a part of the probabilistic value state. They want to verify that all the previous owners who gave up ownership, that the turing complete contracts connected to giving up ownership have valid evidence to make them return "true".
They could potentially need to provide that evidence, if one of the previous owners pretended that they still owned ths part of the probabilistic value space.


But I thought rollup didn't work?
==========

[I previously wrote about how rollup can't work as a scaling mechanism](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/optimistic_rollups_sidechain_attack.md)

But in this case, we aren't using it for scalability, we are using it for availability. Optimistic rollup is a great solution for availability.

each sortition chain could be limited to 1000 accounts/child-sortition-chains, and it is still a great scalability solution. O(log1000(X)) is basically the same as O(1).

So if we used optimistic rollup for sortition chains, we wouldn't need shards. So the issues described in my paper are not a problem in the case of sortition rollup.

