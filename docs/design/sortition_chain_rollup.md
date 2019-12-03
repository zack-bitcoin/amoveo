Sortition Chain Rollup
==========

[sortition chains home](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/sortition_chains.md)

Optimistic rollups are a tool for efficiently making large quantities of data available to a blockchain consensus mechanism. Here is the paper introducing them: https://arxiv.org/pdf/1904.06441.pdf


How optimistic rollup works
=========

With optimistic rollup, everyone could cheaply have data availability guarantees for all of the sortition chains they are involved in.

In optimistic rollup, there is a kind of node that downloads a lot of data, and this node runs almost no computation over that data. At most they might verify that merkel trees embedded in the data are valid.

The optimistic-node doesn't verify if any individual tx that they downloaded is valid, all they do is make the txs available to whoever wants a copy of them. Since optimistic-nodes aren't doing computations with the txs, they don't need to do any random accesses of any of the data that they are making available. Storing memory is extremely cheap if we never need to do random accesses of it.

Users can do computations over subsets of the available txs to calculate the current state of their smart contracts. So with optimistic rollup the smart contract computations move from inside the full nodes, to client side.


Why optimistic rollup and sortition chains are the perfect combo
=============

Optimistic rollup allows us to move computation from the nodes to the users. So for optimistic rollup to be useful, we need to find a way to limit the maximum possible amount of computation that users could be required to do.

Sortition chains are a tool that solve exactly this problem. With sortition chains, value is stored in the form of mutually exclusive lottery tickets. What is nice about lottery tickets, is that you only need to verify that your own lottery tickets are valid.
You don't care at all about any lottery tickets besides your own.
Other lottery tickets can only win if your lottery ticket loses. If your lottery ticket loses, then you have no stake in the outcome of this sortition chain, so you don't care what happens.

Optimistic rollup is a way to move computation from the nodes to the users.
Sortition chains are a way to limit the amount of computation any one user could need to do.
If you combine them both, then you have a system with bounds on the maximum computational resources demanded by any participant.

But I thought rollup didn't work?
==========

[I previously wrote about how rollup can't work as a scaling mechanism](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/optimistic_rollups_sidechain_attack.md)

But in this case, we aren't using it for scalability, we are using it for availability. Optimistic rollup is a great solution for availability.

each sortition chain could be limited to 1000 accounts/child-sortition-chains, and it is still a great scalability solution. 

So if we used optimistic rollup for sortition chains, we would have optimistic rollup inside of shards instead of shards inside of optimistic rollup. So the issues described in my paper are not a problem in the case of sortition rollup.

