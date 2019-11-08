Blockchain Sharding
============
draft: 3

In the Bitcoin blockchain, no matter how many full nodes you add to the system, you can not increase the rate at which transactions can be processed.

The idea of blockchain sharding is that different full nodes should specialize in different parts of the consensus process.
That way if we add more computers to the blockchain, the number of transactions we can process per second will increase.

The goal of this document is to
* describe the different resources that can cause bottlenecks in a blockchain
* describe some different plans for how to add sharding to blockchains.
* calculate how each of the different plans will change our resource requirements.


Blockchain Resources, and how they limit the bandwidth of transactions in bitcoin
============

* cpu power. #txs/second = O(CPU / #full_nodes)

* bandwidth. #txs/second = O(Bandwidth / #full_nodes)

* memory space. #txs = O(Memory / #full_nodes) -> #txs/second = O(Memory * (memory getting cheaper by moorse law) / #full_nodes)

* hard drive memory access. #txs = O(hard_drive_speed / #full_nodes)

* Coin Hours. If you need to lock up coins for a period of time. #txs = O(0)

For most blockchains today hard-drive-memory-access is the bottleneck of how fast they can proess transactions.

Amoveo is an exception. Our bottleneck is either CPU or bandwidth, depending on your hardware.

Sharding Plans
============

* stateless full nodes. With stateless full nodes, ever block contains all of the merkel proofs you need to verify that block. So the process of verifying blocks never involves reading from the hard drive. But this means blocks need to be around 10x bigger. This is a strategy invented for Amoveo, it is how Amoveo worked since the genesis block. Ethereum is considering adding this feature https://ethresear.ch/t/the-stateless-client-concept/172

* optimistic roll-up. https://arxiv.org/pdf/1904.06441.pdf  With optimistic roll-up we keep the history on-chain, but we move all processing of editable state onto side-chains. Miners pay a safety deposit when they publish a block. If anyone can show that a block improperly processed a transaction, they can destroy half the safety deposit and win the rest as a reward. ZK roll-up is very similar to optimistic roll-up. They estimate it is about 4x more efficient than optimistic roll-up https://medium.com/matter-labs/optimistic-vs-zk-rollup-deep-dive-ea141e71e075 . Since they are so similar, I grade them both together just one. In this doc, they estimate that zk roll-up will be 10 to 20-fold more scalable than standard ethereum https://medium.com/coinmonks/zk-rollup-optimistic-rollup-70c01295231b

* sortition chains https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/sortition_chains.md With sortition chains we use probabilistic value instead of full value currency. If a sortition chain has $10k in it, and you own $100 of that sortition chain, this means you have a 1% chance to win $10k. probabilistic value currency is like owning a lottery ticket.


Scoreboard
=============


These scores are based on the cost of running one full node, not the cost of the entire network. 10 is a perfect score. 0 is the worst score.

I added Bitcoin to the score board so you can compare the default setup vs the sharding plans.

```
The meaning of various scores:
0 - O(N ^ #txs)
1 - O(#txs ^ N)
2 - O(N*#txs)
3 - O(#txs)
4 - O(#txs/N)
6 - O(sqrt(#txs))
7 - O(log(#txs))
9 - O(1)
10 - O(0)


The plans we compare:
B = Bitcoin,
S = Stateless full nodes,
O = optimistic rollup,
O2 = stateless + optimistic rollup,
Sort = Sortition chains,
S2 = stateless + sortition chains

Resources:
CPU = CPU
NB = Network Bandwidth
MS = Memory Size
MB = Memory Bandwidth
CH = Coins*Hours. how much stake is locked up.

        CPU  NB  MS  MB  CH
B       3    3   4   3   10
S       2    2   10  10  10
O       3    3   4   10  9
O2      3    2   10  10  9
Sort    7    7   7   7   9
S2      7    7   10  10  9

```

