Blockchain Sharding
============
draft: 5

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


Sharding Plans
============

* stateless full nodes. With stateless full nodes, every block contains all of the merkel proofs you need to verify that block. So the process of verifying blocks never involves reading from the hard drive. This means blocks need to be around 10x bigger. This is a strategy invented for Amoveo, it is how Amoveo worked since the genesis block. Ethereum is considering adding this feature https://ethresear.ch/t/the-stateless-client-concept/172

* Lazy Ledger - https://arxiv.org/pdf/1905.09274.pdf . Lazy ledger is a tehnique where full nodes only need to download around sqrt(# transactions), and they can get a probabilistic gurantee that all the txs in that block were processed correctly. It uses erasure coding and merkel fraud proofs. Lazy Ledger depends on the existance of storage node specialists that are paid to remember the entire history of all the txs, and to serve random subsets of this information to everyone who needs it.

* state channels. like payment channels, but you can put turing complete smart contracts inside of them.

* optimistic roll-up. https://arxiv.org/pdf/1904.06441.pdf  With optimistic roll-up we keep the history on-chain, but we move all processing of editable state onto side-chains. Miners pay a safety deposit when they publish a block. If anyone can show that a block improperly processed a transaction, they can destroy half the safety deposit and win the rest as a reward.

[My review of optimistic rollup](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/optimistic_rollups.md)

[Vitalik talking about optimistic rollup](https://twitter.com/Shaughnessy119/status/1187390153662316544?s=20) it looks like this is the plan for Eth2.0

* Plasma MVP https://ethresear.ch/t/minimal-viable-plasma/426
Plasma MVP is a type of sidechain where each user needs to be aware of all the activity on their sidechain, and be ready to challenge any improper withdraw.
The manager of the sidechain can decide to terminate that sidechain and all the contracts in it at any time. So plasma MVP can only do payments, not smart contracts.
If there is a disagreement about how history occured, we can do binary search on the history to resolve this on the main chain, we can burn safety deposits so this binary search doesn't almost ever happen, but we do need to be prepared to handle the worst case on-chain.
If a sidechain was moved on-chain all at once, then everyone who was participating in that side chain would need to publish their claim for money on-chain. So it is linear in the number of users. and the number of users is linear in the number of txs.

* Plasma Cash https://ethresear.ch/t/plasma-cash-plasma-with-much-less-per-user-data-checking/1298
Plasma cash is a type of sidechain where when you deposit coins into the sidechain, they are in a lump sum that can not be merged with other coins in that sidechain.
You need to keep track of the entire history for all the lump sums that you own.
The cost of closing a sidechain is linear in the number of lumps.
The number of lumps is linear against the number of users.
The number of txs per user is a linear relationship.

* Plasma Flow https://github.com/snjax/plasma-cashflow-spec
Plasma Flow is a plan on how to upgrade Plasma Cash with the ability to merge and split the lumps inside the sidechain.
If the sidechain suddenly had to close on-chain, the cost would be linear with the number of lumps that existed at that time.
So it is the same efficiency as the other versions of plasma.


* sortition chains https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/sortition_chains.md With sortition chains we use probabilistic value instead of full value currency. If a sortition chain has $10k in it, and you own $100 of that sortition chain, this means you have a 1% chance to win $10k. probabilistic value currency is like owning a lottery ticket.
You only need to keep track of the part of the history related to the part of the probability space that you own. You can ignore other parts of the probability space.
On-chain we use a challenge-response protocol, so you only need to provide parts of the history where you are challenged.


Scoreboard
=============


These scores are based on the cost of running one full node, not the cost of the entire network. 10 is a perfect score. 0 is the worst score.

If a network has different kinds of nodes, then we calculate the cost to run a minimum set of nodes to keep the blockchain alive.

I added Bitcoin to the score board so you can compare the default setup vs the sharding plans.

```
The meaning of various scores:
0 - O(N ^ #txs)
1 - O(#txs ^ N)
2 - O(#txs * log(#txs))
3 - O(#txs)
4 - O(#txs/N)
6 - O(sqrt(#txs))
7 - O(log(#txs))
9 - O(1)
10 - O(0)


The plans we compare:
B = Bitcoin,
S = Stateless full nodes,
C = state channels,
CS = state channels + stateless full nodes,
O = optimistic rollup,
OL = optimistic rollup + lazyledger,
P = Plasma
PC = Plasma Cash
PF = Plasma Cashflow
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
S       3    3   10  10  10
C       4    4   4   4   10
CS      4    4   10  10  10
O       3    3   3   6   1
OL      6    6   6   6   1
P       4    4   4   4   9 
PC      4    4   4   4   9
PF      4    4   4   4   9
Sort    7    7   7   7   9
S2      7    7   10  10  9

```

