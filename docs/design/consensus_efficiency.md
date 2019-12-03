Consensus Efficiency
========
draft 3

Models have been proposed to explain relationships of different aspects of the blockchain.

We have a model that show the relationship between security, trust, and the cost of fees. [Trust Theory](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md)

We have models to show a relationship between scalability and the costs of fees [Sharding strategies compared](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/sharding.md)

We have a model showing a relationship between the privacy of transactions and scalability [modeling privacy](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/privacy.md)

We have a model showing a relationship between decentralization and scalability http://www.truthcoin.info/blog/measuring-decentralization/

We have a model showing a relationship between finality and cost

The goal of this paper is to derive a unified equation that simultaniously expresses the relationships between all these different values.

The blockchain's consensus efficiency
========

Using the unified blockchain equation, we can calculate the consensus efficiency for any blockchain design.
If a blockchain's consensus efficiency is higher, then if everything else is equal, it will have lower fees.

defining symbols for the equations
========

S = blockchain security level = (cost to attack)/(how much the victim loses)

P = Privacy = the size of the anonymity set disguising your transaction. This is the same as 1/(the linkability of your tx).

F = (tx fees paid per second) + (block rewards paid per second).

L = number of lotteries per day.

T = volume of txs per second we can support

C = the blockchain's consensus efficiency.

F / T = tx fee for an average tx.

Finality = how much time you need to wait until you can be sure a tx wont get undone.

D = decentralization

What we know from trust theory
===========

S = blockchain security level = (cost to attack)/(how much the victim loses)

In general, doubling the volume of fees collected per second and block rewards paid per second increases S by a factor of 2.

-> `S ~ F`

I am using `~` to express [proportionality](https://en.wikipedia.org/wiki/Proportionality_(mathematics))

What we know from studying sharding
==========


The on-chain cost to settle lotteries is O(log(T))

so `F ~ log(T)`

<!-----

`T ~ V * L`

`V ~ T / L`

`F ~ L * (log(T/L))`

-> `F ~ sqrt(T*L)`
------>


What we know from studying privacy
=========

The cost of a given level of privacy is proportional to the cost of any tx fee.

So, the blockchain which can operate with lower fees has cheaper privacy.

What we know about Finality
=========

Finality increases linearly with the cost of re-mining the blocks that get produced.

So that means if we find a way to keep everything else the same, and charge 1/2 as much fees. That means we are able to keep everything else the same, and have 2 as much finality.

`Finality ~ F`

What we know about decentralization
=========

Decentralization is a measure of the cost of a minimum full node that is able to prove ownership of your part of the currency and keep the network alive.

`D ~ T`

<!-------
`1 ~ D * T`
------->

Putting it together to define consensus efficiency.
=========

`F ~ Finality * S * log(T * D)`

Now we know how to define the consensus efficiency.

We want to define the consensus efficiency=C so that C is always positive, and it is set up so that if everything else is equal, higher values of C mean the tx fees are lower.

-> `F = (1/C)*(S*log(T*D))*Finality`

-> `C = S*log(T*D)*Finality/F` <---------

This is the unified blockchain modeling equation, it is also the definition of consensus efficiency.

Example of using the consensus efficiency formula to calculate how changing the cost of attacking the blockchain will impact the cost of tx fees.
==========

Lets say we start with a blockchain with this configuration:

S = 2

P = 1

F = $100

T = txs per second = 2000

So this means that an attacker who spends $100 can destroy $50.

Lets increase the security level so that an attacker who spends $100 can destroy only $5, and calculate how this impacts the cost of fees.

`C = (S*log(T*D))/(F)`

-> `C = 2 * log(2000 * 1)/(100)`

-> `C = 0.219`

so this blockchain has a consensus efficiency of 0.219

Now lets increase the security level from 2 to 20, and calculate the new fee.

`C = (S * log2(T*D))/F`

-> `0.219 = (20 * log2((1 * 2000)))/F`

-> `F = 1000`

So this shows that increasing the security level by a factor of 10 would require increasing the fees and block rewards per second from $100 to $1000. 
Meaning it would be charging 10x more fees per second and having 10x more inflation per second.
This agrees with the definition of security level in the trust theory document, so it is a confirmation that the consensus efficiency formula is correct.



Limitations in consensus efficiency
==================

Making a transaction for a blockchain will always involve the computational cost of signing that tx, and the bandwidth cost of sending the signed tx to someone else.
So that means there is a minimum computational and bandwith cost that is linearly related to the number of txs.

It is possible to invent protocols where the overhead of consensus is sub-linear, but eventually the cost of signing/publishing txs will dominate, and so further improvements to the cost of overhead stop being useful.