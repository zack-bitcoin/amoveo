Consensus Efficiency
========
draft 2

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

V = the number of txs in a single scaling lottery

The on-chain cost to settle one lottery is O(sqrt(V)), so `F ~ sqrt(V)*(lotteries per day)`

`T ~ V * L`

`V ~ T / L`

`F ~ L * (sqrt(T/L))`

-> `F ~ sqrt(T*L)`


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

`1 ~ D * T`

Putting it together to define consensus efficiency.
=========

`F ~ Finality * S * sqrt(T * L * D)`

Now we know how to define the consensus efficiency.

We want to define the consensus efficiency=C so that C is always positive, and it is set up so that if everything else is equal, higher values of C mean the tx fees are lower.

-> `F = (1/C)*(S*sqrt(T*L*D))*Finality`

-> `C = S*sqrt(T*L*D)*Finality/F` <---------

unit analysis. `C = (unitless ratio)*sqrt((txs/time)*(lotteries/time)*coins)*time/coins`
`C = sqrt(txs * lotteries)`


This is the unified blockchain modeling equation, it is also the definition of consensus efficiency.

Example of using the consensus efficiency formula to calculate how changing the cost of attacking the blockchain will impact the cost of tx fees.
==========

Lets say we start with a blockchain with this configuration:

S = 2

P = 1

F = $100

L = lotteries per day = 100

T = txs per second = 2000

So this means that an attacker who spends $100 can destroy $50.

Lets increase the security level so that an attacker who spends $100 can destroy only $5, and calculate how this impacts the cost of fees.

`C = (S*sqrt(T*L*D))/(1*F)`

-> `C = 2 * sqrt(2000 * 100 * 1)/(1*100)`

-> `C = 8.94`

so this blockchain has a consensus efficiency of 8.94.

Now lets increase the security level to 20, and calculate the new fee.

`C = (S * L * log2((P * T)/L))/F`

-> `8.94 = (20 * 100 * log2((1 * 2000)/100))/F`

-> `F = 1000`

So this shows that increasing the security level by a factor of 10 would require increasing the fees and block rewards per second from $100 to $1000. 
Meaning it would be charging 10x more fees per second and having 10x more inflation per second.
This agrees with the definition of security level in the trust theory document, so it is a confirmation that the consensus efficiency formula is correct.

