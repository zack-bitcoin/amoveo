Blockchain Theory
========

Models have been proposed to explain relationships of different aspects of the blockchain.

We have a model that show the relationship between security, trust, and the cost of fees. [Trust Theory](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md)

We have models to show a relationship between scalability and the costs of fees [Sharding strategies compared](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/sharding.md)

We have a model showing a relationship between the privacy of transactions and scalability [modeling privacy](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/privacy.md)

The goal of this paper is to derive a unified equation that simultaniously expresses the relationships between all these different values.

The blockchain's consensus efficiency
========

Using the unified blockchain equation, we can calculate the consensus efficiency for any blockchain design.
If a blockchain's consensus efficiency is higher, then if everything else is equal, it will have lower fees.

defining symbols for the equations
========

S = blockchain security level = ((cost to attack))/(how much the victim loses)

P = Privacy = the size of the anonymity set disguising your transaction. This is the same as 1/(the linkability of your tx).

F = (tx fees paid per second) + (block rewards paid per second).

L = number of lotteries per day.

T = volume of txs per second

B = the blockchain's consensus efficiency.

What we know from trust theory
===========

you calculate your security level like this: If the attacker's change in balance due to the attack is A, and the defender's loss is D, then the security level is A/D .

`S = A/D`

In general, doubling the volume of fees collected per second increases S by a factor of 2.

-> `S ~ F`

I am using this symbol `~` to express [proportionality](https://en.wikipedia.org/wiki/Proportionality_(mathematics))

What we know from studying sharding
==========

V = the number of txs in a single scaling lottery

The on-chain cost to settle one lottery is O(log2(V)), so `F ~ log2(V)*(lotteries per day)`

`T ~ V * L`

`V ~ T / L`

-> `F ~ L * (log2(T/L))`


What we know from studying privacy
=========

If you pay for twice as many txs, then you can increase the size of the anonymity set twice as much.

`P * T ~ 1`

from sharding, we know that T and F relate like this

`F ~ L * log2(T/L)`

-> `F ~ L * log2((P * T)/L)`

Putting it together to define consensus efficiency.
=========

`F ~ S * L * log2((P * T)/L)`

Now we know how to define the consensus efficiency.

We want to define the consensus efficiency=B so that B is always positive, and it is set up so that if everything else is equal, higher values of B mean the tx fees are lower.

-> `F = (1/B) * L * S * log2((P * T)/L)`

-> `B = (S * L * log2((P * T)/L))/F`

This is the unified blockchain modeling equation, it is also the definition of consensus efficiency.

Example of using the consensus efficiency formula to calculate how changing the level of privacy will impact the cost of tx fees.
==============

Lets say we have a blockchain without any privacy, and we want to know how much higher the transaction fees will be if we add privacy by increasing the size of the anonymity set for all txs to 1000.

The blockchain starts out with this configuration:

S = security level = 2

P = size of the anonymity set = 1

F = $100

L = lotteries per day = 100

T = txs per second = 2000

So, lets calculate this blockchain's consensus efficiency.

`B = (S * L * log2((P * T)/L))/F`

-> `B = 2 * 100 * log2((1 * 2000)/100) / 100`

-> `B = 8.64`

it has a consensus efficiency of 8.64

So lets change P to 1000 which means our anonymity set has 1000 txs in it, and solve for F to see how the size of fees will change.

`B = (S * L * log2((P * T)/L))/F`

-> `8.64 = 2 * 100 * log2((1000 * 2000)/100) / F`

-> `F = $200 * log2(20000) / 8.64`

-> `F = $331`

So for this example, increasing the anonymity set from a size of 1 to a size of 1000 causes the (fees + block rewards) collected per second to increase from $100 to $331.

It will cause the cost of making each tx and the cost due to inflation to increase by a factor of about 3.31.

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

`B = (S * L * log2((P * T)/L))/F`

-> `B = (2 * 100 * log2((1 * 2000)/100))/100`

-> `B = 8.64`

so this blockchain has a consensus efficiency of 8.64.

Now lets increase the security level to 20, and calculate the new fee.

`B = (S * L * log2((P * T)/L))/F`

-> `8.64 = (20 * 100 * log2((1 * 2000)/100))/F`

-> `F = 1000`

So this shows that increasing the security level by a factor of 10 would require increasing the fees and block rewards per second from $100 to $1000. 
Meaning it would be charging 10x more fees per second and having 10x more inflation per second.
This agrees with the definition of security level in the trust theory document, so it is a confirmation that the consensus efficiency formula is correct.

