Blockchain Theory
========

Models have been proposed to explain relationships of different aspects of the blockchain.

We have a model that show the relationship between security, trust, and the cost of fees https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md

We have models to show a relationship between scalability and the costs of fees https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/sharding.md

We have a model showing a relationship between the privacy of transactions and scalability https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/privacy.md

The goal of this paper is to derive a unified equation that simultaniously expresses the relationships between all these different values.

The blockchain's power level
========

Using the unified blockchain equation, we can calculate a power level for any blockchain design.
If a blockchain's power level is higher, then if everything else is equal, it will have lower fees.

symbols
========

S = blockchain security level

P = Privacy = the size of the anonymity set disguising your transaction. This is the same as 1/(the linkability of your tx).

F = (tx fees paid per second) + (block rewards paid per second).

L = number of lotteries per day.

T = volume of txs per second

B = the blockchain power level.

What we know from trust theory
===========

you calculate your security level like this: If the attacker's change in balance due to the attack is A, and the defender's loss is D, then the security level is A/D .

`S = A/D`

I am using this symbol `~` to express proportionality.

In general, doubling the volume of fees collected per second increases S by a factor of 2.

-> `S ~ F`

What we know from studying sharding
==========

V = the number of txs in a single scaling lottery

The on-chain cost to settle one lottery is O(log2(V)), so `F ~ log2(V)*(lotteries per day)`

`T ~ V*L`

`V ~ T / L`

-> `F ~ L * (log2(T/L))`


What we know from studying privacy
=========

If you pay for twice as many txs, then you can increase the size of the anonymity set twice as much.

`P ~ T`

from sharding, we know that T and F relate like this

`F ~ L * log2(T/L)`

-> `F ~ L * log2((P * T)/L)`

Putting it together to define blockchain power.
=========

F ~ S * L * log2((P * T)/L)

Now we know how to define the blockchain power.

We want to define the blockchain power=B so that B is always positive, and it is set up so that if everything else is equal, higher values of B mean the tx fees are lower.

-> F = (1/B) * L * S * log2((P * T)/L)

-> B = (S * L * log2((P * T)/L))/F

This is the unified blockchain modeling equation, it is also the definition of blockchain power.

Example of using blockchain power formula
==============

Lets say we have a blockchain without any privacy, and we want to know how much higher the transaction fees will be if we add privacy by increasing the size of the anonymity set for all txs to 1000.

The blockchain starts out with this configuration:

S = security level = 2

P = size of the anonymity set = 1

F = $100

L = lotteries per day = 100

T = txs per second = 2000

So, lets calculate this blockchain's power level.

`B = blockchain power level = (S * L * log2((P * T)/L))/F`

-> `B = 2 * 100 * log2((1 * 2000)/100) / 100`

-> `B = 8.64`

it has a power level of 8.64

So lets change P to 1000 which means our anonymity set has 1000 txs in it, and solve for F to see how the size of fees will change.

`B = blockchain power level = (S * L * log2((P * T)/L))/F`

-> 8.64 = 2 * 100 * log2((1000 * 2000)/100) / F

-> F = $200 * log2(20000) / 8.64

-> F = $331

So for this example, increasing the anonymity set from a size of 1 to a size of 1000 causes the (fees + block rewards) collected per second to increase from $100 to $331.

It will cause the cost of making each tx and the cost due to inflation to increase by a factor of about 3.31.



