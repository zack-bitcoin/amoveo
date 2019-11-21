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

Derivation symbols
========

I am using this symbol `~` to express proportionality.

S = blockchain security level
P = Privacy = the size of the anonymity set disguising your transaction.
F = (fees per second) + (block rewards paid per second).
A = the quantity of value being sent in the transaction.
V = the number of txs in a single scaling lottery
B = the blockchain power level.

What we know from trust theory
===========

you calculate your security level like this: If the attacker's change in balance due to the attack is A, and the defender's loss is D, then the security level is A/D .

S = A/D
We are only considering designs with trust level 2, so A>D, so S>1.

In general, doubling the volume of (fees + block rewards) collected per second increases S by a factor of 2.
-> `S ~ F`

What we know from studying sharding
==========

The on-chain cost is O(log2(V)), so `F ~ log2(V)`


What we know from studying privacy
=========

If you pay for twice as many txs, then you can increase the size of the anonymity set twice as much.

`P ~ F`

Putting it together to define blockchain power.
=========

S ~ F
F ~ log2(V)
P ~ F

solving for F everywhere

F ~ S - R
F ~ log2(V)
F ~ P

-> F ~ P*log2(V)*S

Now we know how to define the blockchain power.

-> F = B*P*log2(V)*S

-> B = F/(P*S*log2(V))

Conclusions
========

For any blockchain design it is possible to calculate a power-level for that blockchain.
Blockchains with higher power levels can have lower fees, if everything else is configured identically.

The amount of fees and block rewards that a blockchain needs to charge per second is (that blockchain's power level)*(how big you want the anonymity set to be so you can have privacy for your txs)*(how expensive it is for an attacker to break)*(log2(the number of txs bundled in a single scaling lottery)).