Sortition Chain Defense
=============

[sortition chains home](./sortition_chains.md)


The purpose of this document is to look at several criticisms of sortition chains, and see if they will break under various attacks.


What if a Sortition Chain Operator Sells All Their Stake, and then Goes Offline?
===========

Looking at the example of 3 generations of sortition chains, where the middle generation has sold all their stake, and then gone off-line.

Now the grandparent generation wants to buy back all the contracts it sold to hedge it's risk before the sortition chain ends, but the middle generation is gone, so it can't update the contracts with them.

The operators of the grandchildren chains can make sortition contracts with the operator of the grandparent chain, selling all of the contract back to the grandparent, without the middle generation even needing to come online.

So the operator of the grandparent sortition chain ends up controlling many many of his great-grandchild sortition chains.

For this to work, sortition chains need to have the option of reporting the merkel root of their sortition-contracts onto any ancestor, not just the direct ancestor. That way you can still update your contract, even if the person you bought it from went off-line.


What if an attacker tries to increase the number of lotto tickets available, because they have more appatite for risk?
===========

If you stand to have a 5% expected gain every time you can buy lotto tickets, then you might be incentivized to start doing things that will increase the total number of lotto tickets available for you to purchase.

if things are efficient, then there would be many people buying lotto tickets for a small expected profit.
They would keep under-cutting each other, so there wouldn't be almost any incentive to increase the number of lotto tickets.

Purposefully increasing the number of lotto tickets is twice as capital intensive vs just buying up existing lotto tickets.
So they will keep under-cutting each other until the attack stops happening.


Proof of the non-existance of data for hashlocking
=======================

There is an attack. The attacker sets up a hashlock update of a sortition contract, but then refuses to reveal the pre-image of the hash.
This means that the sortition chain operator is left unable to prove their ownership of that part of the money. So no one will accept payments of that part of the money.

To solve this, we will enable proof-of-existence txs on-chain. You store by the commitment, and you look up the pre-image and the height at which it's existence was recorded.

The smart contract can require that the pre-image exist in the proof-of-existence tree, and that the height recorded with it was earlier than some limit.
This way the attacker can only make the money un-spendable for a short period of time.


So we might worry that an attacker would cause us to need to record so many proof-of-existence data, that we can't fit it in a block.
To avoid this problem, whoever is the bigger sortition chain operator should not be the one generating secrets. This way, the worst that could happen is that the sortition chain operator is attacking themselves.



