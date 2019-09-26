Sortition Chain Defense
=============

[sortition chains home](./sortition_chains.md)


The purpose of this document is to look at several criticisms of sortition chains, and see if they will break under various situations.


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


What if your parent sortition chain operator refuses to share merkel proofs of the non-existence of data?
=====================
Lets say there is a sortition chain A built on the main chain, and a sortition chain B inside of it.
at height N, sortition chain operator A publishes the root of their sorition chain state, and the root of all their child chains who wanted to be updated as well.

Now a user wants to buy a contract inside of sortition chain B.
So they request merkel proofs of every state update in sortition chain B to verify that the part of the sortition chain that they want to own, that it is not already owned.

The user thinks that at height N, sortition chain B may have published a state update, so they request a merkel proof for this height.

This means that sortition chain operator B needs to be able to generate a merkel proof to show that block height N did not modify the state of sortition chain B.

But sortition chain operator A was the one who generated that merkel tree. They could choose to not share any merkel proofs from it with sortition chain operator B.

Basically, this seems like a bug that allows the operator of your parent sortition chain to halt progress in your sortition chain.

If a sortition chain operator was malicious, they could only freeze progress, they can't invalidate any smart contract.
And this would destroy their reputation. No one would want to own contracts in any of their sortition chains in the future.
It is cheap and easy to verify that a sortition chain operator is hiding data.

What is unfortunate is that this creates a trade-off.
If there are more servers available where you can record the current state of your sortition chain, this means you can continue operations even if some of those nodes go off-line.
But it also increases the number of servers who have the ability to freeze progress on your sortition chain.