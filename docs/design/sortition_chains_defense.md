Sortition Chain Defense
=============

[sortition chains home](./sortition_chains.md)


The purpose of this document is to look at several criticisms of sortition chains, and see if they will break under various situations.

operators steal the money attack
===========

What if the operators assign your money to them in the probabilistic value tree?

As long as they had assigned it to you first, then your claim has higher priority. So if you win the lottery, they cannot prevent you from taking the prize.

What if the operators put an element in the waivers tree saying that you gave up ownership of your part of the probabilistic value space?

It is only valid if you signed it.

Tx censorship attack
===========

What if the sortition operators refuse to allow you to move your money? Will you get stuck holding lottery risk?

When the sortitoin chain is finally being settled, before the RNG is generated to choose a winner, there is a period of time when you will have one final chance to sell your part of the soritition chain.

Frozen sidechain attack
===========

What if one of the sortition operators stops signing any updates, so no one can transfer their funds inside the sortition chain.

If the sortition chain is frozen, it is still possible to prove what your current balance is in the sortition chain. So we can have a period of time between when the sortition operators are able to make updates, and when we generate the RNG to determine the winner.
During this period of time, we can give everyone one final chance to sell their value from the sortition chain.

So during this time we should allow people to sign an agreement off-chain that says "If I win the sortition chain, I want 100% of the value to go to X".

we can use hashlocking to connect the creation of this agreement to a payment in a different sortition chain, and we can combine it with a safety deposit that you pay.

We can set it up so that if you make multiple of these agreements that contradict with each other, that you will not receive the payment, and you lose your safety deposit.
This is re-using a trick from probabilistic payment research.

Signature availability attack
=========

What if one of the operators witholds their signature. So we don't know if the current state of the sortition chain is commitment number C or C+1.
The operator can choose to reveal their signature at the last minute, or not, depending on which state is better for them.
So this means if you have a tx in commitment C+1, you can't be sure if that tx was executed or not.

So this is similar to the freeze attack, except all the money involved in txs in commitment C+1, we don't know if they are going to be included or not.

A partial solution to this is to add this rule:
The very highest commitment is only valid if there is another commitment built on top of it, and the commitment on top has at least N-1 of the validator's signatures.

So, as long as at least 1 of the validators is honest, there are only 2 possible final outcomes of the sortition chain.

Data availability attack
==========

If some of the operators sign sortition-blocks even though some of the data is not available, this is a kind of attack.

A partial solution to this is to add this rule:
The very highest commitment is only valid if there is another commitment built on top of it, and the commitment on top has at least N-1 of the validator's signatures.

So, as long as at least 1 of the validators is honest, there are only 2 possible final outcomes of the sortition chain.

Solving availability attacks
==========

As long as at least one of the N operators refuses to sign unavailable updates, and makes all the data available, then both kinds of availability attacks have this in common: there are exactly 2 possible final states for the sortition chain. And it is easy for anyone to check whether the signatures and data for the second state is available.

So for every sortition chain being settled, we can ask the oracle "Is an availability attack happening?"
If the oracle responds "false", then we use the very last state. if the oracle responds "true", then we use the 2nd to last state.

N of N availability attacks
==========

What if all N of N sortition operators cooperate to withold some data?

Operators would compete to look more honest. [here is a description of a protocol so that it is cheap to verify that all the data is available](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/sortition_chain_rollups.md)

Optimistic rollup is a great solution so that everyone who wants to use a sortition chain can cheaply verify that all the data needed is available.

As soon as any part of the data is withheld, all of the users of that sortition chain will immediately know about it, because they will not receive a valid sortition block for something that was committed on-chain.

It is cheap to prove to everyone else that this entire list of operators are signing commits without providing sortition blocks, so none should be used as sortition operators again.

This attack is only possible if 100% of the operators work together. They still can't steal anyone's value. but they can freeze your money and force you to take on lottery risk.
So the worry is the the operators will hold your money hostage, and try to buy it from you at a discount.
Depending on how many people sell, and at what level of discount, this attack could be fairly profitable.

But it only happens if all N of N operators cooperate, so as long as we can each trust someone in the list of operators, this attack isn't an issue.

Since the total number of sortition chains can be in the trillions, it is cheap to make a new sortition chain where the list of operators includes someone you trust, and someone trusted by the person you want to make a contract with.


Sortition chain Timeline
===========

~2 months: it is possible to make payments and contracts in the sortition chain.
~1 week: we ask the oracle if an availability attack happened.
~1 week: everyone gets one final chance to sell their stake.
~1 week: people can provide evidence to prove that they won the sortition chain.



