Sortition Chains
=========

This is a scaling plan, similar to sharding or side-chains.

Related documents:

[sortition chain attacks analyzed](./sortition_chains_defense.md)

[sortition chain implementation details.](./sortitoin_chains_implementation.md)

[sortition chain theory.](./sortition_chains_theory.md)


What is a Sortition Chain?
=========

A sortition contract is a kind of smart contract.
If you are participating in a sortition contract, then either you will get all the money in the contract, or none of it.
It is always possible to divide a sortition contract into two lower valued contracts, who would both win value in mutually exclusive situations. And it is always the case that expected_value(big) = expected_value(little_1) + expected_value(little_2)

Example:
You lock $10 into a sortition contract to bet at 50:50 odds on the outcome of a football game. At the end, you have $20 in the contract.
But the total value of the sortition chain is $1000. Since you have $20, what that means is that you have a 2% chance of winning the entire sortition chain of $1000.
Typically, after the end of the football game you would sell your stake in the sortition contract for $20, instead of holding such a high-risk asset.

A sortition contract can only exist as a part of a sortition chain.

A person usually doesn't want to hold a contract that only has a 2% chance of having value. That is a lot of risk. But as long as there are other people willing to buy the contract at a good price, this works.

Updating an existing sortition contract between 2 people can happen instantly, and can be a part of a lightning payment.


Sortition Chain Operators
======

The sortition chain operator keeps track of a merkel tree containing all the active sortition contracts. The merkel root of this is published in every block.

Every branch of the merkel tree has bits of chalang code, which specify the mutually exclusive conditions which could allow each sub-branch to be active.
The chalang code can reference randomness, or the output of an oracle, or anything.
That way, a merkel proof of the existence of your sortition contract is also a proof that no one else has the same part of the probability space as you.

So when you create a sortition contract with a sortition chain operator, the operator is giving you a contract for rights over a certain part of the probability space, and he gives you a proof that this portion of the probability space is owned by him.

Sortition Contracts are Sortition Chains
===========

We sometimes call them "contracts" and sometimes call them "chains" to hint at how it is being used in the current context. If you are using it to make a bet, we call it a contract. If you are using it to run a market where other people can bet, we call it a chain.

If we are talking about the children of a sortition chain, we will often defualt to calling them "contracts".

When a sortition chain settles, and someone's sortition contract wins the lottery, if that account is different from the account that originally created the sortition chain, then they should get paid with the creation of a new sortition chain on-chain.

What this means is that any sortition contract owned by someone besides the sortition chain operator, that sortition contract is a sortition chain, and the owner is the sortition chain operator of that chain.

But before you start selling contracts in your sortition chain, you will need to set up a server with a database for storing all the sortition contracts, and to automatically post the merkel roots to either the blockchain, or to one of your ancestor's sortition chains.

