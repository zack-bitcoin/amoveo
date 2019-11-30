Sortition Chains Theory
===========

[sortition chains home](./sortition_chains.md)

lottery randomness is necessary for sharding
==============

properties we want for sharding

* cost of bandwidth to run a full node should at worst be as expensive as O(log(#users))
* the currency on different shards is fungible.
* the same consensus mechanism is securing all the shards at once.

The goal of this proof is to show that any blockchain with all 3 of these properties at once must be using some lottery randomness type accounts.

If we are using the same single consensus mechanism to secure all the shards, then that means there is a main chain that records the order of history of the heart of the consensus mechanism.

For the same consensus mechanism to secure all the shards, and have fungibility, that means it needs to be possible to move your money onto or through the main chain without anyone else's permission, just by giving txs to miners.

if the bandwidth is only O(log(#users)) or less, then that means the number of accounts recorded on the main chain needs to be less than O(log(#users)).

For it to be possible to move your value onto the main chain without anyone's permission, while it also being the case that only O(log(#users)) at most have their account recorded on the main chain, the only way to have both of these at once is if it is lottery-ticket type value.


Lottery randomness is necessary for sharding #2
===============

If the main chain ever needs to verify the validatity a shard-block's state transition, then an attacker could cause data from each shard to need to be validated at the same time, and over-load the system.
This means that such a sharding system would only be able to scale a constant order more than having a single blockchain.

With probabilitic-value contracts, you never need to prove the validity of a shard state transition to the main chain, so this means it is possible for us to scale with logarithmic efficiency.

Before buying part of a sortition chain, you need to download merkel proofs of the state of that part of the probability space at every block height back to the origin of the sortition chain, as well as all the txs where others had given up ownership of this part of the probability space.
Once you have all this data, then you have trust-free ownership of your part of the probability space.
You don't need to know anything about any other part of the probability space.
If other parts of the probability space were double-spent, it does not matter to you at all. You still own your part of the probability space.

Horizontal and Vertical payments
=================

Sortition chains offer 2 different ways to make payments: horizontal and vertical.
A horizontal payment is how you give someone control of part of an existing sortition chain.
A vertical payment is how you create a baby sortition chain inside of an existing sortition chain, and then you give someone control of a part of the new baby sortition chain.

The off-chain cost of either kind of payment is that the person receiving the payment needs to download merkel proofs of the history of this part of the probability space.

The on-chain cost of horizontal transfers is zero.

The on-chain cost of vertical transfers is (the probability that you win)*(length of a signed chalang contract).

When you make a baby sortition chain, you get to choose a new list of operators for that new sortition chain.



We don't need turing complete contracts to divide the probability space
============

The other day we were talking about how we can have turing complete contracts to divide up the probabilistic value space, and we can have turing complete smart contracts for when people give up control of part of the probability space.
And we realized that it is better to do it at the step where users are giving up control, because less data gets posted on chain.

Now I have realized that any contract done at the probabilistic division step, it can also be done at the step where users are giving up control.

Lets say I have a arbitrary contract at the probabilistic division step. it divides up some of the value space into 2 mutually exclusive outcomes.

I can make the same division by owning all that probabistic value space, and signing a tx that says I am giving up control of this part of the probabilistic value space, but only in the condition that the contract returns true.
And we can set it up so that if I do give up control, next in line is the person on the other side of the contract.

So this greatly simplifies the sortition chain design.


No Pubkey reuse for the same probability space.
===========

You only need to keep track of the part of the probability space that you own.
You need to keep a copy of the txs where anyone else who had previously owned part of your probability space.
If someone else has a proof that they own the same part of the probability space as you, it must have been created after your proof. So the proof of existence will be connected to a later height on the main chain. So your proof will take precidence, and they can't prevent you from winning the lottery.

This means it is not possible to own the same part of the probability space in the same sortition chain more than once with the same account.


state channels inside sortition chains
===========

Sortition chains need to run smart contracts in 2 different kinds of security assumptions.
1) We need a way to divide up the probability space. The outcomes of these contracts needs to be computable to everyone, so they can't depend on off-chain data. You need to wait for on-chain confirmations to change any part of these contracts.
2) We need a way to have smart contracts who's outcome might depend on data only known to the participants of that contract. So the outcome is only computable by the participants, up until the point where they post the evidence on-chain to enforce the correct outcome. For example, this is needed for single-price-batches. These kinds of contracts can be updated instantly, without waiting for any on-chain confirmations.

If a sortition chain is using smart contracts of type 2, then it can't have baby sortition chains. Lets call this a state-channel.
So this means that sortition chain data structures need to be able to store 2 types of elements. sortition chains, and state channels.

If both participants in a state channel give up control of that part of the probability space, then it can be converted back into a normal account, or into a baby sortition chain.

multiple random samples
============

Maybe a sortition chain would be better if we did multiple random samples instead of just 1.

So when a sortition chain closes, it could randomly pay to up to 100 accounts, each receiving 1% of the veo from that chain.

The trade-off is that the on-chain cost per sortition chain increases 100x, but the risk of holding lottery tickets reduces 10x.

We can already know that this strategy is no good, because costs are increasing by the square of the benefits.

Therefore it is always better to have 2 small sortition chains that each pay out once, instead of 1 big one that pays out twice.


Making the value less probabilistic
===============
There are ways to compromise to make the value less probabilistic.
Like if I had 10 sortition chains going at once, and everyone who make a contract in one, I did the identical contract in all 10 with them.

So now there are 10 winners instead of 1 winner.
Which significantly reduces the risk.

Each sub-sortition chain of the recursive tree can still have only 1 winner.

Liquidity in Sortition Chains
=========

A major limitation of channels is that they are terrible for lottery.
You can only win as much money as is in the channel.
Sortition chains don't have this problem.
If you lock $1000 in stake, you could make 1000 sortition contracts, each with a 0.1% chance of winning $1000.

Similarly, if you are running a market inside a sortition chain, you could sell many mutually exclusive sortition contracts using the same staked funds.

If you want to run a market like amoveobook to match trades, you need to have twice as much money locked in channels vs the amount actually at stake in the bet. Half the money is being canceled out by arbitrage.

These lockup costs mean that only very rich people can run a hub (since 1/2 the money in a market at any time is money owned by the hub).

If we go with sortition chains instead, then the market operator only needs to control something like 2% - 10% of the money in his markets (which means it costs a lot less to launch new markets for people to trade).


The Leverage of Sortition Chains
============

If a sortition chain operator keeps selling sortition contracts, eventually they will make back almost all the money that they had paid to create the soritition chain, which means they have enough money to make another sortition chain.
It is like he is getting a leveraged position.
The total value of all the sortition chains he is operating becomes much larger than the total value of the account he had started with.

So a person with only 1 veo can generate and profit from 20+ veo worth of sortition chains all containing smart contracts.

The capital cost of being a sortition chain operator is very low. So it is cheap to launch a new sortition chain and offer custom markets in whatever you care about.

"contracts == chains" UX Advantages
==========

If you are making a new sortition on top of an existing one, it isn't going to be recorded on-chain. Your address isn't even recorded on-chain anywhere. And you can run a market that matches derivatives in single price batches.

"contracts == chains" Resource Consumption Advantages
===================

By layering sortition chains inside of each other, any individual sortition chain wont have to keep track of too much data. So the memory requirement of running a sortition chain can be bounded.

By layering sortition chains, each individual sortition chain can store less value.
So if you are running many different sortition chains, you can use a different private key for each one, so if one of your servers is compromised, you don't lose everything.

Parallelizing tx processing across multiple computers running different sortition chain databases increases throughput of txs.


