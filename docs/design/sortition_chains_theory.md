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


Preventing invalid state transitions
===========

You only need to keep track of the part of the probability space that you own.
You need to keep a copy of the txs where anyone else who had previously owned part of your probability space.
If someone else has a proof that they own the same part of the probability space as you, it must have been created after your proof. So the proof of existence will be connected to a later height on the main chain. So your proof will take precidence, and they can't prevent you from winning the lottery.

This means it is not possible to own the same part of the probability space in the same sortition chain more than once with the same account.



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


