Lottery Random Number Generator
============

[sortition chains home](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/sortition_chains.md)

The requirements for a RNG for a lottery with a large prize are much stricter than most other blockcain RNG applications.
If an attacker has even 1% ability to predict or influence the bits produced by the RNG, this can seriously impact the cost of using the lottery or sortition chain.

Reroll attacks
==========

A reroll attack is when one of the participants involved in generating the random entropy, if they choose to not participate in some part of the process.

For example, lets say we are using the block hash at block height H as our random entropy.
That means that the mining pool that finds block H will be the first to know what randomness is being generated. If they are unhappy with the randomness that was drawn, they can choose to delete the block and never publish it.
This gives the attacker influence over the randomness being generated in proportion to how much of the hashpower they control.

Modeling Financial Randomness
==========

In the context of blockchains, when we are modeling some random entropy, there are 2 main factors that are important to us about the entropy:
1) how many bits of entropy is it?
2) how much does it cost to gain influence or predictive ability of the bits?

Getting a little financial randomness from PoW
===========

We start by using the hash of a block to give us a little randomness. The cost to manipulate these bits is at least as expensive as re-mining the block that they came from. Two strategies have been invented so that we can gather entropy from a sequence of blocks, and the cost to manipulate the entropy is as expensive as re-mining all those blocks. https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/harmonic_rng.md and https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/uncertainty_rng.md

In general, a blockchain lottery's reward needs to be smaller than the cumulative value of all the block rewards during the time period when the randomness was generated.

In the case of sortition chains, we have some tricks to overcome this limitation.

Dividing sortition chains to prevent majority stake attacks
=================

We model an attacker like this: the attacker wants to control >50% of the value in the sortition chain, that way it would become profitable for them to occasionally not collect a reward, to increase their chances of winning later bigger rewards.

To prevent this attack the sortition chain needs the ability to be divided into 2 smaller sortition chains.
So, if there is some attacker who owns 70% and is going to rob the owners of the remaining 30%, someone can buy up those remaining 30%, and then pay a fee to split their 30% off into it's own sortition chain that gets settled seperatly from the attacker's chain.
So the attacker ends up owning 100% of the value on their sortition chain, and there is no one left to attack.

The expectation is that specialists will buy up large amounts of value in each sortition chain, and then split off onto a version where they own 100%. This way they can get rid of all the risk, and earn a profit by helping people move their value into sortition chains that have expirations further in the future.

Estimating scalability during reroll attacks
==============

In order to defend from a reroll attack, we may need to divide the sortition chain into many smaller sortition chains.
Since all the small sortition chains are using the same block's hash as their source of entropy, an attacker who performed a reroll attack would necessarily be rerolling all of the tiny sortition chains at once.
If the value is split up into N many sortition chains, then the profitability of a reroll attack decreases as 1/sqrt(N).

So, at most, we would only need to divide a sortition chain into O(sqrt((total value of the sortition chain)/(value of a block reward))) many parts.

This attack is the reason that sortition chains scale as O(sqrt(rate of txs)) instead of O(log2(rate of txs)).


