Sortition Chains Random Number Generator
============

The requirements for a RNG for a sortition chain are much stricter than most other blockcain RNG applications.
If an attacker has even 1% ability to predict or influence the bits produced by the RNG, this can seriously impact the cost of using sortition chains.

In the context of blockchains, when we are modeling some random entropy, there are 2 main factors that are important to us about the entropy:
1) how many bits of entropy is it?
2) how much does it cost to gain influence or predictive ability of the bits?

We start by using the hash of a block to give us a little randomness. The cost to manipulate these bits is at least as expensive as re-mining the block that they came from. Two strategies have been invented so that we can gather entropy from a sequence of blocks, and the cost to manipulate the entropy is as expensive as re-mining all those blocks. https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/harmonic_rng.md and https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/uncertainty_rng.md

Once we have a little randomness, we can start distributing the winnings from the sortition chain's outcome. In the first round, we distribute 1 block reward worth of value from the sortition chain. The user who won this reward, they have the freedom to either collect it, or leave it uncollected.
If they choose to not collect their prize, this is as expensive as the prize.
So the winner of one round is providing 1 bit of entropy when they make this choice, and there is a measurable cost for them to control it.
This extra entropy allows us to make the next round's reward worth 2 block rewards.
The 3rd round pays out 4 block rewards from the sortition chain.
the 4th pays out 8 rewards.

So in total, if the sortition chain had N rewards in it, it takes log2(N) rounds to fully distribute the winnings to the winners.



We model an attacker like this: the attacker wants to control >50% of the value in the sortition chain, that way it would become profitable for them to occasionally not collect a reward, to increase their chances of winning later bigger rewards.

To prevent this attack the sortition chain needs the ability to be divided into 2 smaller sortition chains.
So, if there is some attacker who owns 70% and is going to rob the owners of the remaining 30%, someone can buy up those remaining 30%, and then pay a fee to split their 30% off into it's own sortition chain that gets settled seperatly from the attacker's chain.
So the attacker ends up owning 100% of the value on their sortition chain, and there is no one left to attack.
