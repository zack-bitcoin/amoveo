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

Dividing sortition chains to eliminate lottery risk.
=================

When a sortition chain is finally being settled, we want a simple process so that people can get their money out on-chain without having to take on much lottery risk.

The way to do this is to allow people to divide up the sortition chain into smaller sortition chains.
They can pay to have it divided up such that they own 100% of the value on one of the sortition chains, and 0% on the other, that way they can eliminate the lottery risk.

The expectation is that specialists will buy up large amounts of value in each sortition chain, and then split off onto a version where they own 100%. This way they can get rid of all the risk, and earn a profit by helping people move their value into sortition chains that have expirations further in the future.

Why dividing sortition chains up is not a scalability solution
==============

In order to defend from a reroll attack, we may need to divide the sortition chain into many smaller sortition chains.
Since all the small sortition chains are using the same block's hash as their source of entropy, an attacker who performed a reroll attack would necessarily be rerolling all of the tiny sortition chains at once.
If the value is split up into N many sortition chains, then the profitability of a reroll attack decreases as (lottery prize)/sqrt(N).

(lottery prize) = (block reward)*sqrt(# sortition chains)

(# sortition chains) = ((lottery prize) / (block reward))^2

So, at most, we would only need to divide a sortition chain into O(((total value of the sortition chain)/(value of a block reward))^(2)) many parts.


Delayed cryptography RNG
=============

A couple papers on this topic:
https://eprint.iacr.org/2015/1015.pdf
http://www.jbonneau.com/doc/BGB17-IEEESB-proof_of_delay_ethereum.pdf

[Here is the delayed crypto library I wrote for Amoveo](https://github.com/zack-bitcoin/vdf_calculate)

How about we use time-delay crypto to find out whether you won the lottery at a given block, and we use a true-bit type interface to prove the long time-delay operation on-chain.
if it takes more than 1 block of time to find out if you won, then you wont know whether to do a reroll or not until it is too late
if everyone had to run time-delay crypto on every block, that would be a lot of computer power running constantly.
oh, we only need to do this on one block per sortition chain, so it's not that bad.
I think we need to do it for a few blocks at least.
If there is a 1/10 chance of exactly 1 winner, then there is a 1/100 chance of 2 winners, and it takes on average 9 blocks to find a winner.
I guess if we had 1 winner 1/2 the time, 2 1/4 of the time, 3 1/8th of the time, that would be alright
so each person's odds of winning a particular round would be (1/2)*(portion of the stake that they own).
looks like some people embed a time-delay element into decryption. that way an attacker can't test as many passwords per second.
So maybe we can reuse some of that math
the world record processor speed is only like 8.5 gigahertz. Which is only like 5x faster than typical commercial hardware.

So if the average user needed 10 blocks to check if they had won, the best software available would still need 2 blocks.

I wonder how fast an ASIC would be, if it was built for time-delay sha256 crypto.

https://www.gwern.net/Self-decrypting-files a blog post on this topic

I think the average user doesn't need to run the time-delay crypto operation.
We can set it up so anyone can run the time-delay operation, and win a reward for proving who won.
Since whoever can prove the winner first gets a prize, we will have a good estimate of how long it took to find the winner. So if hardware improves, we will know when it is time to make the time-delay more expensive.
If the fastest computer needs 30 blocks of time to know who won the lottery, then an attacker would need to bribe ~30 miners to not publish their blocks. That way the attacker has enough time to calculate what the next block should be to make them win.

So lets estimate the cost of those 30 bribes.
The first one is as expensive as a block reward. Because the miner is forgoing a block reward by deciding to not publish.
The second one would again cost the miner 1 block reward to not publish. But the 2nd miner knows that you already paid 1 bribe. So they know that if they publish the block immediately, the cost to the attacker is 1 block reward. So that means the 2nd miner, they can demand a bribe of 2 block rewards.

The Nth miner would need a bribe worth (1 block reward) + (sum of all the bribes so far)
Which works out to (2^N)*(block reward)

So if we increase the time-delay linearly, the cost to attack the system increases exponentially.

If the fastest RNG computation takes 30 blocks, then the total bribes to attack one sortition chain would be ~1 billion block rewards.
ok, I think we have finally solved it
oh, I think it is even more secure than that.
Every time an additional miner finds a solution, you would need to go back and increase the bribes of everyone you had already bribed, and it is impossible for all the bribes to be high enough. 

Out of B bribes, the only way for the Nth bribe to be big enough is if it is bigger than the other B-1 bribes put together, which is impossible.


you can combine each bribe with a safety deposit.
So you only need to pay (block reward)*(2^N).
But the total value of the safety deposits would be N*(block reward)*(2^N), which is far bigger than the total money in the system.
It works if we use a mining pool too. Even if the miners don't know the blocks they found.

The attacker would have to keep paying bigger and bigger rewards from their pool exponentially. Otherwise a miner will mine a block in a different pool to punish the attacker.


Estimating time for on-chain proofs
=================

if we do 10million hashes per second for the time delay function, and we set it up to take 30 blocks, and each block is 10 minutes = 600 seconds, then the on-chain proof to show that you won is log2(10 million * 30 * 600) = 37.5

so the proof has about 38 rounds.
If you have 20 blocks of time to provide evidence for each round, and the person trying to show that you cheated also has 20 blocks, (20+20)*37 = 1480.

So at most, it could take up to 1480 blocks to prove that you won and get your winnings out of the sortition chain.
That is about 10 days, if we have 10 minutes per block.
if we reduce the block time down to 2 minutes, I guess you would only need 2 days at most to get your money out.
if a sortition chain is worth N block rewards, we only need log2(N) many blocks of time delay to get the money out.
currently, the market cap is less than 1 million block rewards. So we only need 20 blocks of time of verified delay at most, not 30.

given that a typical CPU can do over 1 million serialized hashes per second, and that we can safely spend 30 seconds verifying each block, it seems like we could safely group up the VFD into bunches of 1 million hashes. Which would reduce the number of on-chain steps by log2(1 million) =~ 20 fold.

combining both of these improvements, instead of needing 10 days with 10 minute blocks, you would only need 8 hours.
Also, if different people are doing VDF proofs on-chain in the same block, we can verify each in parallel. 
So if each one takes at least 1 second, and we have 8 cores, then we can do 8 in 1 second.

Fast verification of RNG
=========
computing the RNG initially takes like 8 hours or so. But once we have solved it once, it can be efficiently verified using GPU.
The GPU can simultaniously verify many batches of VDF processing.

The RNG computer is based on a CPU SHA256 miner written in C.
The RNG verifier is based on a GPU SHA256 miner written in cuda.
So verification will be faster based on the relative speed difference between CPU mining and GPU mining.
