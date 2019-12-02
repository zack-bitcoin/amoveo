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

Sudoku Scalability
=========

The sortition chain probabilistic value space is divided up by each bit of entropy.
So an attacker who owned 1/4 of the total value, they could carefully decide which parts of the probabilistic value space to own to make sure that exactly 2 bits of the entropy completely determine if they win or not. Then they can focus on attacking just those 2 bits.

So to prevent the attacker from being able to focus their attack on any particular bits, we want people who own value in the sortition chain to own collections of parts of the probabilistic value space such that they are evenly 50-50 hedged for all of the entropy bits that will get generated.
That way, if an attacker attacks one particular bit, they will have no influence on whether you win or not. The attacker would have to attack all of the bits simultaneously to earn any profit, which is computationally impossible, because each additional bit being attacked costs twice as much as the previous.

For this defence to work, we need to show that the contracts to specify how to divide up the probabilistic value space, that these contracts are not excessively long to program.
We also need to show that these additional requirements are not overly onerous for the operators who need to maintain the database of who owns which parts of the probabilistic value space.

These contracts for owning value that is fully hedged, such a contract has a logic to it that is similar to solving a sudoku puzzle, which is why I call this the "sudoku strategy".
I suspect that these contracts will grow in complexity as (txs per second)^2
but since we can make each individual sortition chain very small, this exponential doesn't matter.
It would be like O(log((# txs)^2))
which is the same as O(log())

Calculating the size of the most general way of specifying hedged sortition contracts
==============

So, lets try to find some principles about how to specify these collections of hedged probabilistic value spaces.
For our simple model, we will consider a probabilistic value space divided up by 3 bits of entropy.
So there are 8 possible outcomes.
000
001
010
011
100
101
110
111

Lets try to find all possible hedged collections out of these 8.

Pairs of hedged points:
000 111
001 110
010 101
100 011

There are 4 possible pairs of hedged points, so it would take at least 2 bits of information to specify one of the pairs.

For any hedged collection, it is always possible to express it as a collection of hedged pairs.

There are 4*3=12 possible sets of 4 hedged points. So it would take 4 bits of information to specify one of the 12 possible sets of 4 hedged points.

Before we only looked at the probabilistic space defined by 3 random bits.
Next we consider N random bits.

There are 2^N possible outcomes.

There are 2^(N-1) ways to specify a pair of hedged outcomes. So any individual hedged pair can be specified with N-1 bits.

if you buy M pairs of hedged outcomes, there are (2^(N-1))! / ((2^(N-1))-M)! ways to specify these M pairs.

plugging in some values of N and M and calculating how many bits of information we would need to encode such a contract:

N=20, M=2 -> 38
N=20, M=3 -> 57
N=20, M=4 -> 76
N=20, M=5 -> 95

Seems like the general solution needs (N-1)*M bits to fully specify the arbitrary hedged contract.
So if we fully divided up the sortition chain into contracts like this, it would require (N-1)*(2^N).

Bitcoin uses about 40 bits to specify a quantity of value.

We would need about 39 trillion bits of information to keep track of all the contracts, if we divided it up into 20 million possibilities, and used the most general encoding to specify each contract.

Using baby sortition chains to limit the size of these contracts
===========

If we limit ourselves to only using at most 16 bits of entropy per sortition chain, then the size of the database is
(16-1)*(2^16)*(32 bytes per account) = 15*65536*32 =~ 32 megabytes, which is manageable.
And at most, each of these sortition chains could have up to 65636 accounts.
So if you used 4 levels of sortition chains, that could have up to 1.8*10^19 accounts.

with 16 bits, the smallest amount you could own is ~1/64000th of the value in the sortition chain.

A more efficient encoding of these contracts
===========

We already saw that diving up sortition space using N bits means the total size of contacts would be (N-1)*(2^N).

But, in general, it is always possible to have an order for the pairs of hedged points.
That way, if you wanted to own 4 hedged pairs, you could buy a list of 4 consecutive pairs. That way a contract to specify them only needs to specify the first, and the length of how many you want to own.

So if we are dividing up the sortition space using N bits, and we want to divide it up into M sub-ranges, the total amount of information to specify this is:

M*(N-1)*2

This shows that the total cost of specifying the contracts only needs to increase linearly with the number of accounts that exist at one point in time.


Using baby sortition chains to limit the size of these contracts
===========

If we want to divide a sortition chain into parts that are 1 billionth of the total size, then we need at least 30 bits of entropy.

If we limit ourselves to only using at most 30 bits of entropy per sortition chain, and we limit it to 1000 accounts per sortition chain, then the size of the database for each sortition chain is
(1000 * (30-1) * 2 * (32 bytes per account)) ~= 60000 = approximately 2 megabytes. which is extremely manageable.

with 1000 accounts per level, if you used 4 levels of sortition chains, then you could specify up to 1 trillion accounts.
