Harmonic Random Number Generator
============

draft 4

It would be nice if we could securely generate random numbers inside of a blockchain. This would allow for things like probabilistic value transactions, lotteries, and probabilistic-value sharding for scalability.
The goal of this paper is to show how to do this.

A simple example of a broken RNG
===============

Thanks to Micah from Augur's discord for explaining this to me.

Lets start by considering some simple example RNG, and find out why they do not work.
One simple kind of RNG is to have every block add 1 bit of entropy.
Which means that when we generate our random number, we take the final bit from the hash of the most recent 256 headers, which makes 32 bytes. These 32 bytes are the seed to generate as much randomness as we need.

The problem with this strategy is that whoever mines the very last block before the RNG is generated, that person will have 1 bit of influence in the outcome, and it only costs them 1/2 of a block reward.
1 bit of influence can, at most, give an attacker an extra 25% control of the outcome of the contract.
So this simple example can only be secure if the amount of value controlled by the RNG is less than 2 block rewards.

How about the case where an attacker finds the second-to-last block before we generate RNG. How much profit can an attacker earn by being able to use their influence to try and win?
In this case, the attacker doesn't know what bit will be generated from the next block, so they have less information available to know if re-mining the block is helpful to them. The attacker only gets about 25%/2 influence for finding this block.

In this simple example where every block is generating 1 bit of entropy, the very last block gives 25% influence, and the second to last block is giving less influence.
Since the very last block has such a large amount of influence, that is our weakness.

This base example could support sortition chains with 2 block rewards of veo locked in them.


Harmonic RNG
========

A basic motivator for the harmonic RNG design is that all the blocks should have equal influence over the outcome. That way a miner who finds the last block has no advantage over the miner who finds the 2nd to last, or 3rd to last.

If we gather entropy quicker than this, then the last blocks found have too much influence.
If we gather entropy more slowly, then it takes more blocks to find for the same amount of security.
This path is the fastest way to gather the entropy securely.

in order to achieve this, we need to skew the probability that a block will add a "1" or a "0" to the entropy.
we call these probabilistically skewed bits partial-bits (pbits).

The probability that each pbit is a 1 decreases according to the harmonic sequence.

So lets show that it actually is the harmonic sequence.
Suppose one value 3/35 is the last value of the sequence, what should the second to last be so they both have the same influence?

Whoever can choose 2nd to last, they will assume that the last value will be a 0, because there is 32/35ths chance it will be.
so the second to last person is 32/35ths as likely as the last person to make the right choice.
So we need the 2nd to last value to be (3/35)/(32/35), which is (3/32)

so the normal sequence is like 3/6, 3/9, 3/12...


```
P(N) = the probability that block N produces a "1".
P(N) = 1/(2+N).
```

1/(2+N) is the generic form of a harmonic sequence.


Intuition of Harmonics
=========

Harmonic series happen in systems that involve a constant rate, and we are measuring the inverse of accumulative amounts of whatever is being produced at the constant rate.

For example, if you drive 20 kph for 1/2 the trip, and 60 kph for the second 1/2 of the trip, then your average speed is 30 mph, not 40 mph. https://en.wikipedia.org/wiki/Harmonic_mean
In this example, the constant rate is the rate of time passing. and our speed in kph varies inversely with the amount of time. kilometers/hours. to know our average speed, we are accumulating over how much time passed in the different steps, and dividing the total distance by the total time.

For producing entropy on the blockchain, our constant rate is the rate at which uncertainty in the result is decreasing.
the "amount of uncertainty left" is the same as `(total entropy we will generate)-(the accumulated influence so far)`

```
p1(i) ~ 1/reward(i).
reward1(i) ~ (accumulated amount of influence from 0 to i) 

The p1 (i) = harmonic (i) because p 1(i) varies as 1/(the accumulative amount of influence so far)
```


Variable Block Reward
===========

Using the harmonic pbits, the influence has been concentrated onto the rare 1-pbits.
So an attacker who can reroll just these blocks could have too much influence.

Since the 1-pbits are so rare compared to 0-pbits, we can pay a much higher reward for 1-pbits, without significantly reducing the reward for 0-pbit blocks. In this way, we can make the cost of reroll attacks arbitrarily high.

if block B(N) has pbit of 0, we should pay 1/2 the normal block reward.
if it has a pbit of 1, on block N, we should pay N/2 times the normal block reward.

Expected payout of a block
===========

So on any B(N), the expected payout of mining that block is:
```
BR = normal block reward;

(prob_pbit_0 * reward_for_pbit_0) + (prob_pbit_1 * reward_for_pbit_1)
= (((N-1)/N)*(BR*N/2(N-1))) + (((1/N)*(BR*N/2)
= BR/2 + BR/2
= BR
```

So the expected payout is exactly the same as a normal block reward.

Cost for 1 bit of influence of the outcome
===============

This way the cost of rerolling a 1 can be very high.

If we use 1000 blocks to gather entropy, there is a >50% chance the final 500 blocks have a 1 pbit in them.
Whoever mines that block, if they want 1 bit of influence over the outcome, they would need to give up ~750 block rewards.
And they are adding log2(500) = 9 bits of entropy.

In the time between block 250 and block 500, there is a 50% chance that there will be a 1 pbit. whoever finds that would need to give up about ~375 block rewards to have  1/2 bit influence over the putcome
And they are adding log2(500) = 8 bits of entropy.

....

In between block 0 and block 0, there is a 50% chance that there will be a 1 pbit. whoever finds it would need to give up 1/2 a block reward to have any influence. but there is ((1/2)^10) bit influence over the outcome


If we are generating X bits with the harmonic method.
In between block N and block 2*N, there is a 50% chance there will be one or more 1-pbits. whoever finds it has the option to give up `(2^(n-1))` block rewards to have `((1/2)^(X-N))` bits of influence.

```
influence(N)/cost(N) = ((1/2)^(X-N))/(2^(N-1))
= 1/(2^(N-1+X-N))
= 1/(2^(X-1))
```
Since all the N's cancel out, that means the cost per bit of influence is not dependent on N, it only depends on X, which is log2(number of blocks of time we spend generating entropy).

so, if we are doing 1024 blocks of harmonic RNG, then anyone with the ability to influence the outcome will have to pay 1 block reward for 1/(2^(10-1)) = 1/(512) = 1/512 bits of entropy.

in other words, 1 bit of entropy of control of the outcome costs 512 block rewards to buy.

How much value can we fit in our probabilistic value smart contracts?
============

1 bit of entropy is worth at most 25% of the money at stake in a sortition chain.
So this means we can have over 2048 block rewards in a probabilistic value smart contract, if we need to use 1024 blocks to gather entropy for them.

So if we had 1000 probabilistic value contracts in parallel, we could have 60 thousand block rewards locked in them all together. 60 block rewards in each one.



Confidence in the final outcome
============

lets say we are doing a harmonic sequence of C many blocks in order to generate entropy. starting at block B(0) and finishing at block B(N).
At any block height, we could come up with a best guess for the most likely entropy that will be generated from the entire process.

The probability that our best guess is correct = E(N)
= N/C

So our confidence in the entropy being produced is increasing linearly with the number of blocks found.

The fact that confidence increases linearly is sufficient to show that the probability of pbit N being a 1 is following the harmonic series

Harmonic sequence is a unique solution
=================

E(N) = how confident we are that we can predict the outcome at step N of the process of generating entropy.
P0(N) = the probability that pbit N is a 0.
P1(N) = the probability that pbit N is a 1.

If E increases linearly, that means `E(N+1) = E(N) + C`, for some constant C.
```
-> E(N) = E(0) + N*C
```

we know that `E(N+1) = E(N)/P0(N+1)` because of the definition of P0.
```
-> E(0) + (N+1)*C = (E(0) + N*C) / P0(N+1)
-> P0(N+1) = (E(0) + (N)*C)/(E(0) + (N+1)*C)
```
```
P1(N) = 1 - P0(N) ->
P1(N) = 1 - ((E(0) + (N-1)*C) / (E(0) + (N)*C))
= ((E(0) + (N)*C) - (E(0) + (N-1)*C))/(E(0) + (N)*C)
= C/(E(0) + N*C)
```

Lets plug in for the zero case to solve for the constant.
```
-> P1(0) = C/(E(0))
-> E(0) = C/P1(0)
```

we know that P1(0) = 1/2.
```
-> E(0) = 2*C
-> P1(N) = C/(2*C + N*C) = 1/(2+N)
```

Which proves that `P1(N) = 1/(2+N)`, which is what we were trying to show.

[more math about why it is the harmonic sequence](more_harmonic_evidence.md)


Relating Block Reward with sortition chain volume
================




<!----



Combining Uncertain Expiration with Harmonic
==========

Start with the uncertain expiration design.
Instead of one bit per block, have one pbit per block following the harmonic sequence.
If there is a 1-pbit on step N, then pay N block rewards.

So this means each person can use their secret to look up how many more blocks they need to wait until they can publish a proof to win the sortition chain, assuming:
1) no more entropy is added
and
2) no one else wins before them.

So if it looks like they wont win it, they will want to create a 1-bit to reroll the entropy.
If it looks like they will win it, they will want to create 0-bits so the result wont change.

Forcing a 1-bit cost
==========

if we are on step N of gathering entropy, and the expected number of blocks is L, the odds of finding a 1-bit on this block are 1/N.
So you would need to mine N blocks to force one reroll.

Forcing 0-bits cost
=========

if we are on step N of gathering entropy, and the expected number of blocks is L, the odds of finding a 1-bit on this block are 1/N, and the cost is N. So, we are giving up 1/2 of our mining profits to do this attack.

--->
