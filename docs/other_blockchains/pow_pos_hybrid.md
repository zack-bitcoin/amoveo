PoW/PoS Hybrid Consensus Mechanisms
=========

Proof-of-work is powerful, but limited in it's capabilities.
There have been many efforts to combine the PoW consensus with consensus mechanisms that are based on staking coins, and in this way maybe we can add more features to the combined consensus mechanism.

Here are the motivations for the PoS parts of various hybrid designs that I have seen:
* voting
* oracle mechanisms.
* governance mechanisms. 
* faster finality.
* preventing hashrate rental attacks.

Voting
========

Measuring the opinions/desires of the average coin holder
Voting in blockchains is not something that can possibly be made in a secure way. https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/voting_in_blockchains.md
It is always vulnerable to manipulation in many different ways.

Oracle Mechanisms
=========

Bringing real-world data onto the blockchain. So we can bet on the outcome of sporting events for example.
This seems possible. See Amoveo.


Governance Mechanisms
========

Making decisions about how to change the consensus protocol.
This seems possible. See Amoveo.

Faster Finality
========

faster finality without increasing the block reward.
If miners make a bet that a certain block will not get reverted, this creates a measurable increase in the cost to revert that block.
So this seems possible.

Preventing Hash Rate Rental Attacks
========

If it is the kind of PoS mechanism that requires >50% participation, like Cosmos, then an attacker who took control of the PoS mechanism could censor any PoW block just by refusing to build on it. In that case, the PoW half of the mechanism is meaningless.

If it is the kind of PoS mechanism that is based on something like coin-age, so even if most PoS validators stop participating, PoS blocks can still be found, then we need to consider a some cases based on the fork-choice rule.
In a hybrid design, the fork-choice rule depends on some combination of P = portion of PoS validators participating, and H = the amount of hashpower.
Starting by considering a first-order polynomial. weight = a*P + b*H

considering a couple cases:
normal mode, 10% of value is staked, hashpower = 1. weight = a/10 + b.
hashpower attack, 1% of value is staked, hashpower = 3. weight = a/100 + 3*b.
bribe attack, 10% of value is staked, hashpower = 0.1. weight = a/10 + b/10.

9*a/100 > 2*b
a > 200 * b / 9.

lets try b=1, a=30

weight = 30*P + H

In general, we can set b=1, and by trying out different values for A we can explore the entire space of possible weighting algorithms. This is because if we multiply a weighting algorithm by a scalar, this mapping preserves the order of which block histories were considered higher weight than others. So there is only one free constant N=a/b.

So to hash-power attack, you need to increase the hashpower 30x over current level.
To bribe attack, you need to control 1/2 of the current stakers + (market cap)/30 more of the coins.

The general rule for first-order polynomial fork choice rules is that if we want to be secure against an attacker with Nx more hashpower than we currently have, then that means that an attacker would only need to bribe (1/2 the current stakers) + (market cap)/N more.

It is cheap to bribe the stakers to participate in an attack for all N>1.
But if N<1, that means it is cheaper to do hashpower attacks vs standard PoW.

So lets try N=1.

weight = P + H.
so if the 10% of value is staked, the weight is 1.1
An attacker would either need to control 110% of the stake (yes, more than 100% is impossible), or 1.1x the hashrate that is currently mining, or some linear combination of those 2 things. like 50% of stake + 0.6x the hashrate

So we can conclude that first order polynomials cannot result in a fork choice rule that would let pow/pos hybrid be more secure vs standard pow.


Next considering polynomials of the form weight = P + H + c*P*H
if the network has 10% staked, then the current weight is 1.1 + c/10

if an attacker bribes 60% of active stakers, then the main-chain's weight goes down to 1.04 + c/25
and the attacker has 0.06+H(1+c*0.06).

lets calculate how much hashrate the attacker needs to succeed:
H(1+c*0.06) > 0.98 + c/25
H > (0.98 + c*0.04)/(1+c*0.06)
try c=0 -> H > 0.98
try c=1 -> H > 1.02 / 1.06 -> H > 0.96
try c=10 -> H > 1.38 /1.6 -> H > 0.862

Increasing C above 0 only makes it less secure.
If C is below 0, then there are cases where increasing hashrate or stake participation decreases the weight, which is contradictory with the basic logic of how the fork choice rule should work.

considering other second order polynomials:
weight = P + H + f*P*P + e*H*H
if e<0 or f<0, then sometimes increasing hashrate or stake participation will cause weight to decrease, which is impossible. so we only consider e>0 and f>0.
The problem with e>0 is that every additional megahash of mining power added is more valuable than the previous. So an attacker that has only slightly more hashpower than the main branch, he will easily overpower consensus.
e>0 is only making the blockchain more vulnerable to hashrate attacks.
f>0 is making the blockchain even more vulnerable to bribe attacks than it would otherwise be. You only need a little more participation than the main branch to overpower it. so f>0 is not possible.
therefore, f=0 and e=0.

We have considered all possible first and second order polynomials to define the fork choice rule, and all were significantly less secure than PoW.
Preventing hashrate attacks this way makes us too vulnerable to bribery attacks.

