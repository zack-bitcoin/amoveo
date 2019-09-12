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

We have a fork choice rule weight(H, P).

Increasing PoS participation, or increasing hashpower, can only have a positive influence on the weight of that subchain.

The cost to take majority control of the PoS portion is cheap, because of market failure in voting systems. So lets consider the case where the attacker has 2/3rds control of the validator stake.
normal = weight(H0, P), attacker = weight(H1, P*2).

To calculate how much hashpower the attacker would need to have exactly 50% likelyhood of the attack succeeding:

F1: weight(H1, P*2) = weight(H0, P).

lets suppose that the pow/pos hybrid design is more secure than pow, or at least equally secure. This means that the attacker's hashpower needs to be bigger than the network hashpower for the attack to succeed. (We will use proof by contradiction to show that this supposition is false.)

S1: H1 > H0.

Since weight only increases as participation increases, that means that:

F2: weight(H1, P*2) > weight(H1, P)

since weight only increases as hashrate increases, and H1 > H0, that means that:

F3: weight(H1, P) > weight(H0, P)

combining F2 and F3, we get that

weight(H1, P*2) > weight(H0, P).

but this contradicts with F1.
so S1 must be false.

Therefore H0 > H1.

This means that it is always cheaper to do a hashrate rental attack against a pow/pos hybrid than against a normal pow blockchain.

