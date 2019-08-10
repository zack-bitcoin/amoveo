PoW/PoS Hybrid Consensus Mechanisms
=========

Proof-of-work is powerful, but limited in it's capabilities.
There have been many efforts to combine the PoW consensus with consensus mechanisms that are based on staking coins, and in this way maybe we can add more features to the combined consensus mechanism.

Here are the motivations for the PoS parts of various hybrid designs that I have seen:
* measuring the opinions/desires of the average coin holder.
* oracle mechanisms. Bringing real-world data onto the blockchain. So we can bet on the outcome of sporting events for example.
* governance mechanisms. Making decisions about how to change the consensus protocol.
* faster finality without increasing the block reward.
* preventing hash-rate-rental attacks.

Measuring the opinions/desires of the average coin holder
========

Voting in blockchains is not something that can possibly be made in a secure way. https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/voting_in_blockchains.md
It is always vulnerable to manipulation in many different ways.

Oracle Mechanisms
=========

This seems possible. See Amoveo.


Governance Mechanisms
========

This seems possible. See Amoveo.

Faster Finality
========

???

Preventing Hash Rate Rental Attacks
========

???

Common mistakes in hybrid designs:
=========

* requiring that a certain portion of blocks be created by the PoS mechanism.


Lets suppose that 1 in N blocks is required to be created by a PoS mechanism instead of a PoW mechanism.
If it is the kind of PoS mechanism that requires >50% participation, like Cosmos, then an attacker who took control of the PoS mechanism could censor any PoW block just by refusing to build on it.
If it is the kind of PoS mechanism that is based on something like coin-age, so even if most PoS validators stop participating, PoS blocks can still be found, then we need to consider a couple cases based on the fork-choice rule.
In a hybrid design, the fork-choice rule depends on some combination of P = portion of PoS validators participating, and H = the amount of hashpower.
Starting by considering a first-order polynomial. FCR = a*P + b*H

If an attacker takes control of 2/3rds of the proof-of-stake portion, then their FCR power would be about `2*a/3`, and the rest of the network would be `a/3 + b*H`.

I = the attacker's hashpower
We need it to be the case that a/3 + b*I < b*H
-> I < H-a/(3*b)
-> a < (H-I)*3*b
otherwise the hybrid mechanism will be vulnerable to the same bribery attacks as PoS mechanisms.

If a=0, then it is standard POW. An attacker needs >50% hashpower to attack.

if a>0, then it is hybrid. An attacker needs > (1/2)-a/(3*b) hashpower to attack, which is always <50% hashpower.
If a>(3*b/2), then it is vulnerable to bribery attacks with 0 hashpower.

therefore: requiring that a certain portion of blocks be made by PoS always results in a mechanism less than or equally secure as the pure PoW version would have been.




