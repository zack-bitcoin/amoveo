
The threshold signature scheme needs at least t of the  n randomly selected users to participate.

Let's suppose an attacker has H portion of stake/hashpower.
The number of signatures that the attacker has control of is usually H*n. But it has standard deviation of H*sqrt (n).

So if we set n=100, then standard deviation is H*10.

So 0.1% of the time (once per week with 10 min blocks), the attacker controls more than H*n + (3*sqrt (n))

If for any single block, the attacker controls more than t/n of the signatures, then the attacker can predict the entropy ahead of time.
An attacker that can predict the entropy can know which parts of the currency space they need to own to win the next round of random selection.
So once the attacker breaks this random number generator once, they will be able to maintain centralized control of this blockchain forever. They will be elected block producer of every block from now on.

If the attacker doesn't control enough hashpower to do this attack alone, it is cheap to bribe other validators to reveal their signatures ahead of time, so the attacker can anticipate the entropy that will be generated.
If n/t is less than 1/2, it is easier to attack by stuffing the ballot box. Controlling lots of participants.
If n/t>1/2, the it is easier to attack by withholding signatures and preventing new random values from being generated.
If no new randomness is prevented, then we can predict which accounts will be elected to generate future randomness, so an attacker knows which parts of the currency space they need to control to successfully take down dfinity.
It t/n = 1/2, then both vulnerabilities are about equal
Since pos can't make randomness, and randomness is necessary for sharding, it seems like PoS is less scalable than pow.
