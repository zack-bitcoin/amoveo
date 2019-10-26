Dfinity random number generator
================

The goal of this paper is to show that Dfinity's RNG does not work.

The RNG design
============

this RNG works by using a deterministic signature scheme, where n randomly selected users are selected to make a signature, and we only need a subset t of them in order to make the signature.
It doesn't matter which t of the users are participating in making the signature, they generate the same signature either way.

This way anyone who wants to anticipate the entropy produced for a sigle block would need to control > t/n of the space that is being randomly selected inside of to choose who is able to sign, and anyone who wants to block a block's production  would need > (n-t)/n of the space.


Why it does not work
============

Let's suppose an attacker has H portion of the space being selected inside of, and that n users are selected to make each signature.
The number of signatures that the attacker has control of is usually H*n, and it has standard deviation of H*sqrt(n).


If for any single block, the attacker controls more than t/n of the accounts, then the attacker can predict the entropy ahead of time.
An attacker that can predict the entropy can know which parts of the currency space they need to own to win the next round of random selection.
So once the attacker breaks this random number generator once, they will be able to maintain centralized control of this blockchain forever. They will be elected block producer of every block from now on.

If the attacker controls more than (n-t)/n of the accounts, then the attacker can prevent a signature from being produced, which gives the attacker one bit of influence of the outcome.

If the attacker doesn't control enough hashpower to do this attack alone, it is cheap to bribe other validators to reveal their signatures ahead of time, or to refuse to make signatures, so the attacker can anticipate the entropy that will be generated.

If t/n is less than 1/2, it is easier to attack by stuffing the ballot box. Controlling lots of participants.
If t/n>1/2, the it is easier to attack by withholding signatures and preventing new random values from being generated.
If no new randomness is prevented, then we can predict which accounts will be elected to generate future randomness, so an attacker knows which parts of the currency space they need to control to successfully take down dfinity.
It t/n = 1/2, then the attack involves a mix of both strategies.
