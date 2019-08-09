Cosmos
=======

Recently it was discovered that proof-of-stake cannot be a secure consensus mechanism. https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md

Cosmos is one of the most popular PoS blockchains.

The goal of this document is to explain how PoS will fail using Cosmos as a concrete example.

In order to take control of Cosmos consensus, we need to convince at least 2/3rds of the validator set to accept our patch and run our version of the software.
Our soft fork upgrade can be set up to only activate if >2/3rds of validators have accepted the patch. This way there is no cost to the validator for participating in a soft fork attack that fails.

But, if a validator fails to participate in a soft fork attack, and the soft fork attack succeeds, then that validator will probably lose all their stake as a punishment for not supporting the attack. So this means there is a non-zero cost for validators who want to refuse to participate in the soft fork attack. This makes the bribes to pull off this attack even cheaper.

The richest validators in Cosmos control 8% of the validator stake.
The cost to bribe a validator who has 8% of the stake is: (how much the price of Atoms is harmed by the attack) * (how much more likely the attack is to succeed if this validator participates) * (8% of the value of all the stake)

At worst, a successful attack can completely destroy the value of Atoms. The change in probability of success of the attack if this validator participates is about 8/100.
So we can know that the bribe is always less expensive than: (value of 8% of the stake)*(8/100) = value of 0.64% of the stake.

So this means it costs at most 0.64% of the value of all the stake to bribe a validator who owns 8% of the stake so that they will accept your soft fork update.

Other validators have <8% of the stake, so they are all cheaper to bribe in comparison.

If we need to bribe 2/3rds of the hash power, and we are always paying less than 64/800, then that means the total bribe to get 2/3rds of validators to accept your soft fork is less than  (2/3)*(64/800) = 4 / 75 = 5.33%

So the cost to convince 2/3rds of validators to accept your soft fork is less than 5.33% of the value of all the stake.

Once 2/3rds of validators have accepted the soft fork update, then the the soft fork will activate.
In order to make sure our soft-fork attack is stable, the new soft fork will enforce it's rules by freezing the desposits of any validators who disobey. In Cosmos PoS there are deposits available to be frozen, so a successful soft-fork attack results in a very stable coalition. The coalition has an easy time enforcing it's new rules.

A soft fork attack can change any aspect of consensus, including redistributing the coins however the attacker wants.

It costs < 5.33% of the value of all the stake in order to steal 100% of the value on Cosmos. This puts Cosmos deep into 4.2 level trust, it is worse than centralized alternatives. https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md
