Nyzo Review
======


This is Nyzo's white paper: https://nyzo.co/whitePaper

Nyzo is a very different kind of proof of stake blockchain in comparison to any other design I have looked at.

So first off, I will try to describe my understanding of the system. So if I am confused about the basics, it will be easier to identify which part I am confused about.

Verification Cycles
==========

> "For any block in the blockchain, the verification cycle of that block is defined as the longest list of blocks, ending with that block, that contains no more than one instance of each verifier."

Here is a recursive function that is doing the same thing as what they are describing:
```
vCycle(0) -> 0;
vCycle(n) ->
  H1 = (the last height at which this verifier had verified a block),
  H2 = (current block height),
  X2 = H2 - H1,
  X1 = 1 + vCycle(n-1),
  min(X2, X1).
```

Fork Choice Rule
==========

> "For a block signed by an existing verifier, the incremental score is zero plus four times the difference between the previous block's cycle length and this block's cycle length"

```
weight(block(n)) = 4*(vCycle(n-1) - vCycle(n))
```
So, if this validator is the correct next validator in the cycle, the weight is 0. Otherwise, the weight is 4 times as many validators in the cycle are being skipped.

In nyzo a block with lower weight has higher priority. The full nodes have a fork choice rule to prefer the version of history with the lowest weight. A positive weight indicates a positive time delay that the network would have to wait before being able to process this block.

Adding New Validators to the Set
===========

> "a new verifier is only allowed if none of the other blocks in the cycle, the previous cycle, or the two blocks before the previous cycle were verified by new verifiers."

This means that only one new verifier can join the verification set for every 3 cycles of nyzo.

The weight of the rare block with the new verifier is calculated differently. It is 6 by default. So that means that adding this new verifier would result in a higher priority block in comparison to skipping 2 verifiers in the round robin cycle.

But if this particular verifier candidate is winning an election, the weight can be lower, it can be -6, -2, or 2.

So, the only times a new validator can be added to the set are:
1) if 2 other validators are removed.
2) if this is the 3rd most highly voted candidate, and 1 other validator is removed.
3) if this is the 1st or 2nd most highly voted candidate.

An important fact about Nyzo validators is that they have no minimum required balance of Nyzo. Each account in the validator set only has 1 vote, no matter how many coins that account owns.

Rapidly Shrinking the Validator Set
================

> "Past the Genesis block, the cycle of a block must be longer than half of one more than the maximum of the all cycle lengths in this cycle and the previous two cycles"

This rule means than in any sequence of 4 cycles, the 4th one will be at least 1/2 as long as the first 1.

So if you want to decrease the cycle length by a factor of 2^N, it would take at least 3*N many cycles to do it.

If every 4th one is more than 1/2 as long, then the sum of the first 3 cycles is at most twice as long as the sum of the next 3 cycles.

1 + 1/2 + 1/4 + 1/8 ... = 2.

So, if we obey the rules of Nyzo, the very minimum amount of time until we can decrease the cycle length to 1 block is the same as the amount of time for only 6 cycles, if the cycle length had stayed constant.

Voting
======

For a block to become completely finalized in Nyzo, the validator set votes on it.
Each validator in the set is always choosing a block that they consider to be the current tip of the finalize block chain, they sign a timestamped hash of that block. This signed timestamped message is called a vote.
The votes are gossiped, so all the validators can know how the other validators are voting. The votes are not included in the blocks.

Each vote includes a 32 byte hash and a 56 bytes signature, so it is at least 88 bytes.

88 bytes * number of validators * 12k blocks per day, is at least 1 megabyte of data per validator per day.
If there are 1000 validators, then the votes would take up 1 gigabyte per day.


Transaction Fees
=======

The tx fees are split between a block validator, and the previous 9 block validators. they are always 0.25% of the amount sent.


Validator vote slashing
========
If the majority of the validator set is upset with any particular validator, they can work together to have him kicked out.


Attacks
=========

Soft fork bribery attacks
==========

This section applying the attack described in this paper https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md to Nyzo.

An attacker can bribe the voters to use the validator-vote-slashing mechanism to kick out any arbitrary validator that they dislike.
If the attacker keeps doing this, eventually they can control the majority of the validator pool.

Because of market failure, also called tragedy of the commons, it is cheap to bribe the validators to do censorship.


Soft fork bribery attack that takes advantage of other core parts of Nyzo's consensus
==========

You can bribe the current set of validators to vote or not vote to finalize blocks. ("Freezing" the blocks, according to nyzo jargon).

The ability to censor blocks is a soft fork. Anyone who can cause a soft fork to happen can change the consensus rules in arbitrary ways, so that their control of the blockchain is permanent.

In particular, you could use a soft fork to exclude certain validators from participating in validation, which causes them to be kicked out of the validator set. By repeatedly doing this, eventually you can have control of 100% of the accounts in the validator set, and at that point your control of Nyzo would be absolute.

This is a level 4 failure mode. https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md

Normally the cycle would go Verifier_1 Verifier_2 Verifier_3...
But I bribe the verifiers to build an alternative version of history Verifier_1 verifier_3 verifier_4 ...
it skips verifier 2.

Then I bribe all the verifiers to freeze my alternative version of history that skips verifier_2.
So now verifier_2 is kicked out of the pool of verifiers.


