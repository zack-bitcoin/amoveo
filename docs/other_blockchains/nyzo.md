Nyzo Review
======


This is Nyzo's white paper: https://nyzo.co/whitePaper

Nyzo is a very different kind of proof of stake blockchain in comparison to any other design I have looked at.

So first off, I will try to describe my understanding of the system. So if I am confused about the basics, it will be easier to identify which part I am confused about.


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


> "For a block signed by an existing verifier, the incremental score is zero plus four times the difference between the previous block's cycle length and this block's cycle length"

```
weight(block(n)) = 4*(vCycle(n-1) - vCycle(n))
```
so, if this validator hasn't signed in along time, the weight is -4. Otherwise, the weight is 4 times how much they are decreasing the cycle length by.

In nyzo a block with lower weight has higher priority. The full nodes have a fork choice rule to prefer the version of history with the lowest weight. A positive weight indicates a positive time delay that the network would have to wait before being able to process this block.


> "A “new” verifier is defined as any verifier other than the last verifier of the previous cycle. An “existing” verifier is defined as the last verifier of the previous cycle. If an existing verifier misses a cycle, it will be considered a new verifier the next time it verifies a block. "

The Nyzo team is defining the term "new verifier" to mean something special in this document. This is very confusing, because later in the same document they are using the words "new verifier" to talk about something completely different.

Also, the grammar is disagreeing with itself. is there exactly one existing verifier for each block? or are their multiple per block?

Also, the word "last" in english usually means the event that took place the nearest to the future, and the furthest from the past. But you seem to be using the word "last" to refer to the very earliest verifier in a cycle?

Given all my confusion with this paragraph, I am going to ignore it for my review.


> "a new verifier is only allowed if none of the other blocks in the cycle, the previous cycle, or the two blocks before the previous cycle were verified by new verifiers."
(Not clear if "new verifier" mean someone who has never verified before, or if it means anyone other than the "existing verifier"

This means that only one new verifier can join the verification set for every 3 cycles of nyzo.

The weight of the rare block with the new verifier is calculated differently. It is 6 by default. So that means that adding this new verifier would result in a higher priority block in comparison to skipping 3 verifiers in the round robin cycle.

But if this particular verifier candidate is winning an election, the weight can be lower, it can be -6, -2, or 2.

It isn't clear if this election is on-chain or off-chain.



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


Transaction Fees
=======

The tx fees are split between a block validator, and the previous 9 block validators. they are always 0.25% of the amount sent.






Profitable delay attacks.
=========

If you are a validator, and you manage to trigger a delay at the same time that you are collecting txs for your block, then you can get more txs and more fees.
So all you have to do is pay the correct other validator to not validate their block, and the extra profit you earn in fees will, on average, be more than the cost of the bribe.
If many validators use this logic, then the block time will be much slower than 7 seconds.




The problem is that we can only know the current validator set by verifying the history of votes at every block height.

It is cheap to pay a validator to sign alternative historical votes and build an alternative history. So it isn't possible to store a history off-chain
