Miners can earn more profit if at least 50% of them switch between Amoveo and Ethereum together at the same time.
We need to show that this strategy is not a nash equilibrium.

Lets break the oscillation into 4 periods: A, B, C, D.

A and B) hashrate is high.

C and D) hashrate is low.

B and C) difficulty is high.

A and D) difficulty is low.

For the attack to work, at least 50% of the hashpower needs to mine Amoveo during A and B, and they need to not mine Amoveo during C and D. So we call the strategy of only mining during A and B "cooperate".

But the most profitable strategy is to mine during A and D, and to not mine during B and C. So we call only mining during A and D "cheat".

If many miners cheat by taking the more profitable strategy, then it makes the cooperate strategy less profitable.

So cooperating on this attack is not a nash equilibrium. From this situation, the incentive is for the hashrate to spread out, and for the attack to end.



Now we consider the case where there are 3 or more blockchains who oscilate at the same fequency, or have frequencies that are in some simple fractional ratio of each other.

If period D from one blockchain overlapped with period A from a different blockchain, can this cause cooperation to become a nash equilibrium for any of the blockchains?

Now we need to move to a continuous model.
Mapping the cycle A-B-C-D to the range 0->2pi
Difficulty can be approximated by a -cosine wave.

If the peak of the difficulty cycle (between periods B and C) for blockchain 1 happens at pi, and the peak of cycle 2 happens around pi/2, then cooperating during period D would be more profitable than cheating.
So it is possible to prevent cheating during period D.

Maybe this reasoning only works if the wave is taller for blockchain 2 than it is for blockchain 1. If that is the case, then if 2 blockchains are in harmonic ratio, whichever blockchain has bigger block rewards will suffer less damage.

Let us consider this attack strategy: mine during period A, and to not mine during any other period.
The difficulty would only move 1/2 as far, so this makes the attack 1/2 as profitable as it could be, if coordination was achieved.
The expensive parts of the attack are gone, so this makes the attack about 3/8ths as expensive to commit.

This attack is a nash equilibrium strategy, but it can only happen in the unusual situation where multiple blockchains are oscillating in simple harmonic ratios.
