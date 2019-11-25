Spamming for control
========

I was recently asked this question.

> Is it possible to make a secure blockchain mechanism based on whoever spams the most txs can control the outcome?

This is a question who's answer can be mathematically calculated with trust theory. 

It is a bad idea to have attackers and defenders fight by spamming txs.
If a fight has the side-effect of causing blockchain bloat, or bandwidth bloat, this can become a vulnerability for the blockchain.

This is a specific example of the more general principle:
```
A mechanism has property X if whoever pays the most can control the outcome.
All mechanisms with property X are not secure.
```

I can show it with math.

Lets say that the attacker can steal P if the attack succeeds.
The attacker burns A on the attack.
The defender burns D on defense.
if D>A, the defender keeps P.
if D<A, the attacker takes P.

Looking at the attackers perspective, if the attacker is richer than the defender, then eventually the defender can't afford to pay more than some upper limit D.
The attacker pays D+epsilon to win.

So the cost to the attacker is D+epsilon-P, and the cost to the defender is D+P.

So, the cost to the defender is always higher than the cost to the attacker, meaning this mechanism only has level 3 trust https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/trust_theory.md

Any mechanism worse than level 2 trust will be more expensive than the competitors, it will be out-competed by price.

