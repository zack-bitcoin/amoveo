Zano Review
=======

Here is the Zano white paper: https://zano.org/downloads/zano_wp.pdf

The Zano white paper is very high quality documentation. Possibly the best white paper of any blockchain project I have looked at.
The graphs are great, the explanations are clear.
Accurately communicating your work is critical for others to be able to review it and give helpful advice. This is where Zano shines.

Here is a general proof that PoW/PoS hybrid protocols are impossible: https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/pow_pos_hybrid.md


Privacy and Bribery
========

In general, proof of stake type mechanisms are vulnerable to bribery https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md

The optimal way to do this in Zano is by paying validators to move all their money to a different address, and to sell you a copy of their now empty private key, which was a valid private key a couple hours earlier.

In Zano the validators are hidden behind an encryption protocol, so we can't tell who the validators are. So the mechanism cannot delete any deposits to punish validators who sell their old private keys.

If the attacker buys up the majority of the proof of stake private keys for a point in history between the top block and the most recent checkpoint, then we are ready to go to the second stage of the attack.

Fork Choice Rule
========

in zano the fork choice rule isn't a cumulative weight function looking at a single history.
It is a function that compares 2 alternative histories, and chooses one.
It is a function with 4 inputs.

```
weight(P1, P2, H1, H2) = (P1 + H1) * P1*H1/(P2*H2)
```

multiplying it all out:
```
weight(P1, P2, H1, H2) = (P1*P1*H1 + P1*H1*H1)/(P2*H2)
```

In practice, we are always comparing the weigh of one side against the other.

```
weight2 = (P2*P2*H2 + P2*H2*H2)/(P1*H1)
```

if side 1 wins, that means ->
```
(P1 + H1) * P1 * H1 / (P2 * H2) > (P2 + H2) * P2 * H2 / (P1 * H1)
```
simplifies to ->
```
(P1 + H1) * (P1 * H1)^2 > (P2 + H2) * (P2 * H2)^2
```
simplifies to ->
```
weight(P, H) = (P + H) * (P * H)^2
```
This isn't the exact fork choice rule from zano. it is a simplification to give you a general idea of what sort of fork choice rule they use.

So lets suppose an attacker has 2/3rds control of the stake, and calculate how much hashpower they would need to take control. Fork2 is the attacker's side.

`P2 = 2*P1`

To keep it simple, the non-attacker's side of the fork has normal amounts of participation and hashrate. P1 = 1, H1 = 1

```
(1 + 1) * (1 * 1)^2 = (2+H2) * (2 * H2)^2
-> 2 = 8 * H2^2 + 4 * H2^3
-> 1 = 2 * H2^2(2 + H2)
```

Since H2 is positive, there is only one solution. H2 = 0.451 (I used wolfram alpha to calculate this)
Which means an attacker with control of 2x as much stake as the main chain would only need 45% control of the hashrate to take control of the blockchain.

45% < 51%, so it is cheaper to attack this protocol vs standard PoW.


Next lets see how much hashpower an attacker with 90% control of the stake needs:

P2 = 9
P1 = 1
H2 = ?
H1 = 1

(P1 + H1) * (P1 * H1)^2 = (P2 + H2) * (P2 * H2)^2

2 * 1 = (9 + H2) * 81 * H2 * H2

H2 = 0.052

So if an attacker controlled 90% of the stake, he would only need to control 5.2% of the hashpower to take control of the blockchain.

The actual result here should have been 20%, not 5%.
The problem is that I am using an estimate instead of the actual fork-choice-rule.


More Realistic Conclusion given the Current state of Blockchain Attacks
===========

Even though Zano is weaker than PoW in a strict game-theory sense, it seems like it could still be a better solution for now.

Because if someone started doing these PoW/PoS hybrid bribery attacks, there are a lot of other blockchains that are easier to attack than Zano, so we will have plenty of warning to switch to PoW at that point in time. Zano's excellent fork choice rule means that you still need a lot of PoW hashpower, even once you have bribed nearly all the PoS participants.

And until then the PoS aspects of Zano are giving good protection from hashrate rental attacks.
