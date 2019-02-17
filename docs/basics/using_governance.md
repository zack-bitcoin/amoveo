using governance
==========


We need to make futarchy markets before we update the governance variable, that way the community can come to consensus about what updates should be made.

The oracle is a reporting mechanism, it isn't made for coming to consensus. If you try to use it to come to consensus, it is like a game of chicken, and is bad for the network.


For example, if you want to make governance oracle to change the block reward size, you first need to make a question oracle of something like `Q="we increase the block reward by 20%";P="the market cap of VEO exceeds $5 million";(P and Q)`
You also need an oracle with (P and !Q), and oracle with (!P and Q) and an oracle with (!P and !Q).
Each of the 4 oracles needs a binary market.
We can look at the prices in the 4 markets to determine which decision is best.

(P1 * P3) - (P2 * P4)
if it is positive, then the outcomes are positively correlated. if it is negative, then the outcomes are negatively correlated.

[more about futarchy here](futarchy.md)