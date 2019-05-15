using governance
==========


We need to make futarchy markets before we update the governance variable, that way the community can come to consensus about what updates should be made.

The oracle is a reporting mechanism, it isn't made for coming to consensus. If you try to use it to come to consensus, it is like a game of chicken, and is bad for the network.

for example, if you want to use futarchy to find out if we should merge an update
we should have 2 scalar oracles:
"if update is not merged, return bad. else, return the price of USD in VEO."
"if update is merged, return bad. else, return the price of USD in VEO."

Because if you make a bet in an oracle, and the result is bad, then each participant gets their money back that they had put in the bet.

using these 2 scalar oracles, we can show that the price of veo if we do the update is higher than if we don't do the update.


[more about futarchy here](futarchy.md)