using governance
==========

We need to make futarchy markets before we update the governance variable, that way the community can come to consensus about what updates should be made.

The oracle is a reporting mechanism, it isn't made for coming to consensus. If you try to use it to come to consensus, it is like a game of chicken, and is bad for the network.

The basic idea of governance in Amoveo is called futarchy.
If you can show that a certain decision is good for the price of VEO, then the community makes that decision.

For example, you could make these 2 binary bets:
* "if update X is merged into Amoveo before block height H return bad, otherwise is the price of VEO below $100?"
* "if update X is not merged into Amoveo before block height H return bad, otherwise is the price of VEO above $100?"

You make an open offer to bet on both of them.
You bet on "true" for both.
You use the same odds for both bets.
If neither of these bets get matched for a period of time, that is evidence that the update would be beneficial for Amoveo

[more about futarchy here](futarchy.md)


<!---

for example, if you want to use futarchy to find out if we should merge an update
we should have 2 scalar oracles:
"if update is not merged, return bad. else, return the price of USD in VEO."
"if update is merged, return bad. else, return the price of USD in VEO."

Because if you make a bet in an oracle, and the result is bad, then each participant gets their money back that they had put in the bet.

using these 2 scalar oracles, we can show that the price of veo if we do the update is higher than if we don't do the update.

Scalar oracle 1 creates an asset that is the same value as VEO if the update is merged.
Scalar oracle 2 creates an asset that is the same value as VEO if the update is not merged.
By comparing these 2 assets, we can find out if the update is good for the price of VEO.

if there is simultaneously unmatched orders to long veousd if the update goes through and to short veousd if the update does not go through, and both of these are at the same price, we can have some confidence the price of veo will be higher if there is the update than if there is not.
since nobody wants to take the other side of those trades.

So here is a concrete example of a futarchy market:
```
if the block reward on 21 July at noon GMT is above 0.15 return 'bad', else { A = the price of USD in VEO from 0 to 0.3 on 21 July at noon GMT; B = the price of USD in Veo from 0 to 0.3 on 21 August at noon GMT; return ((0.15 - A + B) * 1024 / 0.3)}
if the block reward on 21 July at noon GMT is below 0.15 return 'bad', else { A = the price of USD in VEO from 0 to 0.3 on 21 July at noon GMT; B = the price of USD in Veo from 0 to 0.3 on 21 August at noon GMT; return ((0.15 - A + B) * 1024 / 0.3)}
```

For the above example, make sure that your futarchy bets expire before 21 July.

--->

