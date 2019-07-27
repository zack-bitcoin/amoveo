Lightning Network + Probabilistic Payments for Amoveo narrative
=======

This document gives some stories of people using Amoveo in a possible version of it's final form we are aiming for in the future.


Alice likes to gamble on sports. She accepts 3 bet offers she sees on a server, this gives her 1 channel with a user, and 2 channels with hubs.
Eventually the bets finish. Alice won her bet against a user, and against one of the hubs, and lost against the other hub.
When she finished her bet against the user, the channel was closed and the money in it went to Alice.
When she finished her bets with the servers, the channels stayed open.
Now Alice wants to use her existing channels to make bets, that way she doesn't have to pay to open a new channel.

Lightning Probabilistic Payments
=========

The hubs don't have a channel together, but Alice can still instantly move her money between her two existing channels using lightning payments.
The step of the lightning payment between the hubs is done with a probabilistic payment. This way the hubs don't need a channel together, and you never have a limit on how much money you can transfer between hubs using lightning payments, and fees are always very low.
The lightning payment is always exactly 3 steps. a channel payment from Alice to her hub, a probabilistic payment between 2 hubs, and a channel payment from the recipient's hub to the recipient.

If Alice likes any available market in the hubs she has a channel with, she can move her money to that channel and participate in the market.
Or, if there is some third hub she likes, Alice can open a new channel with that third hub, and instantly transfer funds from her existing channels to her new one.

If Alice wins too much, and her channel fills up, she can instantly move her winnings to any of her other existing channels. This way she doesn't have to close channels or open new ones often.
Or, if she loses too much, she can instantly replentish her money from her other existing channels.

When Alice wants to purchase something, she can instantly make a payment from any of her existing channels, to anyone who has a channel with any hub.
All the hubs are connected together with 1 step because of probabilistic payments.

Withdraw
======

Lets say Alice wanted to withdraw exactly $100 out of the channel system, back to her on-chain account.
She could rebalance the amounts of money in her channels, so whichever channel she wants to close has $100 in it. Then she just closes that one channel.

Or maybe Alice doesn't want to close any channel, but she does want to withdraw.
She needs to find someone, Bob, who wants to have more money in his channel, and he already has $100 on-chain. Bob's channel can be connected to any hub, it doesn't have to be the same hub as Alice.

We can make a single lightning payment that swaps Alice's channel network veo for Bob's on-chain veo.
All that gets recorded on-chain is a single spend tx moving the on-chain veo from Bob's account to Alice's.
Alice starts a lightning payment that starts in any of her channels, goes to her hub, then to Bob's hub, and then to Bob, and Bob connects the same lightning payment to an on-chain spend tx that goes back to Alice, completing the loop. Alice reveals the one secret that simultaniously unlocks every step of the lightning loop.


Market Making Hub Specialists
==============

So lets say there is a channel hub Charlie running a market for people to make long-term bets in the presidential election. Charlie specializes in market making, not in long-term channel relationships. Charlie ideally would want to close his position in channels as quickly as possible, so he can free up the liquidity to allow more people to show up and make more bets.

We cannot push a presidential election bet through a probabilistic payment.
But if Alice and Bob both have channels with some other hub Dave who specializes in more longer-term channel relationships, we can move the bet from (Alice -> Charlie -> Bob) to (Alice -> Dave -> Bob).
That gives Alice and Bob more time to make a channel together, so that the bet can then be moved to (Alice -> Bob). Which frees up the liquidity in their channels with Dave, so they can use those channels for other things.

Market specialists like Charlie have lots of different people swapping in and out making new channels all the time. They tend to be smallish channels that only hold as much money as you would leave as open trades sitting on a market, or enough as you would make a single buy order on a market.
Having a channel with a market specialist like Charlie is more expensive per unit time.
Charlie runs a market, but all his profit comes from charging you per unit time to leave your money in channels with him.

Storage Hub Specialists
======

Storage specialists like Dave don't run any markets, and they are much cheaper per unit time. This is the normal way to store veo that you plan on using in the next month.
If Alice and Bob both have a channel with Dave, then they can negociate and create any contract with each other through Dave's hub. And they can optionally move these contracts to a direct path between Alice and Bob, freeing up liquidity on Dave's hub.

