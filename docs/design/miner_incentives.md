Miner incentives
=========

A question people have been asking about bitcoin: "Will Bitcoin still work if the block reward goes to zero, and only tx fees incentivize mining?"

Similar question we could ask: "If there is a race to claim a reward on the blockchain, and the reward is bigger than the block reward, will the blockchain maintain consensus?"

This is an important question for any on-chain markets. If the price in a market suddenly changes, the miners will be able to put in their own transaction to profit from the change. If this profit is significantly bigger than the block reward, miners might be incentivized to re-mine history instead of mining on the top block.

It can also be an important question for Amoveo's oracle, because it has an on-chain market.
Whoever is making honest bets in the oracle will double their money, so the miners will want to make the bets first to claim the reward. This could cause miners to be incentivized to re-mine history instead of mining on the top block.


Game Theory
========

This game is similar to the classic game from game theory where there is a group of girls and one is blonde. All the guys are interested in the blonde girl. The guys shouldn't all immediately hit on the blonde one, because then the brown haired girls will feel like a second-choice, and wont be interested in the men.

So there are a couple of solutions to this:
1) no one goes for the blonde girl
2) the men form a cartel. They use randomness to select one of themselves to talk to the girl, and everyone else in the cartel agrees not to interfere.

Similarly the miners have 2 strategies:
1) no one tries to rewrite history and get the extra reward. They just mine on the top block like normal.
2) the miners form a cartel. They work together to win the extra reward, and they divide it evenly.

This is the kind of game where the biggest cartel wins.


How bad can this be for Amoveo?
=========

The biggest mining pool, or cartel of mining pools, will be able to win all the extra money and share it among themselves. It wont break consensus, but it could cause up to 2 blocks to come more slowly than usual. At worst they will take (normal block time)*(largest cartel's hash power)/(total hashpower) per block.
It isn't a stable nash equilibrium because different cliques of miners will keep reforming into different cartels, but at least consensus wont break.


solution
========

If it is possible to find some nash equilibrium, it will have to involve splitting up the winnings from the oracle to everyone that is mining in proportion to hash power.
Otherwise, it will be profitable for some cartel to work together to take the prizes and split it up among themselves.

The way to share the reward in proportion to hash power is by using something like anyone-can-spend txs in bitcoin. You create a slightly smaller prize for whoever mines the next block. This way the other miners will be incentivized to mine on top of your block instead of undercutting it.

Thank you to Fernando Nieto https://twitter.com/fnietom for explaining this solution to me.


