Existing oracle mechanisms are built on top of voting. Voting can not adequately solve the oracle problem in a useful way because of tragedy of the commons. Betting mechanisms do not suffer this same limitations. 

Users want to use blockchains to gamble on future events. They want to gamble on the price of goods, for example.

An oracle consensus mechanism is a game on a blockchain designed so that the players are incentivized to reveal true facts about the world to the blockchain. Once the blockchain learns facts about the world, it can resolve the bets. Winning gamblers get their money, losing gamblers lose their money.

The oracle consensus mechanism's rewards and punishments are all measured in tokens.

USEFUL PRINCIPLE: For an oracle to be useful, it needs to give accurate information about the outside world, even when the amount of money being gambled on the oracle's result is much bigger than the amount of money in the oracle mechanism.

Existing oracle consensus mechanisms are built as voting mechanisms that can punish/reward the voters.
Voting mechanisms do not work.

Lets examine a voter Bob who controls 10% of the money in the oracle.
His vote controls 10% of the oracle's decision.
At most, his punishment for lying is the amount of money he has in the oracle.

If we offer him a bribe that is bigger than how much he could lose from lying, he will lie.
So, if we are willing to bribe more than the total amount of money in the oracle, we could get the oracle to lie.
By the useful principle, the total money in the oracle is much less than the amount being gambled.
So, it is profitable to bribe the validators this way and attack the oracle.



Now lets examine betting mechanisms.
There is a famous experiment. A prediction market with only N money of initial liquidity in it is created.
The outcome of the prediction market will be determined by the flip of a coin.
Some betters are offered a prize. If they can keep the price above 70% chance of heads, then they get 10N of prize money.
The prediction market would still say 50% chance of heads.

Every time the betters where were bribed moved the price up to 70%, it would create an opportunity.
For $0.40, anyone could buy an asset that has 50% chance of being worth $1. On average, a 20% profit.

The manipulators in comparison are paying $0.60 for an asset that has a 50% chance of being worth $1.
Every time they manipulate the price, they are giving money away as a prize to undo their manipulation.

So, a betting market can give accurate answers, even if some users are incentivized to make it lie.
Even if the incentive to make the market lie are bigger than the total amount of money in the market.
Betting markets satisfy the useful principle, so it is possible that they can be used as an oracle mechanism.


There are limitations of using betting as an oracle.
The only things we can bet on are facts that the blockchain already comes to know.
The only thing that the blockchain knows that is correlated with the accuracy of oracle decisions is the difficulty of finding blocks.


We can overcome these limitations.
If the oracle lied, then the users could do a hard-fork to fix the oracle's answer. So the attackers would lose all the money in the attack, and that money would reward the users who participated in defense.
If the miners prefer the honest chain, then the difficulty of finding blocks on the dishonest chain will go very low. So the attackers would lose their money on both forks, and defenders would be rewarded on both forks.


The big picture:
The network decides the outcome of oracles by hard forking the code.
Betting is used to cover the cost of making these expensive hard forks.