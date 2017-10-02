# Progress report on Oracle technology.

This is a progress report for the competition with the biggest prize of any compeition on earth.

Financial derivatives are an important part of finance today. They are for risk management and investment.
Financial derivatives are the biggest part of the economy that can be put onto a blockchain.
The most popular cryptocurrency project that facilitates financial derivatives will be worth hundreds of trillions of dollars.

This purpose of this monthly progress report is to review the teams that are developing this technology, to see who is closer to winning the prize. This information is important to anyone who is investing in any of the teams.

## What does a blockchain need to win?
1) state channels, necessary for scalability
2) off chain markets [explanation for why it is needed](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/state_channel_without_off_chain_market.md) 
3) oracle, to teach the blockchain the results of each bet.
4) a blockchain to host all the technology
5) a community of users

## What teams are working on this technology?
Amoveo, Augur, Gnosis, Aeternity, and Bitcoin Hivemind.

Each team will be ranked in each of the categories from 0 to 10.
* 0 there is no plan on how to achieve goal
* 2 there is a plan on how to achieve the goal
* 5 most of the code is written for this goal.
* 8 there is working code with passing tests
* 10 there is an active community of people using this tool

```
channels, markets, oracle, blockchain, community
           Chan M  O  B  Comm   total
Amoveo     8    8  8  6  1      31
Gnosis     1    0  8  10 10     29
Augur      1    0  0  10 10     21
BH         2    0  0  8  8      18
Aeternity  0    0  0  1  3      4
```

Each team has a total score between 0 and 50.

Most teams got a score of 0 for market because they are putting their markets on-chain.
On-chain markets cannot scale enough.

Most teams got a score of 0 for their oracle because their oracle mechanism cannot escalate.
Oracles that cannot escalate are prohibitivly expensive, or they don't work.

[For an oracle to be useful, it needs to give accurate information about the outside world, even when the amount of money being gambled on the oracle's result is much bigger than the amount of money in the oracle mechanism. For an oracle to function in those conditions, it needs to be possible for users to commit more money to the oracle to make it more secure. This way the situation can escalate to having more money at stake.](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/oracle_motivations.md)


[Discuss this on Reddit](https://www.reddit.com/r/Amoveo/comments/73tn7z/progress_of_each_of_5_teams_competing_for_the/)

If you want to add an additional team to this list, or you want to correct any mistakes in this document, make an issue or pull request on github.
