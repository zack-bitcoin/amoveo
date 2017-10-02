# Progress report on Oracle technology.

This is a progress report for the competition with the biggest prize of any compeition on earth.

Financial derivatives are an important part of finance today. They are for risk management and investment.
Financial derivatives are the biggest part of the economy that can be put onto a blockchain.
The most popular cryptocurrency project that facilitates financial derivatives will be worth hundreds of trillions of dollars.

This purpose of this monthly progress report is to review the teams that are developing this technology, to see who is closer to winning the prize. This information is important to anyone who is investing in any of the teams.

## What does a blockchain need to win?
* state channels, necessary for scalability
* off chain markets [explanation for why it is needed](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/state_channel_without_off_chain_market.md) 
* oracle, to teach the blockchain the results of each bet.
* a blockchain to host all the technology
* a community of users

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
Amoveo     8    8  8  6  0      30
Gnosis     1    0  8  10 10     29
Augur      1    0  0  10 4      15
BH         2    0  0  8  1      11
Aeternity  0    0  0  4  3      7
```

Each team has a total score between 0 and 50.
A team needs all 50 points to have a shot at the $ trillion prize.


### channels

[Here is bitcoin hivemind's plan for channels](http://bitcoinhivemind.com/blog/lightning-network/)

I can't find any work on channels from gnosis or augur, but they have both announced that they will use channels. Augur said they will use Ox for this.
I may be misinformed, as there are many Ethereum channels now. It seems likely that Augur and Gnosis can use them.

Amoveo channels are tested. Lightning payments are tested [here](/tests/test_lightning.py), many edge-cases for channels are tested [here](/apps/ae_core/src/consensus/txs/test_txs.erl).

Aeternity has no software for channels on their github, and they have not announced any plan for how their channels will work. They have hinted that Aeternity will have turing complete state channels capable of running off-chain smart contracts.

### markets

Most teams got a score of 0 for market because they are putting their markets on-chain. On-chain markets cannot scale with channels.

Aeternity gets a 0 because they think that state channels are useful without markets. [This is false](/docs/design/state_channel_without_off_chain_market.md)

Amoveo off-chain markets are tested [here](/tests/test_market.py), edge cases for these markets are tested [here](/apps/ae_core/src/channels/market.erl).

### oracles

Most teams got a score of 0 for their oracle because their oracle mechanism cannot escalate.
Oracles that cannot escalate are prohibitivly expensive, or they don't work.
For an oracle to be useful, it needs to give accurate information about the outside world, even when the amount of money being gambled on the oracle's result is much bigger than the amount of money in the oracle mechanism. For an oracle to function in those conditions, it needs to be possible for users to commit more money to the oracle to make it more secure. This way the situation can escalate to having more money at stake.[read more here](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/oracle_motivations.md)

Gnosis gives an explanation of their oracle with escalation [here](https://blog.gnosis.pm/a-visit-to-the-oracle-fefc9dec5462). the "Ultimate Oracle" is the part with escalation. It is implemented in solidity [here](https://github.com/gnosis/gnosis-contracts/tree/master/contracts/Oracles)

The amoveo oracle is tested [here](/tests/test_market.py), edge cases of the oracle are tested [here](/apps/ae_core/src/consensus/txs/test_txs.erl)

### blockchain

The Amoveo blockchain was recently used to run a live testnet. It recently underwent some major upgrades. It should be running a new testnet better than ever soon.

Gnosis and Augur use Ethereum, which is the second most popular blockchain, so they both get a 10.

Bitcoin hivemind has a live testnet. They reused bitcoin code as much as possible. [here is the testnet software](https://github.com/bitcoin-hivemind/hivemind)

[The Aeternity blockchain is incomplete](https://github.com/aeternity/epoch). They are starting from scratch in erlang. Their consensus state is currently being stored in a database that can't recover from forks, so every time a small fork happens, every node that started on the losing side of the fork has to process every block since the genesis. [Aeternity is having a fast block time and using uncle-blocks with a varient of the GHOST protocol](https://blog.aeternity.com/state-of-development-week-of-sep-25-2017-804c50d5e5d1), this means that many nodes will end up on a small fork on every block.

### Community

Amoveo's community is by far the smallest.

Bitcoin Hivemind has only had private funding, so it is harder to measure it's community size. They have 1600 twitter followers, so I am using that to estimate.

Aeternity is worth around $100 million.
Augur is worth around $200 million.
Gnosis is worth around $1200 million.

# Discuss on reddit

[Discuss this on Reddit](https://www.reddit.com/r/Amoveo/comments/73tn7z/progress_of_each_of_5_teams_competing_for_the/)

# How to make changes to this document

If you want to add an additional team to this list, or you want to correct any mistakes in this document, make an issue or pull request on github.

