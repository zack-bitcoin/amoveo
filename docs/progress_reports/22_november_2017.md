# Progress report on Oracle technology.

This is a progress report for the competition with the biggest prize of any competition on earth.


Financial derivatives are an important part of finance today. They are for risk management and investment.
Financial derivatives are the biggest part of the economy that can be put onto a blockchain.
The most popular cryptocurrency project that facilitates financial derivatives will be worth hundreds of trillions of dollars.

This purpose of this monthly progress report is to review the teams that are developing this technology, to see who is closer to winning the prize. This information is important to anyone who is investing in any of the teams.

## What does a blockchain need to win?
* state channels, necessary for scalability
* sharding, necessary for scalability
* light nodes, necessary for usability and security
* markets in the lightning network [explanation for why it is needed](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/state_channel_without_off_chain_market.md) 
* oracle, to teach the blockchain the results of each bet.
* a blockchain to host it all
* a community of users


## What teams are working on this technology?
Amoveo, Augur, Group Gnosis, Aeternity, Zen Protocol, Bitshares, STOX, Bodhi, and Bitcoin Hivemind.


Each team will be ranked in each of the categories from 0 to 10.
* 0 there is no plan on how to achieve goal
* 2 there is a plan on how to achieve the goal
* 5 most of the code is written for this goal.
* 8 there is working code with passing tests
* 10 there is an active community of people using this tool

```
channels, shards, light nodes, channel markets, oracles, blockchain, community
                 C  S  L  M  O  B  C    total
Amoveo           9  8  9  9  8  9  2    54
Group Gnosis     1  0  5  0  8  10 10   34
Augur            1  0  5  0  0  10 4    20
Stox             0  0  5  0  0  10 2    17
Bodhi            0  0  3  0  0  10 2    15
Bitshares        0  0  0  0  0  10 4    14
Bitcoin Hivemind 2  0  0  0  0  8  3    13
Zen Protocol     0  0  0  0  0  9  2    11
Aeternity        0  0  0  0  0  1  3    4
```



### Channels

[Here is bitcoin hivemind's plan for channels](http://bitcoinhivemind.com/blog/lightning-network/)

I can't find any work on channels from gnosis or augur, but they have both announced that they will use channels. Augur said they will use Ox for this.
I may be misinformed, as there are many Ethereum channels now. It seems likely that Augur and Gnosis can use them.

Amoveo channels are tested. Lightning payments are tested [here](/tests/test_lightning.py), many edge-cases for channels are tested [here](/apps/ae_core/src/consensus/txs/test_txs.erl).
You can try out Amoveo channels by installing the erlang full node and using the web browser interface.

Aeternity has no software for channels on their github, and they have not announced any plan for how their channels will work. They have hinted that Aeternity will have turing complete state channels capable of running off-chain smart contracts.

I can find no mention of channels in the stox white paper.

### Shards

[Here is a document explaining sharding in Amoveo](/docs/design/sharding.md). Since Amoveo doesn't store any contract state on-chain, sharding is simple.

Gnosis, Stox, and Augur use Ethereum, which lacks sharding.

Bodhi uses QTUM, which lacks sharding.

Bitcoin Hivemind is reusing the bitcoin source code, which lacks sharding.

Zen Protocol, Aeternity, and Bitshares have no plans for sharding that I can find.

### Light Nodes

[Amoveo has a working light node in the browser](http://159.89.106.253:8080/wallet.html).
Amoveo miners can use light nodes, they don't need full nodes.
The Amoveo network has no requirement for full nodes, it could be 100% light nodes.

Gnosis, Stox, and Augur use Ethereum, which has a light node, but the Ethereum light node has a very expensive worst case scenario because the blocks do not include the proofs needed for all the consensus state that the block uses. In the worst case, an Ethereum light node has to do everything that a full node does.

Bodhi uses QTUM which has light nodes, but since QTUM uses UTXO instead of accounts, it seems like QTUM would have the same light client limitations as bitcoin. If a light node tries to ask a full node for their balance, the full node would have to scan the entire UTXO set in order to make the proof for the light node. Paying >$1 just to look up your balance is excessively expensive.

None of the other projects have light nodes, or a plan on how to make light nodes.

### Markets

Some teams got a score of 0 for market because they are putting their markets on-chain. On-chain markets cannot scale with channels.
Bitshares, Zen Protocol, Stox, Bodhi, and Aeternity.

A few teams plan to put markets off-chain, but they are markets of brokers, which is much less efficient than an order book. This design also gets a 0.
Bitcoin Hivemind, Group Gnosis, and Augur.

Amoveo has off-chain markets with order books and single-price batches in the testnet. [You can see graphs of the market state here](http://159.89.106.253:8080/explorer.html)

### oracles

Most teams got a score of 0 for their oracle because their oracle mechanism cannot escalate, or because they used an insecure mechanism like voting or a trusted feed.

Oracles that cannot escalate are prohibitivly expensive, or they don't work.
For an oracle to be useful, it needs to give accurate information about the outside world, even when the amount of money being gambled on the oracle's result is much bigger than the amount of money in the oracle mechanism. For an oracle to function in those conditions, it needs to be possible for users realize that an attack is occuring, and to be incentivized to commit more money to the oracle to make it more secure. This way the situation can escalate to having more money at stake.[read more here](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/oracle_motivations.md)

Stox has a collateral which is given by the person who creates the oracle. The amount of collateral does not change, the volume of bets is limited by how much collateral is given by the oracle creator. Stox cannot escalate.

Aeternity and Bodhi use trusted feeds with no collateral. These are vulnerable to bribery and retirement attacks.

Augur has collateral in the form of rep, a subcurrency. The amount of collateral is the market cap of rep. The volume of bets happening in all markets secured by the Augur oracle is limited by the market cap of the Augur oracle, including off-chain bets in the channels. Augur cannot escalate.

Gnosis gives an explanation of their oracle with escalation [here](https://blog.gnosis.pm/a-visit-to-the-oracle-fefc9dec5462). the "Ultimate Oracle" is the part with escalation. It is implemented in solidity [here](https://github.com/gnosis/gnosis-contracts/tree/master/contracts/Oracles)

The Amoveo oracle has escalation. It is tested [here](/tests/test_market.py), edge cases of the oracle are tested [here](/apps/ae_core/src/consensus/txs/test_txs.erl)
The Amoveo oracle is live on the Amoveo testnet.

### blockchain

Gnosis, Stox, and Augur use Ethereum, which is a very popular blockchain.

Bodhi uses QTUM, which is a very successful blockchain.

Bitshares is a popular blockchain.

Amoveo, Bitcoin Hivemind, and Zen Protocol have live testnets where you can try out the services being built.

Aeternity does not yet have a testnet.


### Community

Gnosis is worth about $1.6 billion (only 5% were distributed so far).

Bodhi is on Qtum. The 24 hour volume of Bodhi is $170 000. I can't find out the market cap.

Bitshares is about $356 million

Augur is about $270 million.

Aeternity is about $169 million.

Stox is worth about $25 million.

The rest do not yet have value.

### How to make changes to this document

If you want to add an additional team to this list, or you want to correct any mistakes in this document, make an issue or pull request on github.
