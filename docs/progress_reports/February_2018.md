### Progress report on Oracle technology.

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
* low trading fees
* software that can be maintained and modified, elegance.
* the ability to acknowledge and overcome limitations in their work.


## What teams are working on this technology?
Amoveo, Augur, Group Gnosis, Aeternity, Zen Protocol, Bitshares, STOX, Bodhi, spectre.ai, Fun Fair, ZeroSum Markets, Variabl, and Bitcoin Hivemind.


Each team will be ranked in each of the categories from 0 to 10.
* 0 there is no plan on how to achieve goal
* 2 there is a plan on how to achieve the goal
* 5 most of the code is written for this goal.
* 8 there is working code with passing tests
* 10 there is an active community of people using this tool

```
channels, shards, light nodes, channel markets, oracles, blockchain, community, trading fees, elegance, acknowledge limitations
                 C  S  L  M  O  B  C  F   E  AL  total
Amoveo           9  8  9  9  8  9  2  10  10 5   79
Group Gnosis     1  0  5  0  8  10 10 5   3  10  52
Fun Fair         2  0  5  0  0  10 4  10  3  5   39
Variabl          1  0  5  0  0  10 2  5   3  5   31
Stox             0  0  5  0  0  10 2  0   3  5   25
Augur            1  0  5  0  0  10 4  -5  3  7   25
Bitshares        0  0  0  0  0  10 5  -5  6  5   21
Bitcoin Hivemind 1  0  0  0  0  8  4  -5  5  10  22
Bodhi            0  0  3  0  0  10 3  -5  4  5   20
Aeternity        0  0  0  0  0  8  3  -5  9  0   16
Spectre.ai       0  0  5  0  0  10 3  -10 3  5   16
Zero Sum Markets 0  0  5  0  0  10 2  -10 3  5   15
Zen Protocol     0  0  0  0  0  9  3  -2  1  2   13
```

### Channels

[Here is bitcoin hivemind's plan for channels](http://bitcoinhivemind.com/blog/lightning-network/)

I can't find any work on channels from gnosis or augur, but they have both announced that they will use channels. Augur said they will use Ox for this.
I may be misinformed, as there are many Ethereum channels now. It seems likely that Augur and Gnosis can use them.

Amoveo channels are tested. Lightning payments are tested [here](/tests/lightning.py), many edge-cases for channels are tested [here](/apps/amoveo_core/src/consensus/txs/test_txs.erl).
You can try out Amoveo channels by installing the erlang full node and using the web browser interface.

Aeternity has no software for channels on their github, and they have not announced any plan for how their channels will work. They have hinted that Aeternity will have turing complete state channels capable of running off-chain smart contracts.

I can find no mention of channels in the stox white paper.

Fun Fair's documentation shows that they have a deep understanding of how turing complete state channels will work.

I can find no plan for channels in zerosum markets

### Shards

[Here is a document explaining sharding in Amoveo](/docs/design/sharding.md). Since Amoveo doesn't store any contract state on-chain, sharding is simple.

Gnosis, Stox, Fun Fair, Zero Sum Markets, and Augur use Ethereum, which lacks sharding.

Bodhi uses QTUM, which lacks sharding.

Bitcoin Hivemind is reusing the bitcoin source code, which lacks sharding.

Zen Protocol, Aeternity, and Bitshares have no plans for sharding that I can find.

### Light Nodes

[Amoveo has a working light node in the browser](http://159.89.106.253:8080/wallet.html).
Amoveo miners can use light nodes, they don't need full nodes.
The Amoveo network has no requirement for full nodes, it could be 100% light nodes.

ZeroSum Markets, Gnosis, Stox, Fun Fair, and Augur use Ethereum, which has a light node, but the Ethereum light node has a very expensive worst case scenario because the blocks do not include the proofs needed for all the consensus state that the block uses. In the worst case, an Ethereum light node has to do everything that a full node does.

Bodhi uses QTUM which has light nodes, but since QTUM uses UTXO instead of accounts, it seems like QTUM would have the same light client limitations as bitcoin. If a light node tries to ask a full node for their balance, the full node would have to scan the entire UTXO set in order to make the proof for the light node. Paying >$1 just to look up your balance is excessively expensive.

None of the other projects have light nodes, or a plan on how to make light nodes.

### Markets

Fun Fair has no short-term plan for markets.

Some teams got a score of 0 for market because they are putting their markets on-chain. On-chain markets cannot scale with channels.
Bitshares, Zen Protocol, ZeroSum Markets, Stox, Bodhi, Gnosis, and Aeternity.

A few teams plan to put markets off-chain, but they are markets of brokers, which is much less efficient than an order book with single-price batches. This design also gets a 0.
Bitcoin Hivemind, Gnosis, and Augur.

Amoveo has off-chain markets with order books and single-price batches in the testnet. [You can see graphs of the market state here](http://159.89.106.253:8080/explorer.html)

### oracles

Fun Fair has no short-term plan for oracles.

Zero Sum Markets doesn't have an oracle, maybe it will use Augur.

Most teams got a score of 0 for their oracle because their oracle mechanism cannot escalate, or because they used an insecure mechanism like voting or a trusted feed.

Oracles that cannot escalate are prohibitivly expensive, or they are insufficiently secure.
For an oracle to be useful, it needs to give accurate information about the outside world, even when the amount of money being gambled on the oracle's result is much bigger than the amount of money in the oracle mechanism. For an oracle to function in those conditions, it needs to be possible for users realize that an attack is occuring, and to be incentivized to commit more money to the oracle to make it more secure. This way the situation can escalate to having more money at stake.[read more here](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/oracle_motivations.md)

Stox has a collateral which is given by the person who creates the oracle. The amount of collateral does not change, the volume of bets is limited by how much collateral is given by the oracle creator. Stox cannot escalate.

Aeternity and Bodhi use trusted feeds with no collateral. These are vulnerable to bribery and retirement attacks.

Augur has collateral in the form of rep, a subcurrency. The amount of collateral is the market cap of rep. The volume of bets happening in all markets secured by the Augur oracle is limited by the market cap of the Augur oracle, including off-chain bets in the channels. Augur cannot escalate.

Gnosis gives an explanation of their oracle with escalation [here](https://blog.gnosis.pm/a-visit-to-the-oracle-fefc9dec5462). the "Ultimate Oracle" is the part with escalation. It is implemented in solidity [here](https://github.com/gnosis/gnosis-contracts/tree/master/contracts/Oracles)

The Amoveo oracle has escalation. It is tested [here](/tests/market.py), edge cases of the oracle are tested [here](/apps/amoveo_core/src/consensus/txs/test_txs.erl)
The Amoveo oracle is live on the Amoveo testnet.

### blockchain

Gnosis, Stox, Spectre.ai, Fun Fair, Zero Sum Markets, and Augur use Ethereum, which is a very popular blockchain.

Bodhi uses QTUM, which is a very successful blockchain.

Bitshares is a popular blockchain.

Amoveo, Bitcoin Hivemind, Aeternity, and Zen Protocol have live testnets where you can try out the services being built.


### Community

Gnosis is worth about $5.1 billion (only 5% were distributed so far). up 9% since last report

Bitshares is about $1.2 billion, down 18%

Augur is about $910 million. up 10% 

Aeternity is about $670 million. up 100%

Fun Fair is about $420 million. up 90%

Zen protocol is about $80 million. still seems untradable.

Spectre.ai is worth about $61 million. up 3%

Bodhi is on Qtum. It is worth about $39 million. down 20%

Stox is worth about $35 million. up 25%


### Trading fees

This section works different. Each project starts out with 10 points, and there are several ways to lose them.

* Any project with on-chain markets will lose 5 points, because of blockchain transaction fees to participate in the market.  (Zen only loses 2 points here because they allow for parallel transaction processing in the same block).
* Any project with an oracle-subcurrency will lose 5 points, because of the trading fee that pays the oracle. Bitcoin Hivemind, Augur.
* Any project which forces you to use a subcurrency besides eth for trading will lose 5 points because of the cost of entering and exiting th emarket.
* Any project with trusted feeds will lose 5 points, because of the losses from theft. Aeternity, Bitshares.
* Any project where the oracle cannot escalate will lose 5 points, because this either means the oracle will either be too expensive for the security required, or else it will have insufficient security.
* Spectre.ai plans to charge trading fees between 4% and 11%, charging winners more than losers. This is far more expensive than anyone else's plan, so Spectre.ai loses an additional 5 points.


### Elegance

Over time software tends to become more secure. But it also becomes more brittle and harder to modify.
If a blockchain has not yet won this competition, and it becomes hard to modify, then that blockchain will not be able to win this competition.

Code elegance is estimated by the number of lines of code. Everyone starts with 10 points, and you lose points for having too much code.

Score = 10 - log2((your size) / (smallest size))

Amoveo - 8k

Aeternity - 20k

Bitshares - 126k

Bitcoin Hivemind - 329k

Bodhi, uses QTUM - 518k

Gnosis, uses Ethereum. Geth - 780k

Fun Fair, uses Ethereum. Geth - 780k

Variabl, uses Ethereum. Geth - 780k

Stox, uses Ethereum. Geth - 780k

Augur, uses Ethereum. Geth - 780k

Spectre.ai, uses Ethereum. Geth - 780k

Zero Sum Markets, uses Ethereum. Geth - 780k

Zen Protocol. - code is too unorganized for lines to be counted. Uncompiled, it is about 7 times longer than QTUM is. So I am estimating 3.5M

### Acknowledge Limitations

This is a little harder to test, so I gave 2 points to each project that I have not tested. If they contact me and start an open-source discussion we can publish about the limitation of their product, then I will immediately raise them to 5 points.

Other teams already have more points because I have already had discussions with them.

Group Gnosis. I met with Stefan George in Berlin. Since I already agree with almost everything Gnosis is doing, I had little criticism to offer.
I suggested that they stop working on a delayed decryption scheme, because I could prove that it would necessarily have limitations that make it unusable for oracles. 
They stopped working on the delayed decryption thing.
This shows that they are capable of acknowledging and overcoming mistakes. 10

Augur. The team acknowledges to me the limitations in their design, but their design is now constricted by political limitations. If the Rep was worthless, the investors would be unhappy, it might even be illegal. So the Augur team doesn't waste much time imagining what they are not allowed to do. 7

Aeternity. This team is using designs that will not work. They completely ignore their limitations, censoring anyone who mentions them on their channels. 0

Bitcoin Hivemind. Paul Sztorc is open to criticism. He has even changed the design based on a suggestion I put on his forum. I showed how to use hash-locking for privacy, and he studied hash functions to confirm that my advice was good.
We disagree about a lot of things, but Paul always has good reasons to support his design decisions. He considers everyone's opinions. 10

Zen Protocol. I met with Adam Perlow in Tel Aviv. He is a great guy. Very friendly, and very smart. But he isn't able to rationally look at the short-cominings of his design. He is more interested in the technology than the product. 2

Amoveo. I think it is impossible for me to test this. So i will jut give Amoveo 5.



### How to make changes to this document

If you want to add an additional team to this list, or you want to correct any mistakes in this document, make an issue or pull request on github.
