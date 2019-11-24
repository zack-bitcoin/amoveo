Emin Gun Sirer Review
========

[His twitter](https://twitter.com/el33th4xor)

selfish mining
========

Emin in 2013

Emin's paper on selfish mining: https://www.cs.cornell.edu/~ie53/publications/btcProcFC.pdf

Emin attempts to show that you only need 1/4 of the hash power in order to gain 51% control of bitcoin block production.

Paul Sztorc explained about preventing selfish mining in this blog post: http://www.truthcoin.info/blog/mining-threat-equilibrium/
Which is based on the explanation from this blog post: https://bitcoinmagazine.com/articles/why-bitcoin-mining-pools-aren-t-incentivized-to-broadcast-blocks-quickly-1475249510/

Here is a paper that uses math to formalize the arguments from Sztorc and the bitcoinmagazine article https://eprint.iacr.org/2019/486.pdf

If any mining pool is doing a selfish mining attack, we would all be able to tell that the attack is happening, and we would all know which pool is doing the attack, and we would all know the hash of the blocks that the attacker has found.

So the other pools, they are incentivized to work together to exploit the attacker. They have 2 strategies available:
1) They can mine on the attacker's private block even though it has not been published, because they know the hash.
2) They can also choose to work together to always mine on the version of the block that did not come from the attacker.

By using these 2 strategies in the right situations, the defenders can prevent the attack, and actually steal profits from the attacker.

bitcoin ng
========

Emin in October 2015

Emin's paper on bitcoin ng https://www.usenix.org/system/files/conference/nsdi16/nsdi16-paper-eyal.pdf

The idea of bitcoin ng is that instead of mining on a block directly, a user wins the privilege of choosing which txs are valid for the next period of time.

Emin claims that the bitcoin ng strategy accomplishes these goals:
* same security model as bitcoin
* capacity for tx throughput only limited by bandwidth of nodes
* faster finality

So, if bitcoin ng accomplished all the goals it set out to accomplish, it would allow bitcoin to process around 100x more txs per second, and it would take 100x longer to sync bitcoin blocks.
Since it only scales tx throughput, it doesn't scale our ability to sync with the network. So even if it works how it is supposed to, it is basically worthless for scalability.

Here I have a chart of scaling strategies that actually accomplish scale the blockchain by reducing the bandwidth requirement for syncing: https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/sharding.md


Why it does not have faster finality.
In section 4.5 of the bitcoin ng paper, Emin explains the reasoning for why bitcoin ng has faster finality. If the block creator builds 2 contradictory sets of txs during his period of time, then his block reward can get destroyed.
This strategy for enforcing fast finality does not work, because the amount of money spent in a block can be bigger than the block reward. If the block creator can steal more than 1 block reward by rewriting some history, then he doesn't mind giving up his block reward for finding the block.
So Bitcoin NG does not have faster finality than normal bitcoin.

Why it has a weaker security model than bitcoin.
In section 5.1 of the bitcoin ng paper, Emin explains why the block creator is incentivized to include the txs from the previous block. If a block creator doesn't include some of the txs, the cost to him is a fraction of the value of the fees for those txs.
So that means that your tx will only get included in a block if 2 consecutive block creators both decide to not censor your tx.
This is a strictly weaker security model than bitcoin.
In bitcoin you only need a single block creator to decide to not censor you to get your tx included.


bitcoin covenants
========

Emin in July 2017

Emin's paper on bitcoin covenants https://fc16.ifca.ai/bitcoin/papers/MES16.pdf

Currently with bitcoin scripts, your contract state can only exist for a single UTXO, and once that UTXO is spent, the state in that contract disappears.

In Ethereum it is possible to build smart contracts that survive for extended periods of time, and can interact with multiple different transactions.

Bitcoin covenants is an attempt to add long-term contract state to bitcoin, that way we can make more ethereum-type smart contracts into bitcoin.

Covenants have some serious drawbacks against normal blockchain smart contract state.
A covenant is linked to a particular UTXO. Only that UTXO, and any UTXOs created from it are able to participate in the covenant.

So if a covenant application was popular, and 10k bitcoin accounts all wanted to participate in the new covenant app, that means we would need to store the entire smart contrat on every bitcoin full node 10k times.

Compare to ethereum, where each full node only needs to store the smart contract once.

Another issue is that with bitcoin covenants, is that every time you use a bitcoin covenant transaction, the entire smart contract needs to be duplicated on all the bitcoin full nodes.

Compare to ethereum, where each full node only needs to store the smart contract once, no matter how many times that smart contract is used.

Bitcoin covenants was invented about 1.5 years after Ethereum's launch, and it is a proposal for smart contracts that are exponentially less scalable than Ethereum smart contracts.

It is possible to reduce features from ethereum smart contracts without making so many sacrifices to scalability.
Re-writing the same contract to the blockchain hundreds of thousands of times is not a solution worth considering.

Avalanche consensus
========

Emin in July 2018

Avalanche consensus is for the altcoin Emin is planning to sell.

my review of avalanche: https://github.com/zack-bitcoin/amoveo/tree/master/docs/other_blockchains/avalanche.md

bloxroute
========

Emin in January 2019

https://bloxroute.com/documentation/

bloxroute is a project with the goal of making blockchains more scalable. They want blockchains to have thousands of on chain transactions per second.

bloxroute's strategy for scalability is to make a detailed map of all the full nodes in a network, and calculate the paths that data should flow through so that data can propagate as quickly as possible.

Bloxroute chops up the blocks into pieces like a torrent file. So we can simultaniously download parts of a single block from multiple peers (A feature that already exists in Cosmos blockchain).

Bloxroute does not achieve it's stated goal of making blockchains more scalable for the same reasons that bitcoin-ng failed to achieve the goal of scalability.
The bandwidth cost of syncing with the network, per tx, is not improved by bloxroute. So bloxroute does not increase scalability.

