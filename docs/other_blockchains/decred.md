Decred
======

This is a review of the pow/pos hybrid consensus design used in Decred.

This was the most painful review I have done.
People in the community were sharing false information about how it works.
Documentation includes expired data and is not searchable.
Devs would respond "dyor" to even the most basic questions about how Decred works.


Here is a document that explores the capabilities of pow/pos hybrid designs in general: https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/pow_pos_hybrid.md

https://docs.decred.org/proof-of-stake/overview/ Looking at this page.
At the top of that page, they listed 5 reasons that they incorporate PoS elements into their blockchain. Some of these reasons show flaws in the design.

1) Allowing stakeholders to vote for or against proposed changes to the Decred blockchain. This means that Decred is not secure, because it is cheap to bribe the voters to make an upgrade that would destroy Decred
https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/market_failure.md

In general, voting protocols just can't work on the blockchain:
https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/voting_in_blockchains.md

2) Providing a mechanism for stakeholders to influence Proof-of-Work (PoW) miners. Stakeholders can vote to withhold a miner’s reward even if the block conforms to the consensus rules of the network.
This means that once a coalition takes control of the PoS side, their control is very stable. They can block anyone else from being paid mining rewards, so only their coalition can profitably mine.

Again, this is a voting mechanism. Which means it is cheap to bribe the voters to manipulate the vote to break the blockchain.


5) Snap voting of live tickets is used to make decisions about the project treasury. Again, voting cannot be secure. see (1) and (2).


here is the memcoin 2 white paper, which decred was based on: https://decred.org/research/mackenzie2013.pdf


The fork choice rule from decred, from page 11 of the white paper: weight = sqrt(P * H * Q)
where Q is the number of ticket signatures on that block, either 3, 4 or 5.
P is the portion of coins locked as collaterol for PoS.
H is the hashpower.

considering a couple cases:

normal, 10% value is staked, hashpower = 1, Q=5
weight = sqrt(1 * 0.1 * 5) = 0.707

hashpower attack, 1% of value is staked, hashpower = 3, Q=1
weight = 0. the fork fails with Q<3.

bribe attack. 9% of value is staked, hashpower = 0.001.
weight = sqrt(0.09 * 0.001 * 5) = 0.0212

defenders of bribe attack. 1% of value left staked, hashpower = 1, Q=0
weight = 0. fork fails with Q<3.

A bribe attack does result in a very low weight, far lower than normal operations. But, since the attacker is able to prevent any blocks from being added to the defender's fork, the attack succeeds.

Decred is vulnerable to bribery attacks that take control of >50% of the tickets.
Once >50% control is established, then the coalition that controls PoS can censor any PoW blocks it doesn't like. They can keep causing the difficulty to go down, until they are able to find blocks at a normal pace without allowing anyone outside the coalition to find any blocks.

At that point the coalition is unstoppable.

