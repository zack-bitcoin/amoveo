Decred
======

This is a review of the pow/pos hybrid consensus design used in Decred.

Here is a document that explores the capabilities of pow/pos hybrid designs in general: https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/pow_pos_hybrid.md

https://docs.decred.org/proof-of-stake/overview/ Looking at this page.
At the top of that page, they listed 5 reasons that they incorporate PoS elements into their blockchain. Some of these reasons show flaws in the design.

1) Allowing stakeholders to vote for or against proposed changes to the Decred blockchain. This means that Decred is not secure, because it is cheap to bribe the voters to make an upgrade that would destroy Decred https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/voting_in_blockchains.md

2) Providing a mechanism for stakeholders to influence Proof-of-Work (PoW) miners. Stakeholders can vote to withhold a miner’s reward even if the block conforms to the consensus rules of the network.
This means that once a coalition takes control of the PoS side, their control is very stable. They can block anyone else from being paid mining rewards, so only their coalition can profitably mine.

Again, this is a voting mechanism. Which means it is cheap to bribe the voters to manipulate the vote to break the blockchain.


5) Snap voting of live tickets is used to make decisions about the project treasury. Again, voting cannot be secure. see (1) and (2).



A major problem with decred is that no one knows what decred's fork choice rule is.

Failure to know what their fork choice rule is means that Decred is using different security models to explain why they are secure from different kinds of attacks. This is an invalid strategy in cryptoeconomics.
https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/security_model.md