Decred
======

This is a review of the pow/pos hybrid consensus design used in Decred.

https://docs.decred.org/proof-of-stake/overview/ Looking at this page.
At the top of that page, they listed 5 reasons that they incorporate PoS elements into their blockchain.
1) Allowing stakeholders to vote for or against proposed changes to the Decred blockchain. This means that Decred is not secure, because it is cheap to bribe the voters to make an upgrade that would destroy Decred https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/voting_in_blockchains.md

2) Providing a mechanism for stakeholders to influence Proof-of-Work (PoW) miners. Stakeholders can vote to withhold a miner’s reward even if the block conforms to the consensus rules of the network.
This means that once a coalition takes control of the PoS side, their control is very stable. They can block anyone else from being paid mining rewards, so only their coalition can profitably mine.

Again, this is a voting mechanism. Which means it is cheap to bribe the voters to manipulate the vote to break the blockchain.

3) For a block to be valid, it has to be signed by at least 3 of the 5 tickets that are called to vote in that block. This makes the Decred blockchain more robust to certain kinds of attack, such as those which rely on secret mining.

This means that if you can bribe >about 40% of the stake, then you can freeze the blockchain.
I say "about" because there is a little freedom to re-mine blocks to get a new random selection of 5 ticket holders, and there is the fact that even if my coalition only has 35%, sometimes we will still control >2 of the 5 tickets for that block.
So it is hard to calculate the exact number, but I think it is pretty near to 40%.
Similarly, if you can bribe > about 60% of the stake, then you can take complete control of the consensus and pass any soft for upgrades that you want.

4) resistance to "contentious hard forks". PoW Miners are unable to build on a chain without the Votes of the tickets that are called.

If PoS voters can censor miners, then that means a coalition of the PoS voters could censor anyone outside the coalition from mining any blocks.
I don't know what "contentious hard fork" means, but I do know that a rule like this results in an insecure blockchain.

5) Snap voting of live tickets is used to make decisions about the project treasury. Again, voting cannot be secure. see (1) and (2).