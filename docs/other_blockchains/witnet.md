Witnet Review
========

Witnet is a new blockchain specialized in oracles.
They use a proof of stake consensus mechanism for block creation.
This document will not be reviewing their consensus mechanism.
We will assume that their consensus mechanisms works, and just focus on the oracle.

Witnet is a copy of Bitcoin Hivemind's oracle, similar to Augur.
Bitcoin Hivemind is very big, and it is hard to fit it all into a blockchain while staying scalable.
Augur simplified Bitcoin Hivemind by getting rid of the SVD step, which multiple rounds of voting were combined to more accurately know who is lying.
Witnet simplifies Bitcoin Hivemind in 2 ways.
1) voting on the result of an oracle question is automated. Humans aren't involved.
2) It uses random sampling to select around 2-6 voters to determine the outcome of your oracle.

Witnet is targetting 3.2 level security, like Bitcoin Hivemind and Augur, but as we will see, Witnet is actually 4.2 level secure, it is not as secure as Augur or Bitcoin Hivemind.

In order to be comercially viable, a blockchain oracle system needs to be at least level 2.2 secure. Amoveo is the only blockchain oracle to achieve this so far.

False assumption in regards to voting
=========

Quote from the Witnet white paper:
"The effect is just the same if the entity pre-announces the vote for some false claim, promises a bribe to whoever votes for the same, such bribe is greater than the reward they get from telling the truth, and a majority of participants take the bribe. However, our protocol renders this kind of gambits completely useless by not giving participants the chance to reveal or prove the actual value of the claims they vote for. Even if a participant accepts a bribe, it can still tell the truth to the DON, lie to the briber and earn both the reward and the bribe."
Witnet is assuming that it is impossible to bribe the oracle voters, and they don't provide any mechanism to explain how this could be possible.

I provide a mechanism that can do bribes in witnet:
I make a smart contract on Amoveo with a witnet light node embedded inside of it, that way it can verify merkel proofs of any data on the witnet blockchain. I make a bet with a witnet oracle. The bet resolves in 1000 blocks, at that time, the oracle will have already revealed how they have voted. the bet says that if, at that point in time, they can make a merkel proof of the tx on witnet where they had revealed how they had voted, and if that vote is the way the attacker wanted it to be, then the money in the amoveo smart contract goes to the witnet oracle voter. Otherwise, the attacker gets his money back.
Amoveo smart contracts are for getting rid of trust. Paying bribes on witnet appears to be nothing more than a problem of trust.

I give you a general proof to show that it is impossible to prevent attackers from bribing voters:
1) It needs to be the case that miners can choose to include any subset of valid txs, otherwise the blockchain could get stopped by someone who refuses to publish a tx.
2) The miners need to be able to calculate the new consensus state after processing their block.
3) combining 1+2, it needs to be the case that miners can calculate the consensus state after processing any set of valid txs.
4) it needs to be the case that anyone is free to start mining.
5) combining 3+4, it is necessarily true that anyone can calculate the consensus state after including any set of valid txs in the next block.
6) because of (5), it must be the case that we can calculate what the state of the blockchain would have been, if we had not included one of the txs in it's history.
7) because of (6), it must always be possible to prove the impact that your tx had on the outcome of an oracle.
8) since it is possible to prove the impact your tx had on the oracle's outcome, it is possible to make that proof as evidence to a smart contract.
9) Since it is possible to prove to a smart contract that you participated in an attack, it is possible for the smart contract to release a bribe to you conditional on whether you participated in the attack.

This false assumption in regard to bribery led Witnet to make a mistake in their design.
Reputation voting based oracle systems like Bitcoin Hivemind, Augur, and UMA are all very cautious in regard to the possibility that the oracle voters will get bribed. They make sure that the value of the votecoins is bigger than the value of the bets, that way it isn't ever profitable to bribe the voters, and this is the difference between 3.2 and 4.2.

Witnet just assumes that bribery isn't possible. So they don't have any protections in place to make sure that the value of their reputation is high enough to maintain 3.2 level security.


Looks like a blog post was made specifically explaining the state of research in preventing bribery.
https://medium.com/witnet/anonymizing-commit-and-reveal-transactions-in-decentralized-oracle-solutions-e61067833cd9
Witnet has some different plans, but they haven't implemented this yet, and it looks to me like none of these plans actually solve the problem of bribery. They could be helpful to prevent DOS spam of oracle voters.

Remember, if there is ever a point in time in the future when the voters can prove how they voted, then that means it is possible to bribe them.
So any solution that prevents bribery, it needs to be impossible to prove how you voted, even if you want to.

In this blog post, they proposed solving this using: ring signatures, time-release encryption, zk-snarks, and dandelion networks.

Ring signatures only work if the participants want to be anonymous. It doesn't work to prevent bribes, because the voters are purposefully wanting to give up their anonimity to accept the bribe. They can purposefully reveal the signature pieces that were combined to make the ring signature.

time-release doesn't work because eventually there comes a time when we know the result of how the oracle participants voted, and at that time the bribes can be paid.

zk-snarks don't solve this. If you can make a zk-snark to prove how you participated for the vote, then you could make another zk-snark to prove how you voted to everyone else.

dandelion just anonimizes the IP-Pubkey relationships, it doesn't prevent bribery. so it doesn't solve this either.




The problem of trusting a single api.
===========

During one step of witnet, the 6 computers that you paid to act as an oracle, they will all look up an API at about the same moment in time, and provide this data to the blockchain in txs.
For example, if you are betting on a football game, the witnet voter nodes would all simultaniously look up the same api to find out the result of the game.
The problem with this, is that whoever is running the api where you can look up football game results, they could selectively send different data to different people who contact their server. They could purposefully send the wrong data to the witnet nodes, so that they will all provide the wrong data to the witnet blockchain, and in this way, the people running the football results api can steal from people who are using witnet to gamble on the result of the football game.

This is a second reason that Witnet is only 4.2 and not 3.2 like Bitcoin Hivemind.


Other Issues
======
Even if Witnet's oracle was working, I wouldn't have much confidence in their blockchain.
The oracle is just one part of a system for financial derivatives.
You also need scalable markets with guarantees that trades are matched in a fair sequence and at fair prices.
You need a scalable way to enforce large volumes of these contracts simultaniously.
