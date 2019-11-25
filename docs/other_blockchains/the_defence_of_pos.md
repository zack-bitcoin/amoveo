The Defense of PoS
========

I wrote [the PoS paper](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md) as a general argument to show that PoS is not possible.

I shared the PoS paper with many people who have a vested interest in PoS technology, and I have received different responses from them. The goal of this paper is to collect the various arguments in favor of PoS together in one place, so we can see why different people believe PoS can still work.

If you feel that your opinion is misrepresented here, or if your opinion has changed, feel free to contact me and I will adjust this document for you.

Vitalik Buterin
=========

Vitalik Buterin's explanation is in-depth, I wrote an entire blog post responding to it here: https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/ethereum_casper_ffg.md

Jae Kwon
========

[link to twitter discussion](https://twitter.com/jaekwon/status/1163205417020735488?s=20)

Jae thinks that the security of PoS is based in "social legitimacy". 

But that is on a slower timescale in comparison to how many confirmations you would need to withdraw from an exchange.

If we attempt to use a slow mechanism to solve these descrepancies, then we would necessarily be opening ourselves up to the possibility of double-spend attacks.

Once the attacker has sold their coins in an exchange and withdrawn to fiat, then there is no way to roll back history and recover those coins.

If we can profitably do double spends this way, then we will do it over and over all day long forever to bleed the blockchain dry.

Eventually the attackers could make off with quite a lot of the value from the blockchain.
and every time the txs get rolled back, your money may have been deleted. it would be massively destructive.

Alfred
========

Alfred is someone who regularly contributes to Amoveo's telegram channel.

Alfred's argument is similar to Jae's, but Alfred also suggests that we add a rule so that the total amount of bonded stake is big enough so that if we delete all the bonds of everyone who took bribes, that it is impossible for the attackers to have profited.

To prevent rolling back from being profitable, we would need `(all money moved in one period of social consensus) * 2 < (value staked and online)`, that way we can delete enough stake from validators who were bribed so it is impossible that the money they stole is more valuable than the stake that was burned.

Staking value to someone who is online does not work, because that means we could cheaply bribe the delegate to allow your stake to get deleted, which means that the protocol is not secure.

It ends up like Augur's kind of security.
Costs come from paying enough fees to convince enough people to lock their money in bonds and stay online to verify blocks and to not let our money get stolen.

In augur each oracle is slow, so validators only come online irregularly. if we use it for consensus, then they need to stay online for every block.

To the best of my knowledge, no PoS blockchain is following the rules to use this security model. For example, this model would require us to limit the total number of coins that can be sent per block.

This model is vulnerable to parasite contracts, the same was as Augur.
If something like colored-coin is to bitcoin was built on top of this protocol, and they were not paying fees to the bonded validators proportional to the value transfered, then it could result in the protocol becoming insecure.

If there are subcurrencies, it could be very difficult to correctly calculate fees.

Maigoh91
========

Maigo is a community member from Cosmos, like Jae. [His explanation](https://www.reddit.com/r/cosmosnetwork/comments/cnw1rn/i_attempted_to_prove_that_cosmos_pos_consensus_is/) of why PoS is secure is based on reputation.

Maigo thinks that the value of a business's reputation is at risk, so they wont take bribes that could possibly destroy something so valuable.

But, it does not matter whether value takes the form of stake locked into the blockchain, or business reputation, or anything else. The [same game theory holds. the bribe is much smaller than how much value it can destroy](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/market_failure.md).

Anonymous Eth researcher who is designing Eth 2.0
=========

argument 1:

"soft forks can not change the fork choice rule"

It isn't clear how this assertion could help make a PoS secure against soft-fork-bribery attacks, but I can prove it false anyway.

If an attacker can redistriute funds however they want, then they can redistribute the funds to undo any costs that are internal to the system. So if the fork choice rule gives advantage to the side that pays more financial costs, a soft-fork can be set up to exactly counter those costs.

If the fork choice rule involves costs that are external to the system, that would be considered PoW, not PoS.

argument 2:

"a small number of non participants can make the attack expensive "

It doesn't make sense to call an attack "expensive", if it gives the attacker control to redistribute all the money, and all the costs are internal to the system.

The attacker can change the rules to make it not expensive. 

LionLikesCookies
========

A community member from Cardano who stepped up to explain why PoS works.

[The reddit thread of his explanation](https://www.reddit.com/r/CryptoCurrency/comments/cy4i6l/i_attempted_to_show_that_cardano_ouroboros_is_not/)

Lion's argument is that PoS is game-theoretically the same as PoW in bitcoin for this kind of attack. And that the fact bitcoin works is proof that PoS can also work.

This is basically the same as Vitalik's arguments in favor of PoS from January 2015 in his [P+epsilon paper](https://blog.ethereum.org/2015/01/28/p-epsilon-attack/) in the "Further Consequences" section.

I explain why this bribery attack can not be done against proof of work in [this paper](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md) in the section "Censorship can be good".

sebastiengllmt
========

A different Cardano community member explaining why PoS works.

[reddit thread of explanation](https://www.reddit.com/r/cardano/comments/cy52ab/i_attempted_to_use_math_to_show_that_cardanos/)

Sebast's argument is that it is impossible to know how much to bribe each validator, because we don't know the total number of people who have stake in the system.

The solution to that is that the attacker's smart contracts which are programmed to pay the bribe, all these smart contracts should be slowly offering a higher and higher bribe, until we have sufficient portion of the validator pool participating. This way the attacker only has to spend the minimum amount of money on the bribe as he needs to.

Josh
=======

Josh is a 3rd Cardano community member, and he gives an entirely different explanation for why PoS works.

According to Josh, "You’d have to bribe 51% of the value of the network to destroy 100% of the network"

And that does seem reasonable at first. [but game theorists have known since at least 1833 that this is not always true](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/market_failure.md)

Lets suppose Josh is right, and we will use a proof by contradiction to show that this can't be right.

A: Lets suppose that the (the cost to bribe a user) = (the value in Ada they own).

B: we know that: (cost of bribe) = (value held) * (probability that their vote is pivotal)

plugging A into B -> (value held) = (value held) * (probability that their vote is pivotal)

divide both sides by (value held) -> 1 = (probability that their vote is pivotal)

So we have shown that all of the user's votes are pivotal. So any individual user must have 100% control over the outcome of the vote.

But, if there is more than 1 user, this can't possibly be true.
If two of the users disagree on what they want the outcome, they can't both have 100% control over the outcome.

So we have shown that the initial premise of (the cost to bribe a user) = (the value in ada that they own), we have shown that his premise must be false, because it leads us to a contradiction.

petko
=======

petko is from NXT's community. NXT is one of the oldest PoS blockchains, so he has a financial interest to explain why PoS is possible.

[his tweet](https://twitter.com/petkodp/status/1168596724312662017?s=20)

petko's argument is that it is impossible to bribe someone without one of you having to trust the other to not run off with the money.

But, blockchain smart contract systems are exactly designed to enable trust-free contracts, including bribes.


[petko's second argument](https://twitter.com/petkodp/status/1168553666124664832?s=20)

This is the same as Josh's argument.


Lior Yaffe
========

Lior Yaffe is a programmer for lead dev of NXT, and Ardor, and Ignis.

Here is his argument for why bribery soft fork attacks cannot happen https://twitter.com/lioryaffe/status/1192320643511017473?s=20

According to Lior, this attack is "absurd".
And it can never happen in practice.
He gives the example of "NoS" attacks to clarify his position on the matter.

I assume by "NoS", he is talking about the nothing-at-stake-attack, a topic that was settled in November 2014 https://blog.ethereum.org/2014/11/25/proof-stake-learned-love-weak-subjectivity/
It makes me think that Lior hasn't paid attention to research in PoS since at least 2014. Which I guess makes sense, since NXT was launched in 2013, and is based on our understanding of PoS in 2013.

I don't understand what argument Lior is trying to make. It isn't clear if Lior has read the paper he is commenting on. I suspect that Lior just wants to virtue signal on PoS, he isn't actually interested in learning anything or teaching us anything.


Veil
=========

Veil's argument for why bribery-soft-fork attacks can't break their system is like this: We can use privacy so it is impossible to know who is the validator that you would want to bribe.


Ring signatures work well if the users want to stay private. 

The problem with bribes is that block producers are motivated to purposefully expose data to break privacy, because this is the only way for them to receive bribes.
You might think that it is possible to invent a complicated cryptography scheme so that it is not possible to reveal how you have voted, even if you would like to. 
But I think I have a pretty good argument that such a scheme is not possible.

one desired property of a conensus mechanism, is that it should not freeze just because one of the participants goes offline.
So that means the block creator needs to be able to calculate the next consensus state whether or not I am participating in creating it.
That means they can re-calculate the next consensus state using any subset of the txs of a block.

By comparing the results of including various subsets of the available txs, they are eventually able to derive how each individual txs is influencing the consensus state

An attacker who has access to all of the private keys and entropy used to generate all of the encrypted votes. It must be possible for him to re-trace the steps of the production of each encrypted vote.

Maximilian Roszko
==========

He chatted with me in this telegram group https://t.me/cryptocodereviews

His first argument was that it would be difficult to coordinate the bribe.

But that is a solvable UX problem.

We have to assume that people would prefer owning more value instead of less.
Calculating the expected profits of different possible user strategies, that is a math problem.
So calculating user behavior is a math problem.
Which means that security is a math problem.


His second argument:
"there is a very simple defence against bribery attacks, which is counter bribery attacks, where I bribe just a small amount more than you did, and make it so that we soft fork the protocol where all your funds go to me. Now I defended the network at no loss, in fact maybe I even made some money from it..."

Defenders can try to make a counter-bribe. So the system degrades to a whoever is willing to pay the most wins game. This is a failure mode.

If whoever is willing to pay the most has total control, and total control lets you print more coins and give them to yourself, then it is not clear the network would even be able to agree on a single version of history in a situation like this.

He made a third and fourth argument that were the same as petko's argument and Josh's argument.

Alfred
========

Alfred's argument is that if a soft-fork-bribery attack was to succeed, then we can use the slower social consensus layer to roll back the attacked blocks, and delete all the stake of everyone who had participated.


Emin Gun Sirer
========

Emin the founder of AvaLabs, which is launching the Ava cryptocurrency, which uses the Avalanche PoS consensus mechanism.

His response to the soft-fork-bribery attack proposal: "In practice, most validators, like most miners, are benign and follow the protocol as prescribed instead of playing clever games. Life is so much more fun when people attack, but the truth is 90+% of the participants are typically honest and simpleminded. "

Assuming that 90% are honest would make it trivial to write a secure PoS consensus mechanism.
Assuming 90% honesty in the stakers is not typical in cryptocurrency engineering.

Usually we model user motivations like this: 10% are attackers who are willing to take a loss to harm the protocol, 1% are altruists who are willing to take a loss to help the protocol, and 89% are self-interested who do whatever makes themselves the most money.

The costs due to confusion, that is a solvable UX problem.
A motivated attacker could create enough tooling to reduce the confusion costs to zero.

For example, an attacker could use a prediction market as a lie detector, that way we can have financial guarantees that certain facts about the software the attacker is offering us are true. Even if the stakers don't personally understand the software. If knowing the functionality of a peice of software is as easy as looking up the price of shares in a market, then you don't need technical expertise.

We have to assume that users prefer owning more value instead of less, which means that security is a calculation problem.


Charles Hoskinson
=======

Charles is a founder of Cardano, a blockchain that uses Ouroboros.

His response to the soft-fork-bribery attack proposal: "an attack that can't happen"

Conclusions
========

The response in favor of PoS is not consistent.
Many people agree that PoS is secure against bribery-soft-fork attacks, but they all have different reasons for why they believe this, and none of the reasons are convincing to me.

