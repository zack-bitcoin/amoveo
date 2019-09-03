The Defense of PoS
========

I wrote [the PoS paper](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md) as a general argument to show that PoS is not possible.

I shared the PoS paper with many people who have a vested interest in PoS technology, and I have received different responses from them. The goal of this paper is to collect the various arguments in favor of PoS together in one place, so we can see why different people believe PoS can still work.


Jae Kwon
========

[link to twitter discussion](https://twitter.com/jaekwon/status/1163205417020735488?s=20)

Jae thinks that the security of PoS is based in "social legitimacy". 

But that is on a slower timescale in comparison to how many confirmations you would need to withdraw from an exchange.

If we attempt to use a slow mechanism to solve these descrepancies, then we would necessarily be opening ourselves up to the possibility of double-spend attacks.

Once the attacker has sold their coins in an exchange and withdrawn to fiat, then there is no way to roll back history and recover those coins.

Anonymous Eth research who is desiging Eth 2.0
=========

argument 1:

"soft forks can not change the fork choice rule"

This seems like the most hopeful argument of them all, but there aren't enough details to be sure if it could work.

The difference in the fork choice rule is basically what is protecting PoW from soft-fork-bribery attacks, so it makes sense that a similar trick could be useful for PoS.

But until someone actually provides such a fork choice rule, and demonstrates that it does work to prevent this attack, this isn't enough evidence to show that PoS can possibly be secure from soft-fork-bribery attacks.

argument 2:

"a small number of non participants can make the attack expensive "

It doesn't make sense to call an attack "expensive", if it gives the attacker control to redistribute all the money, and all the costs are internal to the system.

The attacker can change the rules to make it not expensive. 

Maigoh91
========

Maigo is a community member from Cosmos, like Jae. [His explanation](https://www.reddit.com/r/cosmosnetwork/comments/cnw1rn/i_attempted_to_prove_that_cosmos_pos_consensus_is/) of why PoS is secure is based on reputation.

Maigo thinks that the value of a business's reputation is at risk, so they wont take bribes that could possibly destroy something so valuable.

But, it does not matter whether value takes the form of stake locked into the blockchain, or business reputation, or anything else. The [same game theory holds. the bribe is much smaller than how much value it can destroy](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/market_failure.md).

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


petko
=======

petko is from NXT's community. NXT is one of the oldest PoS blockchains, so he has a financial interest to explain why PoS is possible.

[his tweet](https://twitter.com/petkodp/status/1168596724312662017?s=20)

petko's argument is that it is impossible to bribe someone without one of you having to trust the other to not run off with the money.

But, blockchain smart contract systems are exactly designed to enable trust-free contracts, including bribes.


[petko's second argument](https://twitter.com/petkodp/status/1168553666124664832?s=20)

His argument this time is basically like: No one will accept a bribe B to participate in an attack that could make them lose V, if B<V.
And that does seem reasonable at first. [but game theorists have known since at least 1833 that this is not always true](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/market_failure.md)


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


His second argument:
"there is a very simple defence against bribery attacks, which is counter bribery attacks, where I bribe just a small amount more than you did, and make it so that we soft fork the protocol where all your funds go to me. Now I defended the network at no loss, in fact maybe I even made some money from it..."

Defenders can try to make a counter-bribe. So the system degrades to a whoever is willing to pay the most wins game. This is a failure mode.

If whoever is willing to pay the most has total control, and total control lets you print more coins and give them to yourself, then it is not clear the network would even be able to agree on a single version of history in a situation like this.

He made a third and fourth argument that were the same as petko's arguments.

Conclusions
========

The response in favor of PoS is not consistent.
Many people agree that PoS is secure against bribery-soft-fork attacks, but they all have different reasons for why they believe this, and none of the reasons are convincing to me.

