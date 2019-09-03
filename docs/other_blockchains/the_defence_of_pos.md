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


Conclusions
========

The response in favor of PoS is not consistent.
Many people agree that PoS is secure against bribery-soft-fork attacks, but they all have different reasons for why they believe this, and none of the reasons are convincing to me.