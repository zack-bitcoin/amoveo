Review of Ethereum Casper FFG
==========

I wrote a paper about why all PoS blockchains are vulnerable to soft fork bribery attacks https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md

In general, any attempt to recover from a soft fork bribery attack will have one of these shortcomings:
1) we undo the history that occured during the soft fork bribery attack, enabling the attacker to do double-spends between that version of history, and the new version.
2) we don't undo the history that occured during the soft fork bribery attack, so there was a period of time during which the soft fork attack was successful, and the attacker could have profited during that time.

In this blog post https://ethresear.ch/t/responding-to-51-attacks-in-casper-ffg/6363
Vitalik talks about Casper FFG and tries to explain why it is secure against this kind of attack.

Finality Reversion
==========

In the section of Vitalik's blog post titled "Finality Reversion", he explains why it is impossible to do a history rewrite attack, even if >50% of the validator stake is cooperating to attack.

Validator Censorship
==========

"validator censorship" is Vitalik's name for "soft fork bribery attacks".

In this section of Vitalik's paper, he attempts to explain why if >50% of stake decides to censor our txs in the blocks we build, that the remaining minority of stakers is consistently able to do a history re-write attack to undo the censorship attack.

The Contradiction
==========

It is not possible for both of these statements to be true simultaniously:

1) history rewrite attacks are infeasible, even if >50% of stakers are cooperating.
2) history rewrite attacks are consistently successful, even if <50% of stakers cooperate.

Any design that solves one of these, it will fail for the other.

Trying to gather evidence to justify minority soft forks.
==========

Vitalik tries to resolve the contradiction by giving a mechanism for detecting censorship attacks.

The idea is that it doesn't matter whether an honest staker pool is minority or majority, they just need a way to prove that all the other pools are committing censorship attacks.

The mechanism is like this: if some of us notice ourselves being censored, then more people should attempt to sign up as stakers to try and take control from the stakers who had gone rogue. If their transactions to sign up as stakers are not included quickly enough, then this is evidence that a censorship attack is occuring.

The problem with trying to have evidence of censorship attacks is that even if the censorship attack is real, that doesn't necessarily mean the history rewrite we are using to recover is 100% honest. It could have a double-spend attack embedded in it. It could be the case that the attacker is simultaniously doing a soft fork bribery attack to censor txs on-chain, and he is also doing a history re-write attack to do some double spending, and he can use evidence of the first attack to justify executing the second attack. So whichever side of the fork we go with, one of the attacks succeeds.

The False Flag Attack
=======

The false flag attack is where we bribe some of the validators to make it look like the rest of the validators are cheating. We combine this with propaganda on social media.
So the honest validators would all get punished by a community that thinks they cheated, and the attackers would be given 100% control of the validator set.

Conclusions
=======

Ethereum Casper FFG PoS, like all PoS consensus protocols, is vulnerable to bribery censorship attacks.

