Review of Ethereum Casper FFG
==========

I wrote a paper about why all PoS blockchains are vulnerable to soft fork bribery attacks https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md

In this blog post https://ethresear.ch/t/responding-to-51-attacks-in-casper-ffg/6363
Vitalik talks about Casper FFG and why it is secure against this kind of attack.

Finality Reversion
==========

In the section of Vitalik's blog post titled "Finality Reversion", he explains why it is impossible to do a history rewrite attack, even if >50% of the validator stake is cooperating to attack.

Validator Censorship
==========

In this section of Vitalik's paper, he explains why if >50% of stake decides to censor our txs in the blocks we build, that the remaining minority of stakers is consistently able to do a history re-write attack to undo the censorship attack.

The Contradiction
==========

It is not possible for both of these statements to be true simultaniously:

1) history rewrite attacks are infeasible, even if >50% of stakers are cooperating.
2) history rewrite attacks are consistently successful, even if <50% of stakers cooperate.

Evidence of censorship
==========

Vitalik tries to explain how we can get evidence that a censorship attack has occured, and use it to justify a minority history re-write attack to recover from the censorship attack.

The idea is that more people should attempt to sign up as stakers to try and take control from the stakers who had gone rogue. If their transactions to sign up as stakers are not included quickly enough, then this is evidence that a censorship attack is occuring.

The problem with trying to have evidence of censorship attacks is that any minority of stakers can fake being censored. They can build a minority history re-write in secret that includes txs the main chain does not know exist, then they can claim that the main chain is censoring those txs and should be invalid. People who are syncing after the attack has occured would not be able to distinguish between a good history rewrite to recover from censorship vs a bad history rewrite attack done to double-spend Eth.

Another problem with trying to have evidence of censorship attacks is that even if the censorship attack is real, that doesn't necessarily mean the history rewrite we are using to recover is 100% honest. It could have a double-spend attack embedded in it. It could be the case that the attacker is simultaniously doing a soft fork bribery attack to censor txs on-chain, and he is also doing a history re-write attack to do some double spending, and he can use evidence of the first attack to justify executing the second attack. So whichever side of the fork we go with, an attack succeeds.

In general, any recovery from a soft fork bribery attack will have one of these shortcomings:
1) some history gets un-done, enabling the attacker to do double-spends.
2) we don't undo the history that occured during the soft fork bribery attack, so there was a period of time during which the soft fork attack was successful, and the attacker could have profited during that time.

Social Layer Recovery
=========

In this blog post Vitalik also suggests that the social layer of consensus could be used to coordinate on a recovery from soft fork bribery attacks.

We have good models of using the social layer to recover, so lets find out if this claim could be true. I wrote about the social-layer-recovery model in this document: https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/bitcoin.md
I called it the "no-chain model" of blockchain security.

The historical example often used to talk about social layer recover is the DAO hack. After the hack, the stolen money was locked in one place for a long period of time. Given a long enough period of time, the ethereum community was able to talk together and come to a decision to prevent the attacker from taking the coins they had stolen from the DAO.

So the model of social layer recovery is simple.
If we have a long enough period of time to make the decision, and a large enough portion of the community has a vested interest in the outcome, then we are able to make decisions as a community without needing any consensus software.

Social layer recovery is not a valid solution for the soft-fork-bribery-attack, because the money is not locked in one place long enough for the community to make a decision. This attack is dealing with the normal spend txs that get included in every block, those spend txs immediately send the money from one account to another. There is no period of time during which the money is locked up. So we can't use the social layer recovery for this attack.


