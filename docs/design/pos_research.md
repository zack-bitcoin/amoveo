Proof of Stake History
=========

Zack has been researching proof of stake blockchain designs since 2014.

This is the first PoS blockchains he wrote.
https://github.com/zack-bitcoin/slasher

It is based off of this paper from Vitalik: https://blog.ethereum.org/2014/01/15/slasher-a-punitive-proof-of-stake-algorithm/

The slasher type design was considered a big improvement over the coin-age type design, because slasher made it possible to solve the nothing-at-stake problem.

In late 2014/early 2015 Zack was researching bribery attacks in the context of oracles. He brought Paul Sztorc and Vitalik Buterin together to debate this topic. The results of the debate from Vitalik's perspective are here: https://blog.ethereum.org/2015/01/28/p-epsilon-attack/

In 2015 Zack was researching Secure Multi-Party Computation.
https://github.com/zack-bitcoin/secure-multi-party-computation
At that time, we had thought that making the choices of each individual participant in the protocol a secret would solve some bribery problems in PoS systems.
Eventually we discovered that this is not a valid way to prevent bribes, because a validator can always prove how they participated, and the ability to prove how you participated is all that is needed to enforce a trustless bribe.

Here I described some reasoning for why this is not possible https://github.com/zack-bitcoin/plasmodial/blob/master/docs/threshold_signature_anonimity.md

In late 2015, Zack applied the state of the art in PoS research to make a forum-on-a-blockchain system, where you can get paid rewards for sharing content that gets popular.
https://github.com/zack-bitcoin/forumcoin
Shortly after writing this code, the Steemit project was founded based on the same idea.

in 2015 and 2016, Zack's proof of stake research was happening in the FlyingFox blockchain
https://github.com/BumblebeeBat/FlyingFox

FlyingFox explores several different PoS designs.

He looked at incorporating proof-of-burn elements into the consensus.

He looked at using random sampling of coin holders, or of the people who have different kinds of security deposts.

He looked at various kinds of security deposits that validators can make, and using combinations of different kinds of security deposits simultaniously https://github.com/BumblebeeBat/FlyingFox/blob/master/docs/2_types_of_bonds.md

He looked at various kinds of sub-currencies that validators could own to help encourage consensus https://github.com/zack-bitcoin/plasmodial/blob/master/docs/shares.md

He reviewed the possibility of using hardware type oracles https://github.com/zack-bitcoin/plasmodial/blob/master/docs/town_crier.md

He discovered and descibed limitations in mechanism design related to voting https://github.com/zack-bitcoin/plasmodial/blob/master/docs/oracle_motivations.md

He looked at combining PoS validators with a betting mechanism to achieve faster finality https://github.com/BumblebeeBat/FlyingFox/blob/master/docs/consensus_by_bet_failure_mode.md  and https://github.com/BumblebeeBat/FlyingFox/blob/master/docs/faq_consensus.md

He looked at propaganda attacks https://github.com/BumblebeeBat/FlyingFox/blob/master/docs/delegated_pos_problem.md


in early 2017, proof of stake research was moved onto the Plasmodial blockchain
https://github.com/zack-bitcoin/plasmodial

In plasmodial Zack was exploring the possibility of combining the state channel mechanism with the proof-of-stake security deposit mechanism, as a way to try and make both cheaper.

Both channels and PoS involved security deposits, so the idea was that by reusing the same security deposit for both, we could provide more security at a lower cost.

Around April 2017, because of discussions with Jack Pettersson, Zack stopped being primarily a PoS researcher. He shifted his focus to specialize in oracles and state-channel technology while developing Amoveo https://github.com/zack-bitcoin/amoveo

October 2018 - Zack wrote a report about the current understanding of limitations of voting type mechanisms https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/voting_in_blockchains.md

August 2019 -
Zack is researching bribery attacks against various oracle mechanisms, when he makes some discoveries about bribery attacks that are imporant for PoS blockchain design https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/bribery.md

Zack creates a proof that PoS is worse than centralized alternatives: https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md

He applied this knowledge to show how the Cosmos blockchain can be attacked https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/cosmos.md

Zack creates a proof that pos/pow hybrid is worse than pow: https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/pow_pos_hybrid.md

He applied this knowledge to show how the Decred blockchain can be attacked: https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/decred.md


[A plan on how to actually do this attack](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/RCO.md)

[Why others think that PoS does work](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/the_defence_of_pos.md)

[Cardano](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/ouroboros.md)

[Algorand](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/algorand.md)

[Nyzo](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/nyzo.md)

[IOTA](https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/iota.md)
