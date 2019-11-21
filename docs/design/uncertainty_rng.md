Uncertain Expiration Random Number Generator
=============
draft #1

Thanks to Fernando Nieto for describing this mechanism.

In order to increase RNG security, we must smear out control of the entropy equally among many miners, and connect the revealing of entropy to receiving their block reward. That way there is a cost to manipulating the entropy being produced.


The [harmonic rng](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/harmonic_rng.md) is another plan to accomplish the same goal.  It works by giving each miner control of a fraction of a bit of entropy.

This document shows a RNG which works by removing our certainty over when the RNG process will terminate.

Uncertain Expiration RNG basics
=========

The entropy generation process on average takes some number of steps N, this is a configuration parameter.

Each block has a 1/N chance of being the final block of the entropy generation process.

The miner can't tell if a block they find is the final block. Every user who owns money in a probabilistic contract, they can look at the block hash to know if they have won.
If you own P portion of the mony in the probabilistic contract, then on each block there is a P/N chance that you have won all the money in that probabilistic contract.

After a miner creates a block that allows you to win, you wait a few more blocks before claiming your winnings. That way no one can undo the block that allows you to win.


How to attack it
==========

The way to attack the RNG is by owning both a portion of stake in a probabilistic contract as well as some portion of the hash power. Every time you mine a block, you check if that block would cause you to win the probabilistic contract. If not, you do not broadcast the block.

If you have P portion of the money in the probabilistic contract, then you only have to mine N/P many blocks this way to find one where you won the probabilistic contract.

So, if N*(block reward)/P < (amount of money in the probabilistic contract), then it is possible that this attack is profitable.

So a sortition chain can only have as much money in it as the value of all the block rewards mined during the uncertain period.

If there are M sortition chains in their uncertain period simultaniously, then the attacker can attack all M at once to increase the effectiveness of the attack. So it is necessary that all the different sortition chains existing at the same time, they must all expire on the same block height together.


Comparison with Harmonic RNG
========

Much like Harmonic RNG, if you are willing to spend N blocks generating entropy, then the entropy can be as secure as N block rewards. We can only generate one random number at a time.

In Uncertainty RNG, an attacker is losing many more block rewards, which makes this attack less profitable and distributes more of the proceeds of this kind of attack to the other miners.
In Uncertainty RNG we don't have to vary the block reward, it can all happen at the smart contract level. This could result in a simpler implementation.
