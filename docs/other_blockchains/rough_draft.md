Polkadot Review
==========

Polkadot's white paper: https://github.com/w3f/polkadot-white-paper/

Polkadot is a plan to build a blockchain.
This project has very high ambitions, they intend to:

* solve the proof-of-stake problem.
* solve the on-chain governance problem.
* solve the side-chain problem. They call it "parachains".

Appendix A
=========

Looking at Polkadot's 20-page white paper, skip straight to page 19, to Appendix A.

Here you can see a list of problems that the Polkadot teams admits they don't yet have a plan on how to solve, and will need to be solved before they can deliver on Polkadot.

Importantly, all 3 of the highly ambitious goals that polkadot is supposed to solve, these 3 things are all listed in Appendix A as things they don't have any plan on how to solve.

They list:

* "consensus mechanism" and "proof-of-stake chain".
* "governance".
* "parachain implementation" and "relay chain".

As long as they still have no plan on how to solve these problems, there is nothing for me to review.

So this paper will mostly be focused on providing evidence that the goals of Polkadot are impossible, rather than reviewing any actual mechanism.

Philosophy: A Top-Down Design
==========

An effective blockchain mechanism designer starts with tools that are known to work, and attempts combining them in new ways to discover what can possibly exist. He bends his goals to the demands of nature.

An ineffective mechanism designer starts by imagining exactly what he wishes could exist, and then attempts to build all the parts. He bends nature to the demands of his goals.

Polkadot is designed using the ineffective strategy.

Proof of Stake
==========

Quote from the white paper:

```
The mechanics of the underlying BFT consensus al-
gorithm is out of scope for the present work. We will
instead describe it using a primitive which assumes a
consensus-creating state-machine.
```

Since they don't describe a PoS mechanism, there is nothing for me to review.

Instead I will link to a paper I have written that shows proof-of-stake is not a solvable problem https://github.com/zack-bitcoin/amoveo/blob/master/docs/other_blockchains/proof_of_stake.md


Governance
=========

Quote from the white paper:

```
To manage chain upgrades, Polkadot will inherently
support some sort of governance structure, likely based
on existing stable political systems and having a bicam-
eral aspect similar to the Yellow Paper Council [24]. As
the ultimate authority, the underlying stakable token hold-
ers would have “referendum” control. To reflect the users’
need for development but the developers’ need for legiti-
macy, we expect a reasonable direction would be to form
the two chambers from a “user” committee (made up of
bonded validators) and a “technical” committee made up
of major client developers and ecosystem players. The
body of token holders would maintain the ultimate legit-
imacy and form a supermajority to augment, reparam-
eterise, replace or dissolve this structure, something we
don’t doubt the eventual need for
```

Futarchy is one solution to the governance problem.
No one has found any exploits in futarchy governance, yet.

But, Polkadot's description of a referendum of coin holders, that would be a voting-type mechanism.
There are known exploits against voting type governance mechanisms. [market failure](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/market_failure.md) for example.

[it is never a good idea to use voting in blockchains.](https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/voting_in_blockchains.md)

Side Chains
========

The Polkadot goals for side chains involves re-using the work from the main-chain to secure all of the side chains. That way the entire system can increase in security together.

They call their main-chain the "relay chain", and the side-chains are called "parachains".
It is a goal of Polkadot that it should be trust-free to move value between the parachains and the relay chain.

Polkadot side-chains are partially planned out. For example, there are 4 kinds of participants:

* validator - has a safety deposit. Their approval is necessary to make a block valid. They run a full node of the relay chain. A random number generator is used to select which validator's approval is needed for each block.
* nominator - has a safety deposit. This is like delegated-proof-of-stake. A nominator is delegating their control over the consensus mechanism to a validator of their choice. If that validator is punished, then the nominator who delegated control to them is also punished.
* collator - they run a full node of one or more parachains. They provide potential blocks with zero-knowledge proofs of their validity to validators.
* fisherman - fraud proofs. Anyone can provide evidence that a validator has verified contradictory blocks, or that a validator has verified an invalid parachain block. This destroys the validator's security deposit.


This design is already vulnerable to soft-fork attacks against a parachain.

A soft-fork attack is where an attacker is able to trick the network into adding some new censorship rules for how to determine which txs are valid. Soft-fork attacks do not break any existing rule, they are merely making some existing valid txs into invalid ones. A soft-fork attacks are a serious threat because they can change the consensus rules of a blockchain in any way. They can redistribute the money in any way.

Because of [market failure](https://github.com/zack-bitcoin/amoveo/blob/master/docs/basics/market_failure.md) it is cheap to bribe validators to only validate parachain blocks which obey some additional censorship rules. Since it is a soft-fork, all these parachain blocks are valid. So the fisherman cannot produce any fraud proof to punish the validator for participating in a soft-fork attack.

Since it is cheap to steal the money in any parachain, this means polkadot does not work.


Here is my plan on how to build side-chains that could actually work: https://github.com/zack-bitcoin/amoveo/blob/master/docs/design/sortition_chains.md
In this paper I use logic to put bounds on how a side-chain can possibly work, for example, a side-chain that scales better than linearly in bandwidth would require probabilistic-value txs.

