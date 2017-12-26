Channel State
========

Channels are a powerful tool for blockchains that enable scalability. This document explores the different types of data that are enabled by channels, and introduces some vocabulary to make these things easier to talk about.

[here is an explanation of the timeline execution of channels.](channels.md)

## blockchain consensus state

Often called "consensus state".

Consensus state is the data that every full node of the blockchain keeps an identical copy of.

The consensus state of a blockchain is the current blockchain protocol after syncing your node with the network. 

Examples of data that is a part of the consensus state of a blockchain:
* pubkeys for every account.
* balances of every account.

In Amoveo the consensus state is [stored in merkle trees](trees.md)

Channels each have a little data recorded into the consensus state:
* pubkeys of each participant
* balance of each participant
* and more


## channel contracts


Both participants in the channel have signed the channel contract, it is under 2 of 2 consensus.
A channel's contract is data with the potential to become a transaction that will modify the blockchain consensus state.
A channel contract is an agreement between the two participants on how they will behave.
If one of them breaks the rules, then the other can post the channel contract to the blockchain as a transaction, and the blockchain will be a mediator.
It is expensive to have the blockchain enforce the channel contract.
The 2 participants are incentivized to follow the rules of the contract, that way they wont waste money paying the blockchain to enforce rules.

[In Amoveo the contract is turing complete](programmable_state_channels.md)


## channel evidence


Channel evidence is extra information provided to the channel contract. For example, there is a channel between Alice and Bob. The channel contract gives all the money to Alice by default, but it gives all to Bob if he can get Carol to sign something.

In this example, Carol's signature would be evidence.

Unlike the channel contract, channel evidence doesn't have to be signed.
So Bob can provide evidence, even if Alice doesn't want him to.

