# Programmable State Channels

A programmable state channel is a mechanism invented by Zack Hess that allows the blockchain to enforce a smart contract without storing any of the code that makes up the smart contract.

For example, if we play a card game enforced by smart contract, the blockchain wouldn't know that you played cards.
Still, the blockchain can enforce that the winner of the card game gets their money.

Contracts inside of programmable state channels are called "off-chain".
Contracts that store state on the blockchain are called "on-chain". When state is "on the blockchain", that means that every full node has to download and store the state, and every light node is able to verify a merkle proof of the state from headers.


The advantages of off-chain contracts are similar to the advantages of channel payments over on-chain payments:

* The smart contract can be updated instantly. You don't have to wait for any confirmations.
* You don't have to pay a miner fee.
* The smart contract stays off-chain and private, your channel partner can't prove the channel state, even if they wanted to.


These are some common misconceptions about programmable state channels:

* "Smart contracts in channels can only have 2 participants." This is false. We can use hashlocking to connect multiple channels together, this way more than 2 people can participate in a smart contract. [This was invented for cross chain atomic swaps.](https://en.bitcoin.it/wiki/Atomic_cross-chain_trading)
* "channel smart contracts are vulnerable to front running, or the free-option problem." This is false. [There is an off-chain mechanism that can be used to protect participants.](limit_order_in_channel.md)
* "State channels are useful even without off-chain markets". [This is false](state_channel_without_off_chain_market.md)
State channels need off-chain markets, otherwise they are no more valuable than payment channels.
Without markets, any smart contract inside of a state-channel will either be vulnerable to arbitrage, or front-running.
* "State channels can be used to hold multiple currencies". [This is possible, but it gets rid of most of the benefits of having a channel.](why_not_channels_with_multiple_currencies.md)
Any time you want to change the amount of any currency in a channel, you need to do an on-chain transaction. Having to do so much on-chain reduces the utility of channels.

[Discuss this on Reddit](https://www.reddit.com/r/Amoveo/comments/73hdf7/programmable_state_channels_explained/)

This is the VM for state channel smart contracts in Amoveo https://github.com/zack-bitcoin/chalang

It has a couple compilers.

You can see a market for derivatives being run completely off-chain in state channels here: http://159.89.87.58:8080/explorer.html
It is being used as futarchy to decide how to upgrade Amoveo.
It matches trades in single-price batches.

Almost all state channel smart contracts will need to be integrated into markets like these.
The only exceptions are perfectly symmetrical contracts.
If a contract isn’t symmetric, then you need a market to determine the price of participating in the contract.