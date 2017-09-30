# Programmable State Channels

A programmable state channel is a mechanism invented by Zack Hess that allows the blockchain to enforce a smart contract without storing any of the code that makes up the smart contract.

For example, if we play a card game enforced by smart contract, the blockchain wouldn't know that you played cards.
Still, the blockchain can enforce that the winner of the card game gets their money.

Contracts inside of programmable state channels are called "off-chain".
Contracts that store state on-chain are called "on-chain".


The advantages of off-chain contracts are similar to the advantages of channel payments over on-chain payments:

* The smart contract can be updated instantly. You don't have to wait for any confirmations.
* You don't have to pay a miner fee.
* The smart contract stays off-chain and private, your channel partner can't prove the channel state, even if they wanted to.


These are some common misconceptions about programmable state channels:

* "Smart contracts in channels can only have 2 participants." This is false. We can use hashlocking to connect multiple channels together, this way more than 2 people can participate in a smart contract. [This was invented for cross chain atomic swaps.](https://en.bitcoin.it/wiki/Atomic_cross-chain_trading)
* "channel smart contracts are vulnerable to front running or the free-option problem." This is false. [There is an off-chain mechanism that can be used to protect participants.](limit_order_in_channel.md)
* "State channels are useful even if they are not programmable". [This is false](state_channel_without_off_chain_market.md)
State channels need to be at least programmable enough to build off-chain markets, otherwise they are no more valuable than payment channels. This is why "State channel" should mean "programmable state channel", there is no reason to build non-programmable state channels.
* "State channels can be used to hold multiple currencies". [This is false](why_not_channels_with_multiple_currencies.md)
