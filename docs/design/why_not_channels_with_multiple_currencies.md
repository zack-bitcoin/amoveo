# channels with multiple currencies vs channels with derivatives.

Channels with multiple currencies means that pairs of people have payment channels, and each payment channel can have multiple types of coins in it. A channel allows you to move these coins back and forth between the pair of people however you want, as many times as you want, without recording any information to the blockchain. Projects like Raiden, Zen and Aeternity are doing this.

The on-chain memory requirement for a channel includes the balance of tokens controled by that channel. If a channel controls multiple types of tokens, you would need to include the balances of each type, on-chain. If you want to add or remove any of the types of tokens in the channel, you would need to do an on-chain transaction and wait for confirmations.

Channels with derivaties means that the system only holds one tye of currency, but you can make arbitrary contracts priced in the one type of currency to trade risk however you want. So you can own an asset that stays the same value as gold or bitcoin or whatever. Projects like Bitcoin Hivemind, Augur, Gnosis, and Amoveo are doing this.

The disadvantages with derivatives are:
* you have to keep resetting them on-chain every month or so as contracts expire.
* their are margins on every contract, so if the price moves too far you stop having the intended risk exposure.

The disadvantage with subcurrencies is that:
* you have to do an on-chain transaction every time you want to modify the quantity of any type of subcurrency the channel holds, and wait confirmations.
* the on-chain memory requirements of the channel grow linearly with the number of types of currencies held. Compare to derivatives where the on-chain memory requirement is constant regardless of the number of types of assets owned.

Doing everything on-chain would be a similar cost to using subcurrencies in channels.

Given the relative costs, it is clear that derivatives is the better strategy than subcurrencies.

discuss this on reddit https://www.reddit.com/r/Amoveo/comments/72cynt/putting_subcurrencies_into_channels_is_a_bad/