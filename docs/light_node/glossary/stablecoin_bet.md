WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//light_node/glossary/stablecoin_bet.md)

Stablecoin Bet
===========

A stablecoin bet is a type of scalar bet. The difference is that the interface on otc_derivatives is optimized for making stablecoin type contracts.

A BTC stablecoin contract is an asset that stays the same value at BTC. If one participant in the channel has stablecoins, that means the other participant has extra exposure to VEO.

To use a stablecoin bet, it must be connected with a scalar type oracle, and the oracle must ask "what is the price of X in VEO." you cannot ask "what is the price of VEO in X", because that will not work for stablecoins.