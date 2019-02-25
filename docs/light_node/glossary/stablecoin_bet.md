Stablecoin Bet
===========

A stablecoin bet is a type of scalar bet. The difference is that the interface on otc_derivatives is optimized for making stablecoin type contracts.

A BTC stablecoin contract is an asset that stays the same value at BTC. If one participant in the channel has stablecoins, that means the other participant has extra exposure to VEO.

To use a stablecoin bet, it must be connected with a scalar type oracle, and the oracle must ask "what is the price of X in VEO." you cannot ask "what is the price of VEO in X", because that will not work for stablecoins.