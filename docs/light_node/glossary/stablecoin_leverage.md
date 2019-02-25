Stablecoin Leverage
==============

When you are making a stablecoin contract, you are asked to provide a value called "leverage".
This is a number that must be greater than 1.

For example, if I buy a stablecoin contract connected to BTC with a leverage of 1, and the price of BTC increases by 10% over the lifetime of my contract, then my side of the contract will be worth 10% more by the end.

If I buy a stablecoin contract connected to BTC with a leverage of 3, and the price of BTC increases by 10% over the lifetime of the contract, then my side of the contract will be worth 30% more by the end.


Leverage and margins are deeply connected.
Margins are the prices beyond which only one side controls all the veo in the contract.
If you increase the leverage, the margins must necessarily become tighter. 