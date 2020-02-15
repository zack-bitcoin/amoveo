WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//light_node/glossary/stablecoin_current_value.md)

Stablecoin Current Value
===========

in otc_derivatives, if you want to make a stablecoin contracts, you are asked to prove the "current value".

For example, if the oracle is asking "What is the price of BTC in VEO?", and it wont expire for 1 month, then we cannot predict what the oracle will resolve to in 1 month.
But we do know that the current price of BTC is in VEO. This current price is the "current value" that you need to provide in the contract.

For example, if BTC is worth $4000, and VEO is worth $80, then the current value would be 50.