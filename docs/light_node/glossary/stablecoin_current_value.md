Stablecoin Current Value
===========

in otc_derivatives, if you want to make a stablecoin contracts, you are asked to prove the "current value".

For example, if the oracle is asking "What is the price of BTC in VEO?", and it wont expire for 1 month, then we cannot predict what the oracle will resolve to in 1 month.
But we do know that the current price of BTC is in VEO. This current price is the "current value" that you need to provide in the contract.

For example, if BTC is worth $4000, and VEO is worth $80, then the current value would be 50.