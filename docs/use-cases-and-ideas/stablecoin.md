WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//use-cases-and-ideas/stablecoin.md)

Stablecoins
#########

We make synthetic assets using portfolios of financial derivatives.
Everything is settled in Veo.
So different customers can set different margins depending on how much they are willing to pay, and how much volatility they want to be shielded from.


Basically, if you bet on the price of Veo measured in usd, you can bet in such a way that the asset you end up holding stays the same value as usd, as long as the price of Veo measured in usd stays inside the margins specified in the contract.

One-size-fits-all designs are bad. Different users of stablecoin contracts have different needs.
It is more affordable and usable for more people if the margins of each users contract can be customized.
It is also more scalable to keep all these contracts off-chain. That way they can be modified instantly, without putting anything on-chain.
