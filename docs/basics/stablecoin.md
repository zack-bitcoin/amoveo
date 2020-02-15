WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//basics/stablecoin.md)

How to use Amoveo to make a stablecoin.


Example of a good oracle to make a stablecoin that stays the same value as BTC: the price of BTC in VEO from 0-200

Example of a bad oracle that will not make a stablecoin of BTC: the price of VEO in BTC from 0-0.1


Amoveo contracts are natively measured in VEO, so if we want to make an asset that stays the same value as BTC, we need to the oracle to calculate the value BTC/VEO.

Unit analysis:
(BTC / VEO) * VEO = BTC.