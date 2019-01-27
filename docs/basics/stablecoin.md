How to use Amoveo to make a stablecoin.


Example of a good oracle to make a stablecoin that stays the same value as BTC: the price of BTC in VEO from 0-200 (currently 35)

Example of a bad oracle that will not make a stablecoin of BTC: the price of VEO in BTC from 0-0.1


Amoveo contracts are natively measured in VEO, so if we want to make an asset that stays the same value as BTC, we need to the oracle to calculate the value BTC/VEO.

(BTC / VEO) * VEO = BTC.