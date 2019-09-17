Using the Oracle
=========


binary oracle: `Trump won the 2020 election.`

scalar oracle to make an asset that moves the same as USD: `P = the price of USD in VEO from 0 to 0.05 on January 1, 2019, at 5:00 AM GMT; return P * 1024 / 0.05`

When making a scalar oracle you have to choose a range of values that can be measured, this is because a scalar oracle is actually 10 binary oracles, each of which is providing one bit of the scalar value. So a scalar oracle can only measure integer values from 0 to 1023. You need to choose a good range to measure, that way the difference between 2 values that can be measured is small for the range of expected outputs.
For example, if the oracle is set to measure from 0 to 0.05, and the value measured is 0.00025, then that means the scalar is internally using the binary value 0000000101
Which is 20% different from the 2 adjacent values 0000000100 and 0000000110.
20% difference between 2 adjacent measurable values isnt so good, that means on average the oracle's output is being rounded by 5%, so the money will be divided 5% differently than had been intended.
So this is why it is important to choose a good range of values to measure when making an oracle.

A scalar oracle for stablecoins that stay the same value as Euros.
`P the price of Euro in VEO on date D from 0 to 0.04; return P*1023 / 0.04`


scalar oracle to make an asset that moves inversely to Amazon shares:
`P = the price of amazon in veo from 0 to 20 on January 1, 2019, at 4:00 AM GMT; return (20 - P) * 1024 / 20`

scalar oracle to predict the weather: `P = the temperature in Vancouver at noon on April 23, 2019 from -10 to 50 Celsius according to this website https://www.timeanddate.com/weather/canada/vancouver/historic ; return (P+10) * 1024 / 60.`

If I am willing to pay 0.5 to go long, that means I will profit as long as the temperature is above 20 celcius. If I am willing to pay 0.2 to go short, that means I will profit as long as the temperature is below 4 celcius.

Scalar oracle with free-option protections:
`P1 = the price of USD in VEO on December 3, 2019, at 5:00 AM GMT?; P2 = the price of USD in VEO on January 1, 2019, at 5:00 AM GMT; return min(1023, 512 * (1 + ((P2 - P1) / P1)))`

scalar oracle with BTC hedge: `P = the price of BTC in VEO from 0 to 200 on January 1, 2019, at 5:00 AM GMT; if Trump wins the 2020 election return 0, otherwise return P;`


example, making a bet hedged against a currency:
let’s say you want to long some African stock trading in ZAR (South African currency), but you don’t want to have the veo price risk
so then what you basically do, is short veo and long the South African stock at the same time, using one oracle.
"What is the price of the stock X measured in VEO, from 0 to 50?"

so for example, say the stock goes up 10%, but veo drops 10% at the same time
this means that your return in veo is 1.1*1.1 -1 times the amount of veo you started betting with

or like 20% increase in VEO terms.

so your synthetic position exactly matched the return of the stock despite the price of VEO going down



scalar oracle to make an asset that moves inversely to amazon shares with frontrunning protections.

```
P0 = the price of amazon in veo on January 1, 2019, at 7:00 AM GMT;
P2 = P0 * 2;
P = the price of amazon in veo on Febuary 1, 2019, at 1:00 AM GMT;
Z = min(P2, max(0, P2 - P));
round(Z*1024 / P2)
```


A scalar oracle that moves inversely with BSV/USD

```
veo price source: Close price on https://coinpaprika.com/coin/veo-amoveo/#!historical-data
bsv price source; Close price on https://coinpaprika.com/coin/bsv-bitcoin-sv/#!historical-data
final date: Sep 23, 2019;
starting date: Sep 16, 2019;
fV = final number of usd per veo;
sV = starting number of usd per veo; 
fB = final number of usd per bsv; 
sB = starting number of usd per bsv; 
return 512*(2 - (fB/sB))/(fV/sV)
```
