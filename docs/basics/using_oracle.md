Using the Oracle
=========


binary oracle: `Trump won the 2020 election.`

scalar oracle to make an asset that moves the same as USD: `P = the price of USD in VEO from 0 to 0.05 on January 1, 2019, at 5:00 AM GMT; return P * 1024 / 0.05`

scalar oracle to make an asset that moves inversely to Amazon shares:
`P = the price of amazon in veo from 0 to 20 on January 1, 2019, at 4:00 AM GMT; return (20 - P) * 1024 / 20`

scalar oracle to predict the weather: `P = the temperature in Vancouver at noon on April 23, 2019 from -10 to 50 Celsius according to this website https://www.timeanddate.com/weather/canada/vancouver/historic ; return (P+10) * 1024 / 60.`

If I am willing to pay 0.5 to go long, that means I will profit as long as the temperature is above 20 celcius. If I am willing to pay 0.2 to go short, that means I will profit as long as the temperature is below 4 celcius.

Scalar oracle with free-option protections:
`P1 = the price of USD in VEO on December 3, 2019, at 5:00 AM GMT?; P2 = the price of USD in VEO on January 1, 2019, at 5:00 AM GMT; return min(1023, 512 * (1 + ((P2 - P1) / P1)))`

scalar oracle with BTC hedge: `P = the price of BTC in VEO from 0 to 200 on January 1, 2019, at 5:00 AM GMT; if Trump wins the 2020 election return 0, otherwise return P;`


example, making a bet hedged against a currency:
let’s say you want to long some African stock trading in ZAR (South African currency), but you don’t want to have the veo price risk
so then what you basically do, is short veo and long the South African stock at the same time, using one oracle
so what this means is that your payout in VEO is determined like this: (1-V)*(1+Z)-1
Where V is the return of veo over the period and Z is the return of the stock
so for example, say the stock goes up 10%, but veo drops 10% at the same time
this means that your return in veo is 1.1*1.1 -1 times the amount of veo you started betting with
or like 20% increase in VEO
so your synthetic position exactly matched the return of the stock despite the price of VEO going down
what this means is that the other side of the trade is both long veo and short the stock at the same time, so their payout looks like (1+V)*(1-Z)-1
you can bundle all of this into one scalar oracle



scalar oracle to make an asset that moves inversely to amazon shares with frontrunning protections.
```P0 = the price of amazon in veo on January 1, 2019, at 7:00 AM GMT;
P2 = P0 * 2;
P = the price of amazon in veo on Febuary 1, 2019, at 1:00 AM GMT;
Z = min(P2, max(0, P2 - P));
round(Z*1024 / P2)```

