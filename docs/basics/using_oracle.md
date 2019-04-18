Using the Oracle
=========


binary oracle: `Trump won the 2020 election.`

scalar oracle: `P = the price of USD in VEO from 0 to 0.05 on January 1, 2019, at 5:00 AM GMT; return P * 1024 / 0.05`

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