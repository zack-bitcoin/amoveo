


A binary market
=========
```
api:new_market(OID, Expires, Period).
```
period is how many blocks you have to wait till bets get matched in a batch.
Expires is the height at which betting stops.


A scalar market
==========
```
api:new_market(OID, Expires, Period, LowerLimit, UpperLimit, Many).
```

LowerLimit and UpperLimit are used for setting the margins and leverage for the smart contract. They are usually set to 0 and 1023.
Many is how many oracles are being used to generate the value, it is usually set to 10.


Closing a market
========
```
order_book:dump(OID).
```
WARNING: make sure all the channels have closed their positions before closing the market.