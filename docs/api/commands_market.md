




```
api:new_market(OID, Expires, Period).
```

period is how many blocks you have to wait till bets get matched in a batch.
Expires is the height at which betting stops.



```
order_book:dump(OID).
```
This is how you remove a market once it is closed.
WARNING: make sure all the channels have closed their positions before closing the market.