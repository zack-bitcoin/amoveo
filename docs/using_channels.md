making a channel with the server:

```
api:new_channel(Balance, ReceivingLimit).
```

Balance is how much of your money you put into the channel.
ReceivingLimit is how much money the server puts into the channel.
This is the maximum amount of money that can be sent to you until the channel runs out of space. ReceivingLimit needs to be bigger than Balance, or the server wont let you make the channel.
Fee is the transaction fee, so that this transaction will be included into a block soon.


checking your balance in the channel:
```
api:channel_balance().
```


gambling with the server:
```
api:dice(Amount).
```


When you want to close the channel and get your money out:
```
api:close_channel().
```
You  need to sync with the network to see if your channel is closed.
```
api:sync().
```

If your channel partner disappears, or breaks, you can still get your money without his help. Start with a solo-close transaction, then wait over 100 blocks, then do a channel timeout transaction
```
api:solo_close_channel().
```
```
api:channel_timeout().
```
