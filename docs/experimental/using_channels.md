making a channel with the server:

```
easy:new_channel(Balance, ReceivingLimit).
```

Balance is how much of your money you put into the channel.
ReceivingLimit is how much money the server puts into the channel.
This is the maximum amount of money that can be sent to you until the channel runs out of space. ReceivingLimit needs to be bigger than Balance, or the server wont let you make the channel.
Fee is the transaction fee, so that this transaction will be included into a block soon.


checking your balance in the channel:
```
easy:channel_balance().
```


gambling with the server:
```
easy:dice(Amount).
```


When you want to close the channel and get your money out:
```
easy:close_channel().
```
You  need to sync with the network to see if your channel is closed.
```
easy:sync().
```

If your channel partner disappears, or breaks, you can still get your money without his help. Start with a solo-close transaction, then wait over 100 blocks, then do a channel timeout transaction
```
easy:solo_close_channel().
```
```
easy:channel_timeout().
```