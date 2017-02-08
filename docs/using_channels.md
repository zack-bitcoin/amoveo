making a channel with the server:

```
easy:new_channel(Balance, ReceivingLimit, Fee).
```

Balance is how much of your money you put into the channel.
ReceivingLimit is how much money the server puts into the channel.
This is the maximum amount of money that can be sent to you until the channel runs out of space. ReceivingLimit needs to be bigger than Balance, or the server wont let you make the channel.
Fee is the transaction fee, so that this transaction will be included into a block soon.


checking your balance in the channel:
```
easy:channel_balance()
```


gambling with the server:
```
easy:dice(Amount)
```