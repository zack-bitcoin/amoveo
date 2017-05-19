Terminal commands to control channels
===============

####Join the Aeternity channel network
Your balance divided by the receiving limit needs to be bigger than 1/2. Delay is how long at most you would have to wait to get your money out.
```
easy:new_channel_with_server(YourBalance, ReceivingLimit, Delay).
```

####Spend money through the lightning network
```
easy:lightning_spend(To, Amount).
```

####Learn a secret
So that you can receive a channel payment.
The code is like a ScriptPubKey in bitcoin, the Secret is like a ScriptSig in bitcoin. The code is the contract that was agreed to, the Secret is evidence to unlock the contract.
```
easy:add_secret(Code, Secret).
```

####sync channel state
Ask the server if your channel has been updated, and sync. If you received a channel payment for example, then your channel state will have been updated, or if your partern received a channel payment from you, then your channel state will be updated to reflect the new balances. This is how you sync with that new state.
```
easy:pull_channel_state().
```

####Donate money to the server.
```
easy:channel_spend(Amount).
```