When you launch a full node, at first you don't have any aeons.

To generate an address, type following command:

```keys:new("DONT_USE_THIS_PASSWORD").
```

Make sure to have replaced "DONT_USE_THIS_PASSWORD" with a better password.
This password is used to encrypt the private key on your computer.

Before using your node, make sure your _account_ is unlocked:

```keys:unlock("DONT_USE_THIS_PASSWORD").
```


One way to get aeons is to share your _address_ with someone who has already aeons, so that they send some to you.


Generate your address:
```testnet_sign:pubkey2address(keys:pubkey()).
```

To spend money to a brand new _account_, one needs to make a transaction, sign it, and then publish it:
``` {Tx, _} = create_account_tx:make(Address, AmountOfMoney, Fee, keys:id(), NewID, Accounts).
    Stx = keys:sign(Ctx, Accounts).
    tx_pool_feeder:absorb(Stx).
```
