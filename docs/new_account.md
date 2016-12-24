When you launch a full node, at first you don't have any money.

to generate an address, do this:
```keys:new("password_1337")```
substitute your password of choice for "password_1337"
This password is used to encrypt the private key on your computer.

Before using your node, make sure your account is unlocked, like this:
```keys:unlock("password_1337")```

remember to use the password you created instead of "password_1337"

One way to get money is to give your address to someone who has money, that way they can send it to you.
You can generate your address like this:
```testnet_sign:pubkey2address(keys:pubkey()).```

To spend money to a brand new account, you need to make a transaction, sign it, and then publish it. like this:
``` {Tx, _} = create_account_tx:make(Address, AmountOfMoney, Fee, keys:id(), NewID, Accounts),
    Stx = keys:sign(Ctx, Accounts),
    tx_pool_feeder:absorb(Stx) ```