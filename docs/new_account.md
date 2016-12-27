When you launch a full node, at first you don't have any aeons.

Your address is automatically generated for you, and secured with the password "". The empty string is the default password.

To generate an address with a better password, type following command:

```
keys:new("DONT_USE_THIS_PASSWORD").
```

Make sure to have replaced "DONT_USE_THIS_PASSWORD" with a better password.
This password is used to encrypt the private key on your computer.

If you want to change the password, do this:
```
keys:change_password("Old_PASSWORD", "New Password").
```

replace "Old_PASSWORD" with the password you are replacing.
replace "New Password" with the new password you want.


Before using your node, make sure your _account_ is unlocked:

```
keys:unlock("DONT_USE_THIS_PASSWORD").
```


One way to get aeons is to share your _address_ with someone who has already aeons, so that they send some to you.


Generate your address:
```
keys:address().
```

To spend money to a brand new _account_, one needs to make a transaction, sign it, and then publish it:
``` 
{Tx, _} = easy:create_account(Address, AmountOfMoney, Fee, NewID).
Stx = keys:sign(Ctx, Accounts).
tx_pool_feeder:absorb(Stx).
```
