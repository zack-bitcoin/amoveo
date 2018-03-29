When you launch a full node, at first you don't have any aeons.

Your keys are automatically generated for you, and secured with the password "". The empty string is the default password.

The password is used to encrypt the private key on your computer.

If you want to change the password and keep your old keys, do this:
```
keys:change_password("Old_PASSWORD", "New Password").
```

replace "Old_PASSWORD" with the password you are replacing.
replace "New Password" with the new password you want.


Before using your node, make sure your wallet is unlocked. When a wallet is unlocked, that means that a decrypted copy of your private key is stored in ram.

```
keys:unlock("DONT_USE_THIS_PASSWORD").
```

One way to get VEO is to share your pubkey with someone who has already VEO, so that they send some to you.


To spend money to someone who doesn't yet have an account, you use the create_account transaction:
``` 
api:create_account(Pubkey, AmountOfMoney).
```
You can also create a new account by mining. If you don't have an account, and you find a block, it creates an account for you.


It is also possible to generate new keys. This deletes your old keys. Without your old keys, any money you had in your old account becomes inaccessible to you. Be very careful. DANGER DANGER

To generate an address with a better password, type following command: WARNING!! >>> this will delete your old private key <<< WARNING!!

```
keys:new("DONT_USE_THIS_PASSWORD").
```

Make sure to have replaced "DONT_USE_THIS_PASSWORD" with a better password.
