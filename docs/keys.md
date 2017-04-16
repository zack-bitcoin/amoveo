The node keeps an encrypted copy of your private key.
The decrypted copy is only stored in RAM.

You can generate a new private key this way: (warning! this deletes your old private key!!!)
```
keys:new("password").
```

To secure your node so no one can sign transactions, you can either turn off the node, or you can do this command:
```
keys:lock()
```

To unlock your node so that you can start signing transactions again, do this:
```
keys:unlock("password").
```

To check if you node is locked:
```
keys:status().
```

To manually sign a transaction:
```
keys:sign(Transactions, AccountRoot).
```

To manually sign raw binary data:
```
keys:raw_sign(<<"binary data">>).
```

To find out your id, pubkey, and address, there are these three commands
```
keys:id().
keys:pubkey().
keys:address().
```

To calculate a shared_secret with a partner, you need a copy of their pubkey:
```
keys:shared_secret(Pubkey).
```

To load a private key into an existing node:
```
keys:load(Pubkey, Privkey, "password", ID).
```


You can set the password for encryption like this:
```
keys:change_password("old_password", "new_password").
```