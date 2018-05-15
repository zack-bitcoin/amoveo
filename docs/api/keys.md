The node keeps an encrypted copy of your private key.
You find your keys in this file:
`_build/prod/rel/amoveo_core/keys/keys.db`

The decrypted copy is only stored in RAM.

You can generate a new private key this way: (warning! this deletes your old private key!!!)
```
keys:new("password").
```

To secure your node so no one can sign transactions, you can either turn off the node, or you can do this command:
```
keys:lock().
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

To find out your pubkey in the internal binary format:
```
keys:pubkey().
```

To find your pubkey in the external base64 encoded format:
```
api:pubkey().
```

To calculate a shared_secret with a partner, you need a copy of their pubkey:
```
keys:shared_secret(Pubkey).
```


You can set the password for encryption like this:
```
keys:change_password("old_password", "new_password").
```
The default password on a new node is "", the empty string.

