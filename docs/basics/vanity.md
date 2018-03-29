To use the vanity address generator, there are 3 commands.

Turn it on:
```
vanity:start(<<"ALICE">>).
```
All the letters must be capatalized.

check if it is done:
```
vanity:check().
```
"go" means it is still working. when it is done it will display your new pubkey/privkey pair.

stop trying to find the vanity address.
```
vanity:stop().
```