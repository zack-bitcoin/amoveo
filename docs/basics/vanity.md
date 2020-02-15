WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//basics/vanity.md)

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