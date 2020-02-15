WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//api/commands_advanced.md)


Sync-mode should handle itself automatically, so you will probably never need these commands:

WARNING syncing will go very slowly unless you use this:
```
sync_mode:quick().
```
This still gives the same security as a normal sync. It just turns off the tx pool, since you wont be processing txs while syncing.
You can check which mode you are in like this:
```
sync_mode:check().
```
Once you have finished syncing blocks and you are ready to process txs again, change back to normal mode like this:
```
sync_mode:normal().
```

This turns syncing on, so you will download blocks from your peers
```
sync:start().
```

This turns syncing off.
```
sync:stop().
```
