It should take less than 20 seconds to update your node.

First, if your node is on, turn it off.
```
api:off().
halt().
```

Then get the update
```
git pull
```

Turn it back on
```
make prod-restart
```

Switch to sync_mode normal so that you can process txs, and you will automatically stay synced with the blockchain, and so you share any blocks that you mine:
```
sync_mode:normal().
```

If you are mining, or making any sort of tx, then you will probably want to unlock you keys
```
keys:unlock("your password goes here").
```