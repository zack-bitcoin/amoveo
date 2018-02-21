Basic commands to use the blockchain
===========


#### Sync with the network
To sync with the network and download the blockchain: 
```
sync:start().
```

To pause syncing with the network
```
sync:stop().
```
Pausing can be helpful if you ran `sync:start().` more than once, and the multiple processes are interfering with each other.

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

To see the current block height on this node:
```
block:height().
```

To see the current header height on this node:
```
api:height().
```

#### Stop a node
```
api:off().
halt().
```

#### sign a transaction
```
keys:sign(Tx).
```

#### sign binary data
```
keys:raw_sign(Tx).
```
