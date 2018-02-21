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
