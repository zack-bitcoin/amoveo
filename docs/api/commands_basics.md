Basic commands to use the blockchain
===========


#### Sync with the network
To sync with the network and download the blockchain: 
```
sync:start().
```

#### Current block height
To see the current block height downloaded to this node:
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
