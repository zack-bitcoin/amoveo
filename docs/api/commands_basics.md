Basic commands to use the blockchain
===========


#### check progress in syncing blocks

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

#### delete data files to restart at block 0, but preserve the blocks on your hard drive.
```
make prod-clean
```

#### delete blocks from your hard drive
```
make prod-blocks
```

#### use the blocks you already downloaded to resync the blockchain. this re-generates the data files that were deleted by 'make prod-clean'
Recovery should happen automatically.

#### sign a transaction
```
keys:sign(Tx).
```

#### sign binary data
```
keys:raw_sign(Tx).
```
