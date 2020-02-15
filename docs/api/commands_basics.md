WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//api/commands_basics.md)

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

To see the transactions in the tx pool on this node:
```
tx_pool:get().
```
These are txs that could be included in the next block.

#### Stop a node
```
api:off().
halt().
```

#### delete data files to restart at block 0
```
make prod-clean
```

#### sign a transaction
```
keys:sign(Tx).
```

#### sign binary data
```
keys:raw_sign(Tx).
```

#### share new txs with the network
```
amoveo_utils:push_txs().
```
