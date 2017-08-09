Other commands
=======


##### To see the transactions currently in the mempool, which will be included in the next block.
```erlang
api:mempool().
```

#### Lookup block number 5 in current block chain
```erlang
block:get_by_height(5).
```

#### Lookup block whose header has the specified hash
```erlang
block:get_by_hash(<<"The32BytesLongHashOfABlockHeader">>).
```

#### Lookup block number 5 in a specific block chain
The specified hash of the block header identifies a block.  Lookup the
block at height 5 in the chain that goes from such block to the
genesis block.
```erlang
{ok, Header} = headers:read(<<"The32BytesLongHashOfABlockHeader">>),
block:get_by_height(5, Header).
```

### Lookup the top block
```erlang
{ok, H} = api:height(),
block:get_by_height(H).
```
