Other commands
=======


##### To see the transactions currently in the mempool, which will be included in the next block.
```
api:mempool().
```

#### Lookup block number 5
```
block:get_by_height(5).
```

### Lookup the top blocks
```
block:get_by_height(api:height()).
```
