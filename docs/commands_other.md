Other commands
=======


##### To see the transactions currently in the mempool, which will be included in the next block.
```
easy:mempool().
```

#### Lookup block number 5
```
block:read_int(5).
```

### Lookup the top blocks
```
block:read_int(easy:height()).
```