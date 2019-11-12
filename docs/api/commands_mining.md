#### Mining

[There is better mining software available here](https://github.com/zack-bitcoin/amoveo-c-miner)
[gpu mining software is here](https://github.com/Mandelhoff/AmoveoMinerGpuCuda)


to see your list of peers
```
peers:all().
```

to add yourself to the list of peers, if you have ip address 1.2.3.4
```
peers:add({{1,2,3,4}, 8080}).
```

#### see how fast blocks are being produced (in seconds).
```
block:period_estimate().
```

#### to see the total network hashrate (in GH/s).
```
block:hashrate_estimate().
```

#### to see the number of hashes on average to find a block.
```
block:hashes_per_block().
```

To mine a block:
```
api:mine_block().
```

<!----

The mining software integrated into the full node is useful for testing, and as a reference implementation. This is not a profitable miner.

After fresh install, one can start mining.

To start mining with all CPU cores: 
```
mine:start().
```

To stop mining:
```
mine:stop().
```

to check if you are currently mining:
```
mine:status().
```
----->
