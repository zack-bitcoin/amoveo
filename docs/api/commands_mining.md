#### Mining

[There is better mining software available here](https://github.com/zack-bitcoin/amoveo-c-miner)

The mining software integrated into the full node is useful for testing, and as a reference implementation. The full node is not a cost effective miner.


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

to see your list of peers
```
peers:all().
```

to add yourself to the list of peers, if you have ip address 1.2.3.4
```
peers:add({{1,2,3,4}, 8080}).
```