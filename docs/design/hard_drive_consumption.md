Amoveo supports sharding and light nodes. So you can hold as little or as much of the merkle trees as you want.

Headers are each 143 bytes. every node needs to download the headers, but you don't have to store them.
You do need to store the most recent 20 headers, so you can recover from forks that are less than 20 long.
* 2860 bytes

Blocks are each 1 mb at the start, and governance can change their size limit. if you are a full node, or a shard node, then you need to download every block, but you dont' need to store them.

Because of sharding, the amount of consensus state that you choose to store can be arbitrarily small. If you set it to 0, then that makes you a light node.

So in total, the storage requirement for amoveo, like the memory that you have to keep around when you shut the node off so that it can turn back on correctly, is 2860 bytes.

For each account you choose to store in your shard, it costs 106 bytes for the account, plus (256 * log16(number of accounts in the system))

similarly for governance variables, channels, oracles, the cost of storage is about (256 * log16(number of this type of element in the system))

For each account you have the option to store the oracle bets associated to that account. It costs about 300 bytes for every outstanding oracle bet that the account has.

For every oracle you have the option to store the open orders sitting in that oracle. it costs about 377 bytes for every open order you choose to store.


TL;DR 2860 bytes.
