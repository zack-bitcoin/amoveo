
If you have cleaned your database with `make prod-clean`, your database still has the blocks. You can save bandwidth by re-syncing with the blocks from your database.
```
block_absorber:recover(full).%reads all the blocks.
block_absorber:recover(quick).%only reads 90% of blocks, but is faster.
```


You can see how many headers you node has:
`api:height()`
When you first turn on, it should automatically start downloading headers. After that point, you will only automatically receive headers if you have port 8080 open so that peers can send them to you.

You can manually download headers from a peer:
```
sync:get_headers({{1,2,3,4}, 8080}).
```

You can see a list of all your peers like this:
```
peers:all().
```
You can name a peer, and use it to get headers:
```
P1 = lists:nth(3, peers:all()).
sync:get_headers(P1).
```

You can only download a block if you already have the header for that block.

Once you have more than 0 headers, you can start downloading blocks:
`sync:start().`
Sometimes downloading freezes. you can start it again with the same command:
`sync:start().`

Once the node has synced all the blocks, it needs to be changed from quick-mode to normal-mode like this:
```
sync_mode:normal().
```
This will allow you to automatically download blocks for any headers you already have, to process txs, to mine blocks, and automatically tell your peers when new blocks become available.

Your peers will automatically send new headers to you. Once you get the headers, then you will automatically try and download blocks for them.

In order to get headers from peers, they need to know your IP address and port. You need to be added to the list of peers on their nodes.
You can see the list of peers stored on your machine like this:
```
peers:all().
```
You can add yourself to the list like this:
```
peers:add({{1,2,3,4}, 8080}).
```
You can share your list of peers to peer P1 like this: 
```
P1 = lists:nth(3, peers:all()).
sync:trade_peers(P1).
```


To sync transactions with a peer that has ip 1.2.3.4 and port 8080:
```
api:txs({{1,2,3,4}, 8080}).
```

If you want it to stop syncing, you can use:
```sync:stop().```
it can be restarted with
```sync:start().```
