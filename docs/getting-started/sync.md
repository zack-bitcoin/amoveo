
You can see how many headers you node has:
`api:height()`
When you first turn on, it should automatically start downloading headers. After that point, you will only receive headers when your peers send them to you.

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
This will allow you to automatically download blocks, to process txs, to mine blocks, and automatically tell your peers when new blocks become available.

Your peers will automatically send new headers to you. Once you get the headers, then you will automatically try and download blocks for them.

To sync transactions with a peer that has ip 1.2.3.4 and port 8080:
```
api:txs({{1,2,3,4}, 8080}).
```

If you want it to stop syncing, you can use:
```sync:stop().```
it can be restarted with
```sync:start().```