
You can see how many headers you node has:
`api:height()`
When you first turn on, it should automatically start downloading headers. After that point, you will only automatically receive headers if you have port 8080 open so that peers can send them to you.

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

If you also want to make txs, then you need to decrypt your private key:
```
keys:unlock("").
```

Your peers will automatically send new headers to you. Once you get the headers, then you will automatically try and download blocks for them.
If your server doesn't accept connections from the internet, then you wont receive headers, and you wont try to download blocks. You node will seem frozen at one height.

Amoveo shares headers by pushing them, this way we can find out about new blocks as quickly as possible.
Amoveo shares blocks by pulling them, that way we can minimize the volume of data transfered, so we don't waste effort.

If you can't accept messages from the internet, possibly because you are behind a firewall, you can still get your full node in sync by manually requesting headers from a peer, like this:
```
P1 = lists:nth(3, peers:all()).%grabs 3rd peer from you list of peers.
sync:get_headers(P1).
```



If you want it to stop syncing, you can use:
```sync:stop().```
it can be restarted with
```sync:start().```
