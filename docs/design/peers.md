WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//design/peers.md)



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