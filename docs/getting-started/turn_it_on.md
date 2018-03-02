This is how to launch a node and connect to the network.

To run a full node, you need at least 200 mb of ram available.

It is helpful to have port 8080 exposed to the internet, this will let you find out about blocks more quickly, so you would mine faster.


[first, if you haven't installed the dependencies, do that](/docs/getting-started/dependencies.md)

compile it
```
  make prod-build
```
start the node
```
  make prod-go
```

You can communicate with the running node from a terminal like this:
```
  make prod-attach
```
now that you are attached to a node, you can tell it [commands](/docs/api/commands.md)

[You can read about how to download the blocks and sync with the network here](/docs/getting-started/sync.md)


You can turn off a running node
```
  make prod-stop
```
You can delete the database to restart from the genesis block. This preserves your keys.
```
  make prod-clean
```

For convenience, there is a way to wrap this all into one command. To kill any running processes, rebuild the code, and start a node that can connect to the network, do this:
```
  make prod-restart
```


[Once the node is turned on, and you are attached to it, then you will want to sync the blocks](/docs/getting-started/sync.md)
