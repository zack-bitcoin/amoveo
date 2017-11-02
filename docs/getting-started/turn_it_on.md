This is how to launch a node and connect to the network.

[first, if you haven't installed the dependencies, do that](/docs/getting-started/dependencies.md)

compile it
```
  make prod-build
```
start the node
```
  make prod-go
```
[You can access the running node from a browser](http://localhost:8081/main.html)

You can communicate with the running node from a terminal like this:
```
  make prod-attach
```
now that you are attached to a node, you can tell it [commands](/docs/api/commands.md)

You can turn off a running node
```
  make prod-stop
```
You can delete the database to restart from the genesis block. This preserves your keys.
```
  make prod-clean
```

For convenience, there are a couple more ways to start it. To kill any running processes, delete the database, rebuild the code, and start a node that can connect to the network, do this:
```
  make prod-quick
```

Additionally, there is this command to kill all running nodes, and then start a node that can connect to the network:
```
  make prod-restart
```
This keeps the old database around, so you don't have to re-sync any blocks.