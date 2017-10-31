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
