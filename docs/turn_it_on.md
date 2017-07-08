This is how to launch a node and connect to the network.

First compile it
```
  make release-build
```
start the node
```
  make release-start
```
You can communicate with the running node like this:
```
  make release-attach
```
now that you are attached to a node, you can tell it [commands](/commands.md)

You can turn off a running node
```
  make release-end
```
You can delete the database to restart from the genesis block. This preserves your keys.
```
  make release-clean
```
