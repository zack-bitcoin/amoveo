This is how to launch a node and connect to the network.

To run a full node, you need at least 200 mb of ram available.
You need at least 2 gb of disk space,
and you need at least 100 kilobytes per seconds of bandwidth

Do not run Amoveo as root user.   

It is helpful to have port 8080 exposed to the internet, this will let you find out about blocks more quickly, so you would mine faster.
[Most other ports need to be blocked. Enable the fire wall for security](firewall.md)

[Install dependencies](/docs/getting-started/dependencies.md)

compile it, start the node, and attach:
```
make prod-restart
```

to detach from  a running node, and leave it running in the background, hold the Control key, and press the D key.

You can communicate with the running node from a terminal like this:
```
  make prod-attach
```

now that you are attached to a node, you can tell it [commands](/docs/api/commands.md)

[You can read about how to download the blocks and sync with the network here](/docs/getting-started/sync.md)

To run this software in the background, hold the control key, and click the D key.


You can turn off a running node.
First make sure you are attached to it, then do:
```
api:off().
```
to turn off Amoveo.
Then do:
```
halt().
```
to turn off erlang.

You can upgrade the security of your node. First you need to run the node at least once. Then you need to run the node off. Then run this script:
`sudo sh better_cookie.sh`

If you mess up and create a cookie before running your node at least once, then your node will get stuck. you can fix it like this:
`sudo rm -rf ~/.erlang.cookie`


You can delete the database to restart from the genesis block. This preserves your keys.
Make sure to turn your node off first before doing this.
```
  make prod-clean
```

For convenience, there is a way to wrap this all into one command. To kill any running processes, rebuild the code, and start a node that can connect to the network, do this:
```
  make prod-restart
```
prod-restart is used if you need to turn the node back on after updating.
If you did not update, it is much faster to do `make prod-go` and then `make prod-restart`
<!---
-->


[Once the node is turned on, and you are attached to it, then you can issue commands.](../api/commands.md)