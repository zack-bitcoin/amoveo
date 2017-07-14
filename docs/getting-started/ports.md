A node uses two ports.

The first one is public and exposes API for transactions .

The other one is private and exposes more functions that are supposed to leverege unlocked account that the node is running. Currently, we protect the private port by accepting requests only from localhost. You can lose money if you don't protect the internal port!

You can change default port numbers by changing values in Makefile or config file (depending at what stage you are configuring a node).