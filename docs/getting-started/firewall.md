WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//getting-started/firewall.md)

EPMD gives a nice interface so you can run Amoveo programs in the background. This is important for running the integration tests.

Before enabling EPMD, make sure to block port 4369, and other ports used by erlang, otherwise strangers will be able to control your Amoveo node.

In Ubuntu, using root, issue these commands:
```
ufw default deny incoming
ufw default allow outgoing
ufw allow 22
ufw allow 8080
ufw enable
```

This will block everything from connecting your node, except for full node api on 8080, and ssh on 22 for administration of the server.

you can check the status of your firewall like this:
`ufw status`
or
`ufw status verbose`



A useful command to look at what your ports are currently doing:
`netstat -atu | grep "LISTEN"`



epmd should start automatically, and you can just leave it running. So you probably never have to use these commands:

If epmd is stuck running on a loopback interface, you may want to restart it so that your programs will run properly in the background again.

Now that the fire wall is working, you can turn on epmd (if you do it manually, it is recommended to start epmd as root.):
`epmd -daemon`

epmd will continue running even if Amoveo is turned off. To turn off epmd use
`epmd -kill`

to see if epmd is already running, and list the erlang processes it is connected to.
`epmd -names`


a link with more info
https://www.digitalocean.com/community/tutorials/how-to-set-up-a-firewall-with-ufw-on-ubuntu-14-04
