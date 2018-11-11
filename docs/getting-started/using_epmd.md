EPMD gives a nice interface so you can run Amoveo programs in the background. This is important for running the integration tests.

Before enabling EPMD, make sure to block port 4369, and other ports used by erlang, otherwise strangers will be able to control your Amoveo node.

In Ubuntu, using root:
`ufw default deny incoming`
`ufw default allow outgoing`
`ufw allow 22`
`ufw allow 8080`
`ufw enable`

This will block everything from connecting your node, except for full node api on 8080, and ssh on 22 for administration of the server.

you can check the status of your firewall like this:
`ufw status`
or
`ufw status verbose`




epmd should start automatically, and you can just leave it running. So you probably never have to use these commands:

Now that the fire wall is working, you can turn on epmd:
`epmd -daemon`

epmd will continue running even if Amoveo is turned off. To turn off epmd use
`epmd -kill`

to see if epmd is already running:
`epmd -names`




