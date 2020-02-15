WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//basics/full_node_with_slow_connection.md)

If you have slow internet connection, and you want to run a full node, you may find that it is difficult to sync. it often crashes.
In the config/sys.config.tmpl file, set download_blocks_many to 1, so you do fewer connections simultaniously. This should stop it from crashing so much.