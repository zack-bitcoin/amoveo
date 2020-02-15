WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//basics/backup.md)

Backup
=======

To save time with syncing, you might want to back up your block file so if you restart your node from a fresh state, you don't have to resync everything.

Save everything from /db

The steps to restore are:

1) clone Amoveo into a new directory

2) `make prod-restart` to compile and start amoveo

3) `api:off().` to turn off amoveo

4) `halt().` to turn off erlang

5) copy all your saved files into /db of your new amoveo installation.

6) `make prod-restart` to turn on amoveo again, it should be fully synced.