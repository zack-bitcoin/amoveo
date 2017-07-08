sys.config notes

The sys.config file in this directory is the main sys.config. The Makefile starts with this sys.config, and modifies it slightly to produce all the different sys.configs for the different nodes.
So if you want to edit a sys.config, only edit this one. All the other ones will be re-generated, and your edits will be lost.