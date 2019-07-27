## The commands for updating.

first turn the node off.
Attach to it
```make prod-attach```

stop it from syncing to prevent corrupted state
```sync:stop().```

turn off the full node
```api:off().```

turn off the erlang terminal
```halt().```

update the dependencies.
```./rebar3 upgrade```

update Amoveo
```git pull```

if you want to use the new version of the config file, then remove old version of the config file.
```rm config/sys.config.tmpl```

if you need to delete the old blocks
```make prod-clean```

turn the node back on
```make prod-restart```

Tell it to re-sync rejected blocks with the new rules (only important if your node is frozen at a historical height because you didn't update in time for a hard update.)
```block_hashes:second_chance().```

the blocks are already synced, so switch to normal mode
```sync_mode:normal().```

if you want to make transactions, you need to unlock the keys:
```keys:unlock("").```

Now you can detach from the running node, and allow it to continue running in the background by holding the Control key, and pressing the D key.
