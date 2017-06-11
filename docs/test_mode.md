
first install normally:

```
git clone https://github.com/aeternity/testnet.git .
sh install.sh
```

Then make these changes:

* change src/free_constants.erl test_mode to true.
* change src/consensus/constants.erl master_pubkey to your pubkey. you can look up your pubkey with the command `keys:pubkey()`.
* remove peers:all from start.sh

start the node

`sh start.sh`

update your id to the master id

`keys:update_id(1)`


