To run the tests, you need to make a couple small changes.

free_constants:test_mode() needs to be set to true.

in the config file, master_pubkey needs to be the same as the pubkey stored in your wallet. you can find your pubkey like this `keys:pubkey()`

the id stored in your wallet needs to be a 1.

you can look up your id like this `keys:id()`.

you can change your id like this `keys:update_id(1)`