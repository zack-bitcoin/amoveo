Cold Storage
========


Amoveo's javascript light wallet can be used for cold storage.

Generate the private key on the cold storage machine.

Use the pubkey to make a watch-only wallet on a machine that is connected to the internet. 
Generate unsigned transactions on the hot machine.
Make sure to sync all of the headers before generating the transactions.

Sign the transactions on the cold storage machine.

Use the light wallet on the machine that is connected to the internet to broadcast the signed transactions.
