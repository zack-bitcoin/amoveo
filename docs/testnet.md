* 17th May 2017 @zwilla

# How To?

How to? | COMMAND
------------ | -------------
Starting Testnet | `sh setup_test.sh`
Stop Testnet | `sh initd-aeterity.sh stop`
Access screen session 3010| `screen -r aeternity-3010`
Access screen session 3020| `screen -r aeternity-3020`
Access screen session 3030| `screen -r aeternity-3030`


# How it works!
The script 'setup_test.sh' will copy 3 times the testnet-folder into:
- aeternity-testnet3010
- aeternity-testnet3020
- aeternity-testnet3030
 
 * Than it will compile and clean the testnet.app and 
 * starts a screen session for every NODE!
 
 You can access the every screens session.
 * Node 3010 starts as mining node
 * Node 3020 and 3030 as full node
 
 You find this file here: https://github.com/Zwilla/aeternity-testnet/blob/master/docs/testnet.md
 
