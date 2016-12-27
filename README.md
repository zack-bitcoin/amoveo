æternity Testnet
==========

This is the testnet for the [æternity](www.aeternity.com) project.

This testnet uses simple PoW consensus. Its purpose is to show how æternity state-channels work and how this enables scalable trustless æpps.

### Compiling and Runing
You will need Erlang and a couple of libraries. Please follow instructions:
[For Ubuntu](docs/compile.md)

Then, start your node with following script:
```
sh start.sh
```

### Commands
#### Create an account
[Make an account](docs/new_account.md)

If you just want to start mining, skip this step, sync with the network and use the 'start mining' command below.

#### Sync with the network
To sync with the network and download the blockchain: 
```
easy:sync()
```

#### Start mining
To start mining with all CPU cores: 
```
easy:mine().
```

To stop mining: ```CTRL+C``` (shuts down the node, TODO improve)

#### Check your balance
```
easy:balance().
```

#### Stop a node
To stop a node run:
```
testnet_sup:stop()
```


### Else
If you want to know more, check out our whitepaper on [aeternity.com](https://aeternity.com) and get in touch with us via [Gitter Chat](https://gitter.com/aeternity?Lobby) or write us (emails in whitepaper). 
