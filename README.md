Aeternity
==========

A blockchain for trust-free markets in financial derivatives

This is being used by: [æternity](https://aeternity.com).

### Compiling and Runing
you will need Erlang and a couple of libraries. Please follow instructions:

[For Ubuntu](docs/compile_ubuntu.md)

[For Mac](docs/compile_mac.md)


### Commands

[Read about the commands in depth in the docs](docs/commands.md)

#### Start the blockchain
Start your node with following script:
```
sh start.sh
```

#### Sync with the network
To sync with the network and download the blockchain: 
```
sync:start().
```

#### Mining
After fresh install, one can start mining.

To start mining with all CPU cores: 
```
mine:start().
```
To stop mining:
```
mine:stop().
```
to check if you are currently mining:
```
mine:is_on().
```

#### Spend
```
easy:spend(To, Amount).
```
To is the recipient's account ID

#### Last transactions
```
tx_pool:data().
```

#### Find out your account ID
```
keys:id().
```
If it returns something less than 1, that means you don't have an account yet.

#### Create an account
(does get done automatically when no account and mining starts)
[Make an account](docs/new_account.md)

#### Check your balance
```
easy:balance().
```

#### Stop a node
To stop a node run:
```
easy:off().
```


### Else
If you want to know more, get in touch with us via [gitter chat](https://github.com/zack-bitcoin/testnet)
