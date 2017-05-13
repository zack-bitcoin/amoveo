Aeternity
==========

A blockchain for trust-free markets in financial derivatives

This is being used by: [æternity](https://aeternity.com).

### Compiling and Runing
you will need Erlang and a couple of libraries. Please follow instructions:

[For Ubuntu](docs/compile_ubuntu.md)
[For Mac](docs/compile_mac.md)

Then, start your node with following script:
```
sh start.sh <port>
```

To download updates do this:
```
sh update.sh
```

### Commands

#### Sync with the network
To sync with the network and download the blockchain: 

Then, start your node with following script:
```
sh start.sh <port>
```
- port for:
- testnet = 8041
- node 1  = 3010
- node 2  = 3020

aso.

### Commands

#### Sync with the network
To sync with the network and download the blockchain: 
```
easy:sync().
```

#### Mining
After fresh install, one can start mining.

To start mining with all CPU cores: 
```
mine:start().
```
You will see something like this:

start mining with 4 cores.
ok

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
(does get done automatically when no accocunt and mining starts)
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

#### Keys
[You can read about how your private key is protected here](docs/keys.md)


### Else
If you want to know more, get in touch with us via [Slack](http://slack.aeternity.com)
